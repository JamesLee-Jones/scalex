package lexer

import lexer.NFA.{combine, empty}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.quoted.*

extension (regex: String)
  inline def ->[T](inline token: T): RegexToToken[T] =
    RegexToToken(regex, _ => token)

  inline def ->[T](inline token: String => T): RegexToToken[T] =
    RegexToToken(regex, token)

case class RegexToToken[T](regex: String, token: String => T)

object Lexer {
  inline def apply[T, Err](
      inline errorBuilder: ErrorBuilder[Err],
      inline regexToTokens: RegexToToken[T]*
  ): Lexer[T, Err] =
    ${ applyImpl('{ regexToTokens }, '{ errorBuilder }) }

  inline def apply[T](
      inline regexToTokens: RegexToToken[T]*
  ): Lexer[T, LexerError] =
    ${ applyImpl('{ regexToTokens }, '{ DefaultErrorBuilder() }) }

  private def applyImpl[T, Err](
      regexToTokens: Expr[Seq[RegexToToken[T]]],
      errorBuilder: Expr[ErrorBuilder[Err]]
  )(using Type[T], Type[Err])(using ctx: Quotes): Expr[Lexer[T, Err]] = {
    import ctx.reflect.*
    // Parse the input regular expressions to an internal representation and error if they cannot be parsed.
    val parsed: Seq[(RegEx, Expr[String => T])] = regexToTokens match {
      case Varargs(args) =>
        args.map { case '{ RegexToToken($regex, $token) } =>
          // Lex each input regular expression
          val lexed = Scanner.scan(regex.valueOrAbort)
          // Parse each input regular expression, returning an informative compilation error on failure.
          Parser.parseRegex(lexed) match {
            case Right(value) => (value, token.asExprOf[String => T])
            case Left(error) =>
              report.error(error.format);
              (Emp, ???) // Never reached
          }
        }
    }

    // Convert each regular expression to an NFA
    val nfaToToken: Seq[(NFA, Expr[String => T])] = parsed.map {
      (regex, token) =>
        (regexToNfa(regex), token)
    }

    // Map NFA accepting states to resulting tokens.
    val nfaAcceptingStatesToToken: mutable.HashMap[Int, Expr[String => T]] =
      mutable.HashMap()
    nfaToToken.foreach((nfa, token) =>
      nfa.accept.foreach(state =>
        nfaAcceptingStatesToToken.addOne(state.id, token)
      )
    )

    // Combine the NFAs for each regex into one NFA.
    val nfas = nfaToToken.map((nfa, _) => nfa)
    val combinedNfa = combine(nfas)

    // Convert the NFA into a DFA.
    val (dfa, nfaToDfaAccept) = nfaToDfa(combinedNfa)

    val (minId, maxId) = dfa.nodeIdRange()
    val acceptingStates: Expr[Seq[Int]] = Expr(
      dfa.accept.toSeq.map(s => s.id - minId)
    )

    // Map DFA accepting states to resulting tokens.
    var dfaAcceptToToken: Seq[Expr[(Int, String => T)]] = Seq()
    nfaToDfaAccept.foreach((nfaId, dfaIds) =>
      dfaIds.foreach(dfaId =>
        dfaAcceptToToken = dfaAcceptToToken.appended('{
          (${ Expr(dfaId - minId) }, ${ nfaAcceptingStatesToToken(nfaId) })
        })
      )
    )

    // Produce a map from current state id and char to the next state.
    val transitionMap = mutable.HashMap[(Int, Char), Int]()
    dfa.traverse(
      (_, _) => (),
      (),
      (transition, map: mutable.HashMap[(Int, Char), Int]) => {
        if transition._2.nonEmpty then
          map += ((
            transition._1.id - minId,
            transition._2.get
          ) -> (transition._3.id - minId))
      },
      transitionMap
    )

    val transitionSeq: Expr[Seq[((Int, Char), Int)]] = Expr(transitionMap.toSeq)

    // Produce an implementation of a lexer for the current nfa token pairs.
    val result = '{
      new Lexer[T, Err] {
        private var string: String = ""
        private var pointer: Int = 0

        // TODO: This duplicates information in transitions. Factor out.
        private val validTransitions = mutable.HashMap[Int, Set[Char]]()
        ${ transitionSeq }.foreach { case ((state, char), _) =>
          validTransitions.updateWith(state) {
            case Some(states) => Some(states + char)
            case None         => Some(Set(char))
          }
        }

        private val transitions =
          mutable.HashMap[(Int, Char), Int]().addAll { ${ transitionSeq } }
        private val accept: mutable.HashSet[Int] =
          mutable.HashSet().addAll { ${ acceptingStates } }

        private val stateToToken =
          mutable.HashMap[Int, String => T]().addAll {
            ${ Expr.ofSeq(dfaAcceptToToken) }
          }

        private val failed = mutable.BitSet()

        private val bad = -2
        private val error = -1

        private val eb = ${ errorBuilder }
        private var errorMessage: Option[Err] = None

        private def bitSetIndex(pointer: Int, state: Int): Int =
          pointer * (${ Expr(maxId) } - ${ Expr(minId) }) + state

        private def nextWord(): Either[Err, T] = {
          var state: Int = ${ Expr(dfa.initial.id - minId) }
          var lexeme: String = ""
          val stack = mutable.Stack[(Int, Int)]((bad, bad))

          while (
            state != error && (pointer < string.length) && !failed.contains(
              bitSetIndex(pointer, state)
            )
          ) {
            // Get the next character and add it to the lexeme
            val char = string(pointer)
            pointer += 1
            lexeme += char

            if (accept.contains(state)) stack.clear()

            stack.push((state, pointer))

            if transitions.contains((state, char))
            then state = transitions((state, char))
            else state = error

            if (state == error || pointer >= string.length) {
              errorMessage = Some(
                eb.build(
                  eb.pos(pointer, 0),
                  eb.unexpected(
                    char,
                    expected = validTransitions.getOrElse(state, Set.empty)
                  )
                )
              )
            }
          }

          while (state != bad && !accept.contains(state)) {
            failed += bitSetIndex(pointer, state)
            val (s, pos) = stack.pop()
            state = s
            pointer = pos
            lexeme = lexeme.dropRight(1)
            pointer -= 1
          }

          // If an accepting state is found, return the token associated with the state, otherwise return an error.
          if accept.contains(state)
          then Right(stateToToken(state)(lexeme))
          else Left(errorMessage.get)
        }

        override def lex(input: String): Either[Err, List[T]] = {
          // Set the string to be lexed
          string = input

          // Reset variables needed for lexing.
          pointer = 0
          failed.clear()

          val result = ListBuffer[T]()
          // Repeatedly get words from the input.
          while (0 <= pointer && pointer < string.length) {
            // Get the next word.
            val wordOrErr: Either[Err, T] = nextWord()

            // If the word is not accepted, return an error, otherwise add it to the result.
            wordOrErr match {
              case Right(word) => result.addOne(word)
              case Left(err)   => return Left(err)
            }
          }

          Right(result.result())
        }
      }
    }

    result
  }
}

abstract class Lexer[T, Err] {
  def lex(input: String): Either[Err, List[T]]
}
