package lexer

import lexer.NFA.combine

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.quoted.*

extension (regex: String)
  inline def ->[T](inline token: T): RegexToToken[T] =
    RegexToToken(regex, token)

case class RegexToToken[T](regex: String, token: T)

object Lexer {
  inline def apply[T](inline regexToTokens: RegexToToken[T]*): Lexer[T] =
    ${ applyImpl('{ regexToTokens }) }

  private def applyImpl[T](
      regexToTokens: Expr[Seq[RegexToToken[T]]]
  )(using Type[T])(using ctx: Quotes): Expr[Lexer[T]] = {
    import ctx.reflect.*
    // Parse the input regular expressions to an internal representation and error if they cannot be parsed.
    val parsed: Seq[(RegEx, Expr[T])] = regexToTokens match {
      case Varargs(args) =>
        args.map { case '{ RegexToToken($regex, $token) } =>
          val lexed = Scanner.scan(regex.valueOrAbort)
          Parser.parseRegex(lexed) match {
            case Right(value) => (value, token.asExprOf[T])
            case Left(error) =>
              report.error(error.format); (Emp, ???)
          }
        }
    }

    // Convert each regular expression to an NFA
    val nfaToToken: Seq[(NFA, Expr[T])] = parsed.map { (regex, token) =>
      (regexToNfa(regex), token)
    }

    // Map NFA accepting states to resulting tokens.
    val nfaAcceptingStatesToToken: mutable.HashMap[Int, Expr[T]] =
      mutable.HashMap()
    nfaToToken.foreach((nfa, token) =>
      nfa.accept.foreach(state =>
        nfaAcceptingStatesToToken.addOne(state.id, token)
      )
    )

    // Combine the NFAs into one NFA
    val nfas = nfaToToken.map((nfa, _) => nfa)
    val combinedNfa = combine(nfas)

    // Convert the NFA into a DFA
    val (dfa, nfaToDfaAccept) = nfaToDfa(combinedNfa)

    val (minId, maxId) = dfa.nodeIdRange()
    val acceptingStates: Expr[Seq[Int]] = Expr(
      dfa.accept.toSeq.map(s => s.id - minId)
    )

    var dfaAcceptToToken: Seq[Expr[(Int, T)]] = Seq()
    nfaToDfaAccept.foreach((nfaId, dfaIds) =>
      dfaIds.foreach(dfaId =>
        dfaAcceptToToken = dfaAcceptToToken.appended('{
          (${ Expr(dfaId-minId) }, ${ nfaAcceptingStatesToToken(nfaId) })
        })
      )
    )


    nfaToDfaAccept
      .map((nfaId, dfaId) =>
        '{ (${ Expr(dfaId) }, ${ nfaAcceptingStatesToToken(nfaId) }) }
      )
      .toSeq

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

    val result = '{
      new Lexer[T] {
        private var string: String = ""
        private var pointer: Int = 0

        private val transitions =
          mutable.HashMap[(Int, Char), Int]().addAll { ${ transitionSeq } }
        private val accept: mutable.HashSet[Int] =
          mutable.HashSet().addAll { ${ acceptingStates } }

        private val stateToToken =
          mutable.HashMap[Int, T]().addAll { ${ Expr.ofSeq(dfaAcceptToToken) } }

        private val failed = mutable.BitSet()

        private val bad = -2
        private val error = -1

        private def bitSetIndex(pointer: Int, state: Int): Int =
          pointer * (${ Expr(maxId) } - ${ Expr(minId) }) + state

        private def nextWord(): Option[T] = {
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
          }

          while (state != bad && !accept.contains(state)) {
            failed += bitSetIndex(pointer, state)
            val (s, pos) = stack.pop()
            state = s
            pointer = pos
            lexeme.dropRight(1)
            pointer -= 1
          }

          if accept.contains(state)
          then Some(stateToToken(state))
          else None
        }

        override def lex(input: String): List[T] = {
          // Set the string to be lexed
          string = input
          
          // Reset variables needed for lexing.
          pointer = 0
          failed.clear()
          
          val result = ListBuffer[T]()
          // Repeatedly get words from the input.
          while (0 <= pointer && pointer < string.length) {
            val word: Option[T] = nextWord()
            if word.nonEmpty then result.addOne(word.get)
          }
          result.result()
        }
      }
    }

    result
  }
}

abstract class Lexer[T]() {
  def lex(input: String): List[T]
}
