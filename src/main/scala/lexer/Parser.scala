package lexer

import scala.collection.mutable

object Parser {
  type Parser[A] = (tokens: List[PositionedToken]) => Either[
    ParseError,
    (List[PositionedToken], A)
  ]

  private def checkToken(
      expected: Token,
      tokens: List[PositionedToken]
  ): Either[ParseError, List[PositionedToken]] = {
    tokens match {
      case PositionedToken(token, pos) :: tail =>
        if (token == expected) Right(tail)
        else
          Left(
            ParseError(
              s"Expected token ${expected} not equal to actual token ${token}",
              pos
            )
          )
      case Nil =>
        Left(
          ParseError(
            s"Expected token ${expected} but no tokens left",
            SourcePosition(0, 0)
          )
        )
    }
  }

  private def parseAtom(
      tokens: List[PositionedToken]
  ): Either[ParseError, (List[PositionedToken], RegEx)] = {
    tokens match {
      case Nil                                        => Right((tokens, Emp))
      case PositionedToken(TChar(c), _) :: tail       => Right((tail, Ch(c)))
      case PositionedToken(TLeftBracket, pos) :: tail =>
        // When encountering a left bracket, parse until a matching right bracket.
        tail match {
          case PositionedToken(TRightBracket, _) :: tail => Right((tail, Emp))
          case _ =>
            for {
              (remainingTokens, regex) <- parseAlternation(tail)
              tokensAfterBracket <- checkToken(
                TRightBracket,
                remainingTokens
              ) match {
                case Left(_) =>
                  Left(
                    ParseError(
                      "Left bracket does not have a matching right bracket.",
                      pos
                    )
                  )
                case Right(result) => Right(result)
              }
            } yield (tokensAfterBracket, regex)
        }

      case PositionedToken(TAlternation, pos) :: _ =>
        Left(
          ParseError(
            s"The left-hand side of an alternation must be an expression.",
            pos
          )
        )

      case posTok :: _ =>
        Left(ParseError(s"Unexpected token ${posTok.token}", posTok.position))
    }
  }

  private def parseStar(
      tokens: List[PositionedToken]
  ): Either[ParseError, (List[PositionedToken], RegEx)] = {
    for {
      (remainingTokens, regex) <- parseAtom(tokens)
      result <- remainingTokens match {
        case PositionedToken(TClosure, _) :: tail => Right((tail, Star(regex)))
        case _ => Right(remainingTokens, regex)
      }
    } yield result
  }

  private def parseSeq(
      tokens: List[PositionedToken]
  ): Either[ParseError, (List[PositionedToken], RegEx)] = {
    for {
      (rest, headRegex) <- parseStar(tokens)
      result <- rest match {
        case Nil                                    => Right((rest, headRegex))
        case PositionedToken(TAlternation, _) :: _  => Right(rest, headRegex)
        case PositionedToken(TRightBracket, _) :: _ => Right(rest, headRegex)
        case _ =>
          for {
            (rest, restRegex) <- parseSeq(rest)
          } yield (rest, headRegex ~> restRegex)
      }
    } yield result
  }

  private def parseAlternation(
      tokens: List[PositionedToken]
  ): Either[ParseError, (List[PositionedToken], RegEx)] = {
    if (tokens.isEmpty) return Right((tokens, Emp))
    for {
      (rest, leftRegex) <- parseSeq(tokens)
      result <- rest match {
        case PositionedToken(TAlternation, pos) :: tail =>
          for {
            (rest, rightRegex) <- parseSeq(tail)
            _ <- rightRegex match {
              case Emp =>
                Left(
                  ParseError(
                    "The right-hand side of an alternation must be an expression.",
                    pos
                  )
                )
              case _ => Right(())
            }
          } yield (rest, Alt(leftRegex, rightRegex))
        case _ => Right((rest, leftRegex))
      }
    } yield result
  }

  def parseRegex(tokens: List[PositionedToken]): Either[ParseError, RegEx] = {
    for {
      (rest, regex) <- parseAlternation(tokens)
      result <- {
        if (rest.nonEmpty)
          Left(
            ParseError(
              "Right bracket does not have a matching left bracket.",
              rest.head.position
            )
          )
        else Right(regex)
      }
    } yield result
  }

  def parse(tokens: List[Token]): RegEx = {
    if (tokens.isEmpty) return Emp

    val regexpStack: mutable.Stack[RegEx] = mutable.Stack()
    val operatorStack: mutable.Stack[Token] = mutable.Stack()
    var current: Option[RegEx] = None

    for (t <- tokens) {
      if (
        (t == TAlternation || t == TRightBracket) &&
        operatorStack.nonEmpty &&
        operatorStack.top == TAlternation
      ) {
        if (regexpStack.isEmpty || current.isEmpty) throw new Error()
        operatorStack.pop()
        current = Some(Alt(regexpStack.pop(), current.get))
      }

      t match {
        case TLeftBracket =>
          operatorStack.push(TLeftBracket)
          if (current.nonEmpty) {
            regexpStack.push(current.get)
            current = None
          }

        case TRightBracket =>
          if (operatorStack.isEmpty || operatorStack.pop() != TLeftBracket)
            throw new Error()

        case TAlternation =>
          if (current.isEmpty) throw new Error()
          regexpStack.push(current.get)
          current = None
          operatorStack.push(TAlternation)

        case TClosure =>
          // TODO: This can be abstracted
          current = Some(current match {
            case Some(Sequence(regExs)) =>
              Sequence(regExs.updated(regExs.size - 1, Star(regExs.last)))
            case Some(regex) => Star(regex)
            case _           => throw new Error()
          })

        case TChar(char) =>
          // TODO: Factor this out
          current = Some(current match {
            case Some(regex) => regex ~> Ch(char)
            case _           => Ch(char)
          })
      }
    }

    if (current.nonEmpty) regexpStack.push(current.get)

    while (operatorStack.nonEmpty) {
      if (operatorStack.top != TAlternation || regexpStack.size < 2)
        throw new Error()
      operatorStack.pop()
      val second = regexpStack.pop()
      regexpStack.push(Alt(regexpStack.pop(), second))
    }

    if (regexpStack.size == 2) {
      val second = regexpStack.pop()
      regexpStack.push(regexpStack.pop() ~> second)
    }

    println(operatorStack)
    println(regexpStack)
    if (operatorStack.nonEmpty || regexpStack.size > 1) throw new Error()

    if regexpStack.isEmpty then Emp else regexpStack.top
  }
}
