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
}
