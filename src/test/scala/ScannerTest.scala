class ScanTest

import org.scalatest.flatspec.AnyFlatSpec
import lexer.{
  PositionedToken,
  Scanner,
  SourcePosition,
  TAlternation,
  TChar,
  TClosure,
  TLeftBracket,
  TRightBracket
}

class ScannerFlatSpec extends AnyFlatSpec {
  "Empty regex" should "produce an empty list of tokens" in {
    assert(Scanner.scan("") == List())
  }

  "An character" should "produce a character token" in {
    assert(
      Scanner.scan("a") == List(
        PositionedToken(TChar('a'), SourcePosition(1, 1))
      )
    )
    assert(
      Scanner.scan("8") == List(
        PositionedToken(TChar('8'), SourcePosition(1, 1))
      )
    )
  }

  "Brackets" should "produce bracket tokens" in {
    assert(
      Scanner.scan("(") == List(
        PositionedToken(TLeftBracket, SourcePosition(1, 1))
      )
    )
    assert(
      Scanner.scan(")") == List(
        PositionedToken(TRightBracket, SourcePosition(1, 1))
      )
    )
    assert(
      Scanner.scan("()") == List(
        PositionedToken(TLeftBracket, SourcePosition(1, 1)),
        PositionedToken(TRightBracket, SourcePosition(1, 2))
      )
    )
    assert(
      Scanner.scan("((") == List(
        PositionedToken(TLeftBracket, SourcePosition(1, 1)),
        PositionedToken(TLeftBracket, SourcePosition(1, 2))
      )
    )
  }

  "An alternation" should "return an alternation token" in {
    assert(
      Scanner.scan("|") == List(
        PositionedToken(TAlternation, SourcePosition(1, 1))
      )
    )
    assert(
      Scanner.scan("||") == List(
        PositionedToken(TAlternation, SourcePosition(1, 1)),
        PositionedToken(TAlternation, SourcePosition(1, 2))
      )
    )
  }

  "A closure" should "return a closure token" in {
    assert(
      Scanner.scan("*") == List(PositionedToken(TClosure, SourcePosition(1, 1)))
    )
    assert(
      Scanner.scan("**") == List(
        PositionedToken(TClosure, SourcePosition(1, 1)),
        PositionedToken(TClosure, SourcePosition(1, 2))
      )
    )
  }

  "Consecutive tokens" should "be converted correctly" in {
    assert(
      Scanner.scan("a*b") == List(
        PositionedToken(TChar('a'), SourcePosition(1, 1)),
        PositionedToken(TClosure, SourcePosition(1, 2)),
        PositionedToken(TChar('b'), SourcePosition(1, 3))
      )
    )
    assert(
      Scanner.scan("(a)*b") == List(
        PositionedToken(TLeftBracket, SourcePosition(1, 1)),
        PositionedToken(TChar('a'), SourcePosition(1, 2)),
        PositionedToken(TRightBracket, SourcePosition(1, 3)),
        PositionedToken(TClosure, SourcePosition(1, 4)),
        PositionedToken(TChar('b'), SourcePosition(1, 5))
      )
    )
  }

  "White space" should "be ignored" in {
    assert(Scanner.scan(" ") == List())
    assert(
      Scanner.scan("a b") == List(
        PositionedToken(TChar('a'), SourcePosition(1, 1)),
        PositionedToken(TChar('b'), SourcePosition(1, 3))
      )
    )
  }
}
