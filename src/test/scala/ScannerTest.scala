class ScanTest

import org.scalatest.flatspec.AnyFlatSpec
import lexer.{
  Scanner,
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
    assert(Scanner.scan("a") == List(TChar('a')))
    assert(Scanner.scan("8") == List(TChar('8')))
  }

  "Brackets" should "produce bracket tokens" in {
    assert(Scanner.scan("(") == List(TLeftBracket))
    assert(Scanner.scan(")") == List(TRightBracket))
    assert(Scanner.scan("()") == List(TLeftBracket, TRightBracket))
    assert(Scanner.scan("((") == List(TLeftBracket, TLeftBracket))
  }

  "An alternation" should "return an alternation token" in {
    assert(Scanner.scan("|") == List(TAlternation))
    assert(Scanner.scan("||") == List(TAlternation, TAlternation))
  }

  "A closure" should "return a closure token" in {
    assert(Scanner.scan("*") == List(TClosure))
    assert(Scanner.scan("**") == List(TClosure, TClosure))
  }

  "Consecutive tokens" should "be converted correctly" in {
    assert(Scanner.scan("a*b") == List(TChar('a'), TClosure, TChar('b')))
    assert(
      Scanner.scan("(a)*b") == List(
        TLeftBracket,
        TChar('a'),
        TRightBracket,
        TClosure,
        TChar('b')
      )
    )
  }

  "White space" should "be ignored" in {
    assert(Scanner.scan(" ") == List())
    assert(Scanner.scan("a b") == List(TChar('a'), TChar('b')))
  }
}
