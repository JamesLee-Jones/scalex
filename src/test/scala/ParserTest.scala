class ParserTest

import lexer.{Alt, Ch, Emp, Parser, RegEx, Scanner, Sequence, Star}
import org.scalatest.flatspec.AnyFlatSpec

class ParserFlatSpec extends AnyFlatSpec {
  def parsePrintInverse(regex: String): Unit = {
    val regex = Parser.parse(Scanner.scan("(a|b)*"))
    val printed = RegEx.prettyPrint(regex)
    val reparsedRegex = Parser.parse(Scanner.scan(printed))
    assert(regex == reparsedRegex)
  }

  "An empty regex" should "return Emp" in {
    assert(Parser.parse(Scanner.scan("")) == Emp)
  }

  "An constant regex" should "return a constant" in {
    assert(Parser.parse(Scanner.scan("a")) == Ch('a'))
    assert(Parser.parse(Scanner.scan("4")) == Ch('4'))
  }

  "An sequence regex" should "return a Seq" in {
    assert(Parser.parse(Scanner.scan("gb")) == Sequence(Ch('g'), Ch('b')))
    assert(
      Parser.parse(Scanner.scan("gb64")) == Sequence(
        Ch('g'),
        Ch('b'),
        Ch('6'),
        Ch('4')
      )
    )
  }

  "An alternation" should "return an alternation with the regexes either side" in {
    assert(Parser.parse(Scanner.scan("a|b")) == Alt(Ch('a'), Ch('b')))
    assert(
      Parser.parse(Scanner.scan("a1|b2")) == Alt(
        Sequence(Ch('a'), Ch('1')),
        Sequence(Ch('b'), Ch('2'))
      )
    )
  }

  "Matching brackets" should "parse correctly" in {
    assert(Parser.parse(Scanner.scan("()")) == Emp)
    assert(Parser.parse(Scanner.scan("(a)")) == Ch('a'))
    assert(Parser.parse(Scanner.scan("(ab)")) == Sequence(Ch('a'), Ch('b')))
    assert(
      Parser.parse(Scanner.scan("(a1)|(b2)")) == Alt(
        Sequence(Ch('a'), Ch('1')),
        Sequence(Ch('b'), Ch('2'))
      )
    )
    assert(
      Parser.parse(Scanner.scan("(a1|b)2")) == Sequence(
        Alt(Sequence(Ch('a'), Ch('1')), Ch('b')),
        Ch('2')
      )
    )
    assert(
      Parser.parse(Scanner.scan("a(1|b)2")) == Sequence(
        Ch('a'),
        Alt(Ch('1'), Ch('b')),
        Ch('2')
      )
    )
    assert(
      Parser.parse(Scanner.scan("a(1|b2)")) == Sequence(
        Ch('a'),
        Alt(Ch('1'), Sequence(Ch('b'), Ch('2')))
      )
    )
  }

  "Closure" should "bind correctly" in {
    assert(Parser.parse(Scanner.scan("a*")) == Star(Ch('a')))
    assert(
      Parser.parse(Scanner.scan("ab*")) == Sequence(Ch('a'), Star(Ch('b')))
    )
    assert(
      Parser.parse(Scanner.scan("a(1|b)*2")) == Sequence(
        Ch('a'),
        Star(Alt(Ch('1'), Ch('b'))),
        Ch('2')
      )
    )
  }

  "Non matching brackets" should "throw an error" in {
    assertThrows[Error](Parser.parse(Scanner.scan("(()")))
    assertThrows[Error](Parser.parse(Scanner.scan("((a)")))
    assertThrows[Error](Parser.parse(Scanner.scan("((a)b*")))
  }

  "An alternation without two arguments" should "throw an error" in {
    assertThrows[Error](Parser.parse(Scanner.scan("a|")))
    assertThrows[Error](Parser.parse(Scanner.scan("|b")))
    assertThrows[Error](Parser.parse(Scanner.scan("ab*|")))
    assertThrows[Error](Parser.parse(Scanner.scan("|(ab*)")))
  }

  "Printing and parsing" should "be inverses of one another" in {
    List(
      "a",
      "b",
      "a|b",
      "a|b*",
      "(a|b)*",
      "abc*"
    ).foreach(regex => parsePrintInverse(regex))
  }
}
