class ParserTest

import lexer.{
  Alt,
  Ch,
  Emp,
  ParseError,
  Parser,
  RegEx,
  Scanner,
  Sequence,
  SourcePosition,
  Star
}
import org.scalatest.flatspec.AnyFlatSpec

class ParserFlatSpec extends AnyFlatSpec {
  def parsePrintInverse(regex: String): Unit = {
    val regex = Parser.parseRegex(Scanner.scan("(a|b)*"))
    assert(regex.isRight)
    regex match {
      case Right(result) =>
        val printed = RegEx.prettyPrint(result)
        val reparsedRegex = Parser.parseRegex(Scanner.scan(printed))
        assert(regex == reparsedRegex)

      case Left(_) =>
        assert(false)
    }
  }

  "An empty regex" should "return Emp" in {
    assert(Parser.parseRegex(Scanner.scan("")) == Right(Emp))
  }

  "An constant regex" should "return a constant" in {
    assert(Parser.parseRegex(Scanner.scan("a")) == Right(Ch('a')))
    assert(Parser.parseRegex(Scanner.scan("4")) == Right(Ch('4')))
  }

  "An sequence regex" should "return a Seq" in {
    assert(
      Parser.parseRegex(Scanner.scan("gb")) == Right(Sequence(Ch('g'), Ch('b')))
    )
    assert(
      Parser.parseRegex(Scanner.scan("gb64")) == Right(
        Sequence(
          Ch('g'),
          Ch('b'),
          Ch('6'),
          Ch('4')
        )
      )
    )
  }

  "An alternation" should "return an alternation with the regexes either side" in {
    assert(
      Parser.parseRegex(Scanner.scan("a|b")) == Right(Alt(Ch('a'), Ch('b')))
    )
    assert(
      Parser.parseRegex(Scanner.scan("a1|b2")) == Right(
        Alt(
          Sequence(Ch('a'), Ch('1')),
          Sequence(Ch('b'), Ch('2'))
        )
      )
    )
  }

  "Matching brackets" should "parse correctly" in {
    assert(Parser.parseRegex(Scanner.scan("()")) == Right(Emp))
    assert(Parser.parseRegex(Scanner.scan("(a)")) == Right(Ch('a')))
    assert(
      Parser.parseRegex(Scanner.scan("(ab)")) == Right(
        Sequence(Ch('a'), Ch('b'))
      )
    )
    assert(
      Parser.parseRegex(Scanner.scan("(a1)|(b2)")) == Right(
        Alt(
          Sequence(Ch('a'), Ch('1')),
          Sequence(Ch('b'), Ch('2'))
        )
      )
    )
    assert(
      Parser.parseRegex(Scanner.scan("(a1|b)2")) == Right(
        Sequence(
          Alt(Sequence(Ch('a'), Ch('1')), Ch('b')),
          Ch('2')
        )
      )
    )
    assert(
      Parser.parseRegex(Scanner.scan("a(1|b)2")) == Right(
        Sequence(
          Ch('a'),
          Alt(Ch('1'), Ch('b')),
          Ch('2')
        )
      )
    )
    assert(
      Parser.parseRegex(Scanner.scan("a(1|b2)")) == Right(
        Sequence(
          Ch('a'),
          Alt(Ch('1'), Sequence(Ch('b'), Ch('2')))
        )
      )
    )
  }

  "Closure" should "bind correctly" in {
    assert(Parser.parseRegex(Scanner.scan("a*")) == Right(Star(Ch('a'))))
    assert(
      Parser.parseRegex(Scanner.scan("ab*")) == Right(
        Sequence(Ch('a'), Star(Ch('b')))
      )
    )
    assert(
      Parser.parseRegex(Scanner.scan("a*b")) == Right(
        Sequence(Star(Ch('a')), Ch('b'))
      )
    )
    assert(
      Parser.parseRegex(Scanner.scan("a(1|b)*2")) == Right(
        Sequence(
          Ch('a'),
          Star(Alt(Ch('1'), Ch('b'))),
          Ch('2')
        )
      )
    )
  }

  "Non matching brackets" should "return an error" in {
    val rightError = (line: Int, col: Int) =>
      Left(
        ParseError(
          "Left bracket does not have a matching right bracket.",
          SourcePosition(line, col)
        )
      )
    val leftError = (line: Int, col: Int) =>
      Left(
        ParseError(
          "Right bracket does not have a matching left bracket.",
          SourcePosition(line, col)
        )
      )
    val expected =
      (line: Int, col: Int) => Left(ParseError("", SourcePosition(line, col)))
    assert(Parser.parseRegex(Scanner.scan("(()")) === rightError(1, 1))
    assert(Parser.parseRegex(Scanner.scan("((a)")) === rightError(1, 1))
    assert(Parser.parseRegex(Scanner.scan("b((a)")) === rightError(1, 2))
    assert(Parser.parseRegex(Scanner.scan("((a)b*")) === rightError(1, 1))
    assert(Parser.parseRegex(Scanner.scan("()a)")) === leftError(1, 4))
    assert(Parser.parseRegex(Scanner.scan("())")) === leftError(1, 3))
    assert(Parser.parseRegex(Scanner.scan("(a))")) === leftError(1, 4))
    assert(Parser.parseRegex(Scanner.scan("(a)b*)")) === leftError(1, 6))
  }

  "An alternation without two arguments" should "return an error" in {
    val rightError = (line: Int, col: Int) =>
      Left(
        ParseError(
          "The right-hand side of an alternation must be an expression.",
          SourcePosition(line, col)
        )
      )
    val leftError = (line: Int, col: Int) =>
      Left(
        ParseError(
          s"The left-hand side of an alternation must be an expression.",
          SourcePosition(line, col)
        )
      )
    assert(Parser.parseRegex(Scanner.scan("a|")) === rightError(1, 2))
    assert(Parser.parseRegex(Scanner.scan("|b")) === leftError(1, 1))
    assert(Parser.parseRegex(Scanner.scan("ab*|")) === rightError(1, 4))
    assert(Parser.parseRegex(Scanner.scan("|(ab*)")) === leftError(1, 1))
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
