class LexerTest

import lexer.{->, Lexer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.matchPattern
import org.scalatest.matchers.should.Matchers.should

// Resulting tokens to be used for testing
sealed trait LexerTok
case object Tok1 extends LexerTok
case object Tok2 extends LexerTok
case object Tok3 extends LexerTok
case object Tok4 extends LexerTok

class LexerFlatSpec extends AnyFlatSpec {
  "Lexer" should "lex constants correctly" in {
    val lexer = Lexer("a" -> Tok1)
    lexer.lex("a") should matchPattern { case Right(List(Tok1)) => }
    lexer.lex("b") should matchPattern { case Left(_) => }
    lexer.lex("abb") should matchPattern { case Left(_) => }
  }

  "Lexer" should "lex sequences correctly" in {
    val lexer = Lexer("abc" -> Tok2)
    lexer.lex("a") should matchPattern { case Left(_) => }
    lexer.lex("b") should matchPattern { case Left(_) => }
    lexer.lex("ab") should matchPattern { case Left(_) => }
    lexer.lex("abc") should matchPattern { case Right(List(Tok2)) => }
    lexer.lex("alhkhd") should matchPattern { case Left(_) => }
    lexer.lex("abcabc") should matchPattern { case Right(List(Tok2, Tok2)) => }
    lexer.lex("abca") should matchPattern { case Left(_) => }
  }

  "Lexer" should "lex closure correctly" in {
    val lexer = Lexer("a*" -> Tok3)
    lexer.lex("a") should matchPattern { case Right(List(Tok3)) => }
    lexer.lex("b") should matchPattern { case Left(_) => }
    lexer.lex("ab") should matchPattern { case Left(_) => }
    lexer.lex("aaaaaaaa") should matchPattern { case Right(List(Tok3)) => }
    lexer.lex("aba") should matchPattern { case Left(_) => }
    lexer.lex("baaaaaaaa") should matchPattern { case Left(_) => }
  }

  "Lexer" should "lex alternation correctly" in {
    val lexer = Lexer("c|d" -> Tok1)
    lexer.lex("c") should matchPattern { case Right(List(Tok1)) => }
    lexer.lex("d") should matchPattern { case Right(List(Tok1)) => }
    lexer.lex("ccccc") should matchPattern { case Right(List(Tok1, Tok1, Tok1, Tok1, Tok1)) => }
    lexer.lex("dddd") should matchPattern { case Right(List(Tok1, Tok1, Tok1, Tok1)) => }
    lexer.lex("cdcd") should matchPattern { case Right(List(Tok1, Tok1, Tok1, Tok1)) => }
    lexer.lex("Hello World") should matchPattern { case Left(_) => }
    lexer.lex("acd") should matchPattern { case Left(_) => }
    lexer.lex("cad") should matchPattern { case Left(_) => }
    lexer.lex("cda") should matchPattern { case Left(_) => }
  }

  "Lexer" should "lex a combination of constructs correctly" in {
    val lexer = Lexer("a|1*" -> Tok2)
    lexer.lex("a") should matchPattern { case Right(List(Tok2)) => }
    lexer.lex("1") should matchPattern { case Right(List(Tok2)) => }
    lexer.lex("a1") should matchPattern { case Right(List(Tok2, Tok2)) => }
    lexer.lex("a11111111") should matchPattern { case Right(List(Tok2, Tok2)) => }
    lexer.lex("11111111a") should matchPattern { case Right(List(Tok2, Tok2)) => }
    lexer.lex("1111a1111") should matchPattern { case Right(List(Tok2, Tok2, Tok2)) => }
    lexer.lex("11a11a1111") should matchPattern { case Right(List(Tok2, Tok2, Tok2, Tok2, Tok2)) => }
    lexer.lex("a1cd") should matchPattern { case Left(_) => }
    lexer.lex("ca1d") should matchPattern { case Left(_) => }
    lexer.lex("1cda") should matchPattern { case Left(_) => }
  }

  "Lexer" should "produce the correct token when there are multiple options" in {
    val lexer = Lexer("a" -> Tok1, "b" -> Tok2, "c" -> Tok3)
    lexer.lex("a") should matchPattern { case Right(List(Tok1)) => }
    lexer.lex("b") should matchPattern { case Right(List(Tok2)) => }
    lexer.lex("c") should matchPattern { case Right(List(Tok3)) => }
    lexer.lex("ab") should matchPattern { case Right(List(Tok1, Tok2)) => }
    lexer.lex("ac") should matchPattern { case Right(List(Tok1, Tok3)) => }
    lexer.lex("abc") should matchPattern { case Right(List(Tok1, Tok2, Tok3)) => }
    lexer.lex("bca") should matchPattern { case Right(List(Tok2, Tok3, Tok1)) => }
    lexer.lex("cab") should matchPattern { case Right(List(Tok3, Tok1, Tok2)) => }
  }

  "Lexer" should "match the longest occurrence" in {
    val lexer1 = Lexer("aa" -> Tok1, "a" -> Tok2)
    val lexer2 = Lexer("a" -> Tok2, "aa" -> Tok1)
    lexer1.lex("aa") should matchPattern { case Right(List(Tok1)) => }
    lexer2.lex("aa") should matchPattern { case Right(List(Tok1)) => }
    lexer1.lex("aaa") should matchPattern { case Right(List(Tok1, Tok2)) => }
    lexer2.lex("aaa") should matchPattern { case Right(List(Tok1, Tok2)) => }
    lexer1.lex("aaaa") should matchPattern { case Right(List(Tok1, Tok1)) => }
    lexer2.lex("aaaa") should matchPattern { case Right(List(Tok1, Tok1)) => }
  }
}
