class LexerTest

import lexer.{Lexer, ->}
import org.scalatest.flatspec.AnyFlatSpec

// Resulting tokens to be used for testing
sealed trait LexerTok
case object Tok1 extends LexerTok
case object Tok2 extends LexerTok
case object Tok3 extends LexerTok
case object Tok4 extends LexerTok

class LexerFlatSpec extends AnyFlatSpec {
  "Lexer" should "lex constants correctly" in {
    val lexer = Lexer("a" -> Tok1)
    assert(lexer.lex("a") === List(Tok1))
    assert(lexer.lex("b") === List())
    assert(lexer.lex("ab") === List(Tok1))
  }

  "Lexer" should "lex sequences correctly" in {
    val lexer = Lexer("abc" -> Tok2)
    assert(lexer.lex("a") === List())
    assert(lexer.lex("b") === List())
    assert(lexer.lex("ab") === List())
    assert(lexer.lex("abc") === List(Tok2))
    assert(lexer.lex("abcd") === List(Tok2))
    assert(lexer.lex("aalksd") === List())
  }

  "Lexer" should "lex closure correctly" in {
    val lexer = Lexer("a*" -> Tok3)
    assert(lexer.lex("a") === List(Tok3))
    assert(lexer.lex("b") === List())
    assert(lexer.lex("ab") === List())
    assert(lexer.lex("aaaaaaaa") === List(Tok3))
    assert(lexer.lex("aba") === List(Tok3))
    assert(lexer.lex("baaaaaaaa") === List())
  }
}
