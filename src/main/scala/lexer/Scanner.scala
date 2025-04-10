package lexer

import scala.collection.mutable.ListBuffer

sealed trait Token

case object TClosure extends Token
case object TAlternation extends Token
case object TLeftBracket extends Token
case object TRightBracket extends Token
case class TChar(char: Char) extends Token


object Scanner {
  def scan(regex: String): List[Token] = {
    val result = ListBuffer[Token]()
    // Ignore whitespace.
    for (char <- regex; if char != ' ') {
      result.addOne(char match {
        case '*' => TClosure
        case '|' => TAlternation
        case '(' => TLeftBracket
        case ')' => TRightBracket
        case c => TChar(c)
      })
    }
    result.toList
  }
}
