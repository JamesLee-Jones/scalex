package lexer

import scala.collection.mutable.ListBuffer

sealed trait Token

case object TClosure extends Token
case object TAlternation extends Token
case object TLeftBracket extends Token
case object TRightBracket extends Token
case class TChar(char: Char) extends Token

case class SourcePosition(line: Int, column: Int)

case class PositionedToken(token: Token, position: SourcePosition)

object Scanner {
  def scan(regex: String): List[PositionedToken] = {
    var line = 1
    var column = 1

    val result = ListBuffer[PositionedToken]()
    // Ignore whitespace.
    for (char <- regex; if char != ' ') {
      result.addOne(PositionedToken(char match {
        case '*' => TClosure
        case '|' => TAlternation
        case '(' => TLeftBracket
        case ')' => TRightBracket
        case c   => TChar(c)
      }, SourcePosition(line, column)))
      column += 1
    }
    result.toList
  }
}
