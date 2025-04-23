package lexer

trait ErrorBuilder[Err] {
  def build(pos: Position, lines: ErrorInfoLines): Err

  def pos(line: Int, column: Int): Position

  def unexpected(unexpected: Char, expected: Set[Char]): ErrorInfoLines

  type Position
  type ErrorInfoLines
  type Unexpected
  type Expected
}

type Pos = (Int, Int)

case class LexerError(pos: Pos, errorInfo: String) {
  def format: String = {
    s"Lexer error at (${pos._1}, ${pos._2}): $errorInfo"
  }
}

class DefaultErrorBuilder extends ErrorBuilder[LexerError] {
  override def build(pos: Position, lines: ErrorInfoLines): LexerError =
    LexerError(pos, lines)

  override def unexpected(
      unexpected: Unexpected,
      expected: Expected
  ): ErrorInfoLines =
    s"Unexpected character: $unexpected. Expected one of: ${expected.mkString(", ")}"

  override def pos(line: Int, column: Int): Position = (line, column)

  override type Position = Pos
  override type ErrorInfoLines = String
  override type Unexpected = Char
  override type Expected = Set[Char]
}
