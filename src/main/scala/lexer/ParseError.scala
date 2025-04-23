package lexer

case class ParseError(message: String, position: SourcePosition) {
  def format: String = {
    s"Parse error at (${position.line}, ${position.column}): $message"
  }
}
