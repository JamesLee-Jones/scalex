package lexer

case class ParseError(val message: String, val position: SourcePosition) {
  def format: String = {
    s"Parse error at (${position.line}, ${position.column}): $message"
  }
}
