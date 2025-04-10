package lexer

import scala.collection.mutable

object Parser {
  def parse(tokens: List[Token]): RegEx = {
    if (tokens.isEmpty) return Emp

    val regexpStack : mutable.Stack[RegEx] = mutable.Stack()
    val operatorStack : mutable.Stack[Token] = mutable.Stack()
    var current: Option[RegEx] = None

    for (t <- tokens) {
      if ((t == TAlternation || t == TRightBracket) &&
          operatorStack.nonEmpty &&
          operatorStack.top == TAlternation) {
        if (regexpStack.isEmpty || current.isEmpty) throw new Error()
        operatorStack.pop()
        current = Some(Alt(regexpStack.pop(), current.get))
      }

      t match {
        case TLeftBracket =>
          operatorStack.push(TLeftBracket)
          if (current.nonEmpty) {
            regexpStack.push(current.get)
            current = None
          }

        case TRightBracket =>
          if (operatorStack.isEmpty || operatorStack.pop() != TLeftBracket) throw new Error()


        case TAlternation =>
          if (current.isEmpty) throw new Error()
          regexpStack.push(current.get)
          current = None
          operatorStack.push(TAlternation)

        case TClosure =>
          // TODO: This can be abstracted
          current = Some(current match {
            case Some(Sequence(regExs)) => Sequence(regExs.updated(regExs.size-1, Star(regExs.last)))
            case Some(regex) => Star(regex)
            case _           => throw new Error()
          })

        case TChar(char) =>
          // TODO: Factor this out
          current = Some(current match {
            case Some(regex) => regex ~> Ch(char)
            case _ => Ch(char)
          })
      }
    }

    if (current.nonEmpty) regexpStack.push(current.get)

    while (operatorStack.nonEmpty) {
      if (operatorStack.top != TAlternation || regexpStack.size < 2) throw new Error()
      operatorStack.pop()
      val second = regexpStack.pop()
      regexpStack.push(Alt(regexpStack.pop(), second))
    }

    if (regexpStack.size == 2) {
      val second = regexpStack.pop()
      regexpStack.push(regexpStack.pop() ~> second)
    }

    println(operatorStack)
    println(regexpStack)
    if (operatorStack.nonEmpty || regexpStack.size > 1) throw new Error()

    if regexpStack.isEmpty then Emp else regexpStack.top
  }
}
