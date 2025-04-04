package lexer

import scala.collection.mutable

object RegExParser {
  private def isOperator(char: Char): Boolean = {
    // TODO: Fill in the rest
    char == '|' || char == '(' || char == ')'
  }

  def parseToNFA(regEx: String): Automata = {
    // TODO: This could probably be abstracted
    if (regEx.isEmpty) {
      return Automata.empty()
    }

    val automataStack : mutable.Stack[Automata] = mutable.Stack()
    val operatorStack : mutable.Stack[Char] = mutable.Stack()

    for (c <- regEx) {
      if ((c == '|' || c == ')') && operatorStack.nonEmpty && operatorStack.top == '|') {
        if (automataStack.size < 2) throw new Error()
        operatorStack.pop()
        automataStack.push(automataStack.pop() <|> automataStack.pop())
      }
      c match {
        case '(' => operatorStack.push(c)
        case ')' =>
          if (operatorStack.isEmpty || operatorStack.top != '(') throw new Error()
          operatorStack.pop()

        // TODO: Check open bracket is top operator
        case '|' =>
          operatorStack.push(c)

        case '*' =>
          if (automataStack.isEmpty) throw new Error()
          automataStack.push(Automata.repeat(automataStack.pop()))

        case _ =>
          if (automataStack.size >= 2)  {
            val second = automataStack.pop()
            val first = automataStack.pop()
            automataStack.push(first ~> second)
          }
          automataStack.push(Automata.const(c))
      }
    }

    if (automataStack.size == 2 && operatorStack.isEmpty) {
      val second = automataStack.pop()
      val first = automataStack.pop()
      return first ~> second
    } else if (automataStack.size == 2 && operatorStack.size == 1 && operatorStack.top == '|') {
      val second = automataStack.pop()
      val first = automataStack.pop()
      return first <|> second
    }
    automataStack.top
  }

}


@main def main(): Unit = {
  RegExParser.parseToNFA("(a|b)*").printTransitions()
}