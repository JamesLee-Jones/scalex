package lexer

@main
def main(): Unit = {
  for {
    result <- Parser.parseRegex(Scanner.scan("a|a"))
  } do {
    val nfa = regexToNfa(Alt(Ch('a'), Ch('a')))
    println(nfa)
    println(nfa.printTransitions())
    println(nfa.nodeAndTransitionCount())
  }
}
