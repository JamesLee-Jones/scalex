package lexer

@main
def main(): Unit = {
  for {
    result <- Parser.parseRegex(Scanner.scan("a(b|c)*"))
  } do {
    val dfa = nfaToDfa(regexToNfa(result))
    dfa.printTransitions()
    println(dfa.nodeAndTransitionCount())
  }
}
