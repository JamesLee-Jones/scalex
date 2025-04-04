class RegExParserTests
import org.scalatest.flatspec.AnyFlatSpec
import lexer.RegExParser

import scala.collection.mutable

class RegExFlatSpec extends AnyFlatSpec {
  "An empty Automata" should "have one transition" in {
    val nfa = RegExParser.parseToNFA("")
    assert(nfa.transitionCount() == 1)
  }

  "An empty Automata" should "have an epsilon transition" in {
    val nfa = RegExParser.parseToNFA("")
    assert(nfa.initial.transitions.head._1.isEmpty)
  }

  "A constant Automata" should "have one transition" in {
    val nfa = RegExParser.parseToNFA("a")
    assert(nfa.transitionCount() == 1)
  }

  "A constant Automata" should "have a transition matching the constant" in {
    val nfa = RegExParser.parseToNFA("a")
    assert(nfa.initial.transitions.head._1.contains('a'))
  }
}
