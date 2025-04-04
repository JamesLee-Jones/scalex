class AutomataTests

import org.scalatest.flatspec.AnyFlatSpec
import lexer.{Automata, State}

import scala.collection.mutable

class AutomataFlatSpec extends AnyFlatSpec {
  "An empty Automata" should "have one transition" in {
    assert(Automata.empty().transitionCount() == 1)
  }

  "An empty Automata" should "have an epsilon transition" in {
    assert(Automata.empty().initial.transitions.head._1.isEmpty)
  }

  "A constant Automata" should "have one transition" in {
    assert(Automata.const('a').transitionCount() == 1)
  }

  "A constant Automata" should "have a transition matching the constant" in {
    assert(Automata.const('a').initial.transitions.head._1.contains('a'))
  }
}
