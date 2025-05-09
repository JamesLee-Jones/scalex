class DFATest

import lexer.{NFA, NFAState, epsilon, epsilonClosure}
import org.scalatest.flatspec.AnyFlatSpec

class DFAFlatSpec extends AnyFlatSpec {
  val epsilonClosureSize: NFA => Int = (nfa: NFA) =>
    epsilonClosure(Set(nfa.initial)).size

  "The epsilon closure of an empty NFA" should "contain all nodes" in {
    assert(epsilonClosureSize(NFA.empty()) === 2)
  }

  "An empty NFA" should "have one accepting state" in {
    assert(NFA.empty().accept.size === 1)
  }

  "The epsilon closure of a constant NFA" should "contain one node" in {
    assert(epsilonClosureSize(NFA.const('a')) === 1)
  }

  "A constant NFA" should "have one accepting state" in {
    assert(NFA.const('a').accept.size === 1)
  }

  "The epsilon closure of a sequence NFA" should "contain one node" in {
    assert(epsilonClosureSize(NFA.const('a') ~> NFA.const('b')) === 1)
  }

  "A sequence NFA" should "have one accepting state" in {
    assert((NFA.const('a') ~> NFA.const('b')).accept.size === 1)
  }

  "The epsilon closure of an alternation NFA" should "contain three node" in {
    assert(epsilonClosureSize(NFA.const('a') <|> NFA.const('b')) === 3)
  }

  "An alternation NFA" should "have one accept state" in {
    assert((NFA.const('a') <|> NFA.const('b')).accept.size === 1)
  }

  "The epsilon closure of a closure NFA" should "contain three node" in {
    assert(epsilonClosureSize(NFA.repeat(NFA.const('a'))) === 3)
  }

  "A closure NFA" should "have one accept state" in {
    assert(NFA.repeat(NFA.const('a')).accept.size === 1)
  }

  "The epsilon closure of an NFA with n epsilon transitions" should "contain n+1 nodes" in {
    // It is n+1 because the epsilon transition should also contain the starting node.
    val initial = NFAState()
    val nfa = NFA(initial, Set())
    for (i <- 1 to 100) {
      initial.addTransition(epsilon, NFAState())
      assert(epsilonClosureSize(nfa) === i + 1)
    }
  }
}
