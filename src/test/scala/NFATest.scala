class NFATest

import lexer.{Alt, Ch, NFA, RegEx, Sequence, Star, NFAState, regexToNfa}
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NFAFlatSpec extends AnyFlatSpec {
  "An empty NFA" should "have two nodes and one transition" in {
    assert(NFA.empty().nodeAndTransitionCount() === (2, 1))
  }

  "A constant NFA" should "have two nodes and one transition" in {
    assert(NFA.const('8').nodeAndTransitionCount() === (2, 1))
    assert(NFA.const('a').nodeAndTransitionCount() === (2, 1))
  }

  def checkCounts(left: NFA, right: NFA)(
      op: (NFA, NFA) => NFA
  )(nodeDiff: Int, transitionDiff: Int): Unit = {
    val (leftNodes, leftTransitions) = left.nodeAndTransitionCount()
    val (rightNodes, rightTransitions) = right.nodeAndTransitionCount()
    assert(
      op(left, right).nodeAndTransitionCount() === (
        leftNodes + rightNodes + nodeDiff,
        leftTransitions + rightTransitions + transitionDiff
      )
    )
  }
  def checkCounts(
      nfa: NFA
  )(op: NFA => NFA)(nodeDiff: Int, transitionDiff: Int): Unit = {
    val (nodeCount, transitionCount) = nfa.nodeAndTransitionCount()
    assert(
      op(nfa).nodeAndTransitionCount() === (
        nodeCount + nodeDiff,
        transitionCount + transitionDiff
      )
    )
  }

  "A sequence of n constant regexs" should "have n nodes and n-1 transitions" in {
    assert(
      (NFA.const('8') ~> NFA.const('b')).nodeAndTransitionCount() === (4, 3)
    )
    assert(
      (NFA.const('h') ~> NFA.const('g') ~> NFA.const('d'))
        .nodeAndTransitionCount() === (6, 5)
    )
    var nfa = NFA.const('a')
    for (n <- 1 to 100) {
      assert(nfa.nodeAndTransitionCount() === (2 * n, 2 * n - 1))
      nfa = nfa ~> NFA.const('a')
    }
  }

  val nfas: List[() => NFA] = List(
    () => NFA.const('a'),
    () => NFA.const('b'),
    () => NFA.repeat(NFA.const('b')),
    () => NFA.const('a') ~> NFA.const('b'),
    () => NFA.repeat(NFA.const('a') ~> NFA.const('b')),
    () => NFA.const('a') <|> NFA.const('b'),
    () => NFA.const('a') <|> NFA.const('b') <|> NFA.repeat(NFA.const('5')),
    () => NFA.const('a') <|> NFA.const('b') <|> NFA.repeat(NFA.const('5'))
  )

  "An alternation" should "add two new nodes and four new transitions" in {
    val op = (left: NFA, right: NFA) => left <|> right
    val check = (left: NFA, right: NFA) => checkCounts(left, right)(op)(2, 4)

    for (left <- nfas) {
      for (right <- nfas)
        check(left(), right())
    }
  }

  "A closure" should "add two new nodes and four new transitions" in {
    val op = NFA.repeat(_)
    val check = (nfa: NFA) => checkCounts(nfa)(op)(2, 4)
    for (nfa <- nfas) check(nfa())
  }

  /** Check that a breadth first search of an NFAs and the NFA produced from a
    * regex lead to an equivalent set of transitions.
    */
  def checkTransitionalEquality(nfa: NFA, regEx: RegEx): Assertion = {
    val getTransitions = (nfa: NFA, lb: ListBuffer[Option[Char]]) =>
      nfa.traverse(
        (_, _) => (),
        (),
        (transition, _) => lb.prepend(transition._2),
        ()
      )

    val nfaTransitions = ListBuffer[Option[Char]]()
    val regexTransitions = ListBuffer[Option[Char]]()
    val regexNfa = regexToNfa(regEx)
    getTransitions(nfa, nfaTransitions)
    getTransitions(regexNfa, regexTransitions)
    assert(nfaTransitions === regexTransitions)
    assert(nfa.nodeAndTransitionCount() === regexNfa.nodeAndTransitionCount())
  }

  "A constant regex" should "be converted to a constant NFA" in {
    checkTransitionalEquality(NFA.const('a'), Ch('a'))
    checkTransitionalEquality(NFA.const('8'), Ch('8'))
  }

  "A sequence regex" should "be converted to a sequence NFA" in {
    checkTransitionalEquality(
      NFA.const('a') ~> NFA.const('b'),
      Sequence(Ch('a'), Ch('b'))
    )
    checkTransitionalEquality(
      NFA.const('8') ~> NFA.const('g') ~> NFA.const('i'),
      Sequence(Ch('8'), Ch('g'), Ch('i'))
    )
  }

  "An alternation regex" should "be converted to an alternation NFA" in {
    checkTransitionalEquality(
      NFA.const('a') <|> NFA.const('b'),
      Alt(Ch('a'), Ch('b'))
    )
    checkTransitionalEquality(
      NFA.const('8') <|> NFA.const('g') <|> NFA.const('i'),
      Alt(Alt(Ch('8'), Ch('g')), Ch('i'))
    )
  }

  "A closure regex" should "be converted to a repeated NFA" in {
    checkTransitionalEquality(NFA.repeat(NFA.const('a')), Star(Ch('a')))
    checkTransitionalEquality(
      NFA.repeat(NFA.const('i')) ~> NFA.const('5'),
      Sequence(Star(Ch('i')), Ch('5'))
    )
    checkTransitionalEquality(
      NFA.const('8') ~> NFA.repeat(NFA.const('g')),
      Sequence(Ch('8'), Star(Ch('g')))
    )
    checkTransitionalEquality(
      NFA.repeat(NFA.const('8') ~> NFA.repeat(NFA.const('g'))),
      Star(Sequence(Ch('8'), Star(Ch('g'))))
    )
  }

  "A regex" should "be converted to an equivalent NFA" in {
    val regexNfa = List(
      (
        (NFA.const('r') ~> NFA.const('8')) <|> (NFA.const('b') ~> NFA.const(
          'i'
        )),
        Alt(Sequence(Ch('r'), Ch('8')), Sequence(Ch('b'), Ch('i'))),
        (
          NFA.const('a') <|> NFA.const('b') <|> NFA.repeat(NFA.const('5')),
          Alt(Alt(Ch('a'), Ch('b')), Ch('5'))
        ),
        (
          NFA.const('y') ~> (NFA.const('t') <|> NFA.const('e')),
          Sequence(Ch('y'), Alt(Ch('t'), Ch('e')))
        )
      )
    )
    for (pair <- regexNfa)
      checkTransitionalEquality(pair._1, pair._2)
  }
}
