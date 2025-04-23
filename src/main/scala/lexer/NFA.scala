package lexer

import scala.collection.mutable

/** A state in the NFA graph.
  *
  * @param transitions
  *   the outgoing transitions from the current state.
  */
class NFAState(var transitions: Map[Option[Char], Set[NFAState]] = Map.empty)
    extends AutomataState[NFAState, Set] {

  /** Add a transition from the current state to a new state.
    *
    * @param input
    *   The possibly empty character associated with the transition. None
    *   represents an epsilon transition.
    * @param state
    *   The state to transition to.
    */
  def addTransition(maybeChar: Option[Char], state: NFAState): Unit = {
    transitions = transitions.updatedWith(maybeChar) {
      case Some(states) => Some(states + state)
      case None         => Some(Set(state))
    }
  }
}

object NFA {

  /** @return
    *   An empty NFA.
    */
  def empty(): NFA = const(None)

  /** Produce a constant NFA with a single transition.
    * @param char
    *   The character associated with the single transition.
    * @return
    *   A constant NFA.
    */
  def const(char: Char): NFA = const(Some(char))

  private def const(char: Option[Char]): NFA = {
    val start = NFAState()
    val end = NFAState()
    start.addTransition(char, end)
    NFA(start, Set(end))
  }

  def combine(nfas: Seq[NFA]): NFA = {
    val initial = NFAState()
    var accept = Set[NFAState]()
    nfas.foreach(nfa =>
      initial.addTransition(epsilon, nfa.initial)
      accept = accept.union(nfa.accept)
    )
    NFA(initial, accept)
  }

  /** Produce an NFA that can be repeated zero or more times.
    * @param automata
    *   The NFA that can be repeated.
    * @return
    *   An NFA that can be repeated zero or more times.
    */
  def repeat(automata: NFA): NFA = {
    val first = NFAState()
    val last = NFAState()
    first.addTransition(epsilon, automata.initial)
    first.addTransition(epsilon, last)
    automata.accept.foreach(state => state.addTransition(epsilon, last))
    automata.accept.foreach(state =>
      state.addTransition(epsilon, automata.initial)
    )
    NFA(first, Set(last))
  }
}

/** A representation of an NFA.
  * @param initial
  *   The initial state.
  * @param accept
  *   The set of accepting states of the NFA.
  */
case class NFA(initial: NFAState, var accept: Set[NFAState])
    extends Automata[NFAState, Set] {

  /** Produce the NFA that represents the current NFA followed by a constant NFA
    * that matches a character.
    * @param that
    *   The character to be matched following the current NFA.
    * @return
    *   An NFA that represents the current NFA followed by a constant NFA that
    *   matches a character.
    */
  def ~>(that: Char): NFA = {
    val automata = NFA.const(that)
    ~>(automata)
  }

  /** Produce the NFA that represents the current NFA followed by another NFA.
    * @param that
    *   The NFA to be matched following the current NFA.
    * @return
    *   An NFA that represents the current NFA followed by another NFA.
    */
  def ~>(that: NFA): NFA = {
    this.accept.foreach(state => state.addTransition(epsilon, that.initial))
    NFA(this.initial, that.accept)
  }

  /** Produce an NFA that matches either the current NFA or a given NFA.
    * @param that
    *   The alternative to the current NFA.
    * @return
    *   An NFA that represents the current NFA followed by another NFA.
    */
  def <|>(that: NFA): NFA = {
    // Add a new initial state connected to both options with an epsilon transition.
    val first = NFAState()
    first.addTransition(epsilon, this.initial)
    first.addTransition(epsilon, that.initial)

    // Add a new accepting state connected to all the accepting states of both options with epsilon transitions.
    val last = NFAState()
    this.accept.foreach(state => state.addTransition(epsilon, last))
    that.accept.foreach(state => state.addTransition(epsilon, last))

    NFA(first, Set(last))
  }
}

/** Convert a regular expression to an NFA.
  * @param regEx
  *   The regular expression to be converted to an NFA.
  * @return
  *   An NFA that matches the same set of inputs as the given regular
  *   expression.
  */
def regexToNfa(regEx: RegEx): NFA = {
  regEx match {
    case Emp              => NFA.empty()
    case Ch(char)         => NFA.const(char)
    case Alt(left, right) => regexToNfa(left) <|> regexToNfa(right)
    case Star(inner)      => NFA.repeat(regexToNfa(inner))
    case Sequence(regExs) =>
      regExs.tail.foldLeft(regexToNfa(regExs.head))((nfa: NFA, regEx: RegEx) =>
        nfa ~> regexToNfa(regEx)
      )
  }
}
