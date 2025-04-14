package lexer

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

val epsilon = None

/** A class for generating unique state IDs.
  */
object UniqueIdGenerator {
  private val counter = new AtomicInteger(0)
  def nextId(): Int = counter.getAndIncrement()
}

/** A state in the NFA graph.
  *
  * @param transitions
  *   the outgoing transitions from the current state.
  */
class State(var transitions: Map[Option[Char], Set[State]] = Map.empty) {
  val id: Int = UniqueIdGenerator.nextId()

  /** Add a transition from the current state to a new state.
    * @param input
    *   The character associated with the transition.
    * @param state
    *   The state to transition to.
    */
  def addTransition(char: Char, state: State): Unit =
    addTransition(Some(char), state)

  /** Add a transition from the current state to a new state.
    *
    * @param input
    *   The possibly empty character associated with the transition. None
    *   represents an epsilon transition.
    * @param state
    *   The state to transition to.
    */
  def addTransition(maybeChar: Option[Char], state: State): Unit = {
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
    val start = State()
    val end = State()
    start.addTransition(char, end)
    NFA(start, Set(end))
  }

  /** Produce an NFA that can be repeated zero or more times.
    * @param automata
    *   The NFA that can be repeated.
    * @return
    *   An NFA that can be repeated zero or more times.
    */
  def repeat(automata: NFA): NFA = {
    val first = State()
    val last = State()
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
case class NFA(initial: State, var accept: Set[State]) {

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
    val first = State()
    first.addTransition(epsilon, this.initial)
    first.addTransition(epsilon, that.initial)

    // Add a new accepting state connected to all the accepting states of both options with epsilon transitions.
    val last = State()
    this.accept.foreach(state => state.addTransition(epsilon, last))
    that.accept.foreach(state => state.addTransition(epsilon, last))

    NFA(first, Set(last))
  }

  // TODO(JLJ): This can be refactored to use UniqueIdGenerator. The largest Id generated is equal to the number of edges.

  /** Get the number of nodes and transitions in the current NFA.
    * @return
    *   The number of nodes and transitions in the current NFA.
    */
  def nodeAndTransitionCount(): (Int, Int) = {
    // TODO(JLJ): Factor out duplication with printTransitions.
    val visited = mutable.Set[Int]()
    val queue = mutable.Queue[State]()
    var transitions = 0

    queue.enqueue(this.initial)
    visited += this.initial.id

    while (queue.nonEmpty) {
      val state = queue.dequeue()
      state.transitions.foreach { (char, nextStates) =>
        val label = char.map(_.toString).getOrElse("ε")
        nextStates.foreach { next =>
          transitions += 1
          if (!visited.contains(next.id)) {
            visited += next.id
            queue.enqueue(next)
          }
        }
      }
    }

    (visited.size, transitions)
  }

  /** Print the transitions in the current NFA.
    */
  def printTransitions(): Unit = {
    val visited = mutable.Set[Int]()
    val queue = mutable.Queue[State]()

    queue.enqueue(this.initial)
    visited += this.initial.id

    while (queue.nonEmpty) {
      val state = queue.dequeue()
      state.transitions.foreach { (char, nextStates) =>
        val label = char.map(_.toString).getOrElse("ε")
        nextStates.foreach { next =>
          println(s"${state.id} --[$label]--> ${next.id}")
          if (!visited.contains(next.id)) {
            visited += next.id
            queue.enqueue(next)
          }
        }
      }
    }
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
