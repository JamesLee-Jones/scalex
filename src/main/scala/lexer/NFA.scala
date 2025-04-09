package lexer

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

val epsilon = None

object UniqueIdGenerator {
  private val counter = new AtomicInteger(0)
  def nextId(): Int = counter.getAndIncrement()
}

class State(var transitions: Map[Option[Char], Set[State]] = Map.empty) {
  val id: Int = UniqueIdGenerator.nextId()

  def addTransition(input: Char, state: State): Unit = addTransition(Some(input), state)

  def addTransition(input: Option[Char], state: State): Unit = {
    transitions = transitions.updatedWith(input) {
      case Some(states)  => Some(states + state)
      case None          => Some(Set(state))
    }
  }
}

object NFA {
  def empty(): NFA = {
    val start = State()
    val end = State()
    start.addTransition(epsilon, start)
    NFA(start, Set(end))
  }

  def const(char: Char): NFA = {
    val start: State = new State()
    val end = new State()
    start.addTransition(char, end)
    NFA(start, Set(end))
  }

  def repeat(automata: NFA): NFA = {
    val first = new State()
    val last = new State()
    first.addTransition(epsilon, automata.initial)
    first.addTransition(epsilon, last)
    automata.accept.foreach(state => state.addTransition(epsilon, last))
    automata.accept.foreach(state => state.addTransition(epsilon, automata.initial))
    NFA(first, Set(last))
  }
}

class NFA(val initial: State, var accept: Set[State]) {
  def ~>(that: Char): NFA = {
    val automata = NFA.const(that)
    ~>(automata)
  }

  def ~>(that: NFA): NFA = {
    this.accept.foreach(state => state.addTransition(epsilon, that.initial))
    NFA(this.initial, that.accept)
  }

  def <|>(that: NFA): NFA = {
    val first = new State()
    first.addTransition(epsilon, this.initial)
    first.addTransition(epsilon, that.initial)
    val last = new State()
    this.accept.foreach(state => state.addTransition(epsilon, last))
    that.accept.foreach(state => state.addTransition(epsilon, last))
    NFA(first, Set(last))
  }

  def transitionCount(): Int = {
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

    transitions
  }

  def printTransitions(): Unit = {
    val visited = mutable.Set[Int]()
    val queue = mutable.Queue[State]()

    queue.enqueue(this.initial)
    visited += this.initial.id

    while (queue.nonEmpty) {
      val state = queue.dequeue()
      state.transitions.foreach {(char, nextStates) =>
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