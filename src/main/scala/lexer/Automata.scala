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

final case class Id[A](value: A) extends Iterable[A] {
  // Provide an iterator that yields the single element.
  override def iterator: Iterator[A] = Iterator.single(value)
}

trait AutomataState[T <: AutomataState[T, F], F[_]] {
  val id: Int = UniqueIdGenerator.nextId()
  var transitions: Map[Option[Char], F[T]]

  /** Add a transition from the current state to a new state.
    *
    * @param input
    *   The character associated with the transition.
    * @param state
    *   The state to transition to.
    */
  def addTransition(input: Char, state: T): Unit =
    addTransition(Some(input), state)

  /** Add a transition from the current state to a new state.
    *
    * @param input
    *   The possibly empty character associated with the transition. None
    *   represents an epsilon transition.
    * @param state
    *   The state to transition to.
    */
  def addTransition(input: Option[Char], state: T): Unit
}

extension [T <: AutomataState[T, F], F[_]](iterable: Iterable[T])
  private def ids(): Set[Int] = iterable.map(state => state.id).toSet

trait Automata[S <: AutomataState[S, F], F[A] <: Iterable[A]] {
  def initial: S
  def accept: Set[S]

  /** A function for traversing an automata that takes an action on every stat
    * and transition.
    * @param stateAction
    *   The action to be taken on each state.
    * @param stateState
    *   The state required by stateAction.
    * @param transitionAction
    *   The action to be taken on each transition.
    * @param transitionState
    *   The state required by transitionAction.
    * @tparam T
    *   The type of the state required by stateAction.
    * @tparam R
    *   The type of the state required by transitionAction
    */
  def traverse[T, R](
      stateAction: (S, T) => Unit,
      stateState: T,
      transitionAction: ((S, Option[Char], S), R) => Unit,
      transitionState: R
  ): Unit = {
    val visited = mutable.Set[Int]()
    val queue = mutable.Queue[S]()

    queue.enqueue(initial)
    visited += initial.id

    while (queue.nonEmpty) {
      val state = queue.dequeue()
      stateAction(state, stateState)
      state.transitions.foreach { (char, nextStates) =>
        nextStates.foreach { next =>
          transitionAction((state, char, next), transitionState)
          if (!visited.contains(next.id)) {
            visited += next.id
            queue.enqueue(next)
          }
        }
      }
    }
  }

  /** Get the number of nodes and transitions in the current Automata.
    *
    * @return
    *   The number of nodes and transitions in the current Automata.
    */
  def nodeAndTransitionCount(): (Int, Int) = {
    var nodeCount = 0
    var transitionCount = 0
    traverse((_, _) => nodeCount += 1, (), (_, _) => transitionCount += 1, ())

    (nodeCount, transitionCount)
  }

  def nodeIdRange(): (Int, Int) = {
    var minimum = Int.MaxValue
    var maximum = 0
    traverse(
      (s, _) => { minimum = minimum.min(s.id); maximum = maximum.max(s.id) },
      (),
      (_, _) => (),
      ()
    )
    println(minimum)
    println(maximum)
    (minimum, maximum)
  }

  /** Print the transitions in the current Automata.
    */
  def printTransitions(): Unit = {
    val printTransition = (state: S, char: Option[Char], next: S) =>
      println(
        s"${state.id} --[${char.map(_.toString).getOrElse("ε")}]--> ${next.id}"
      )
    traverse(
      (_, _) => (),
      (),
      (transition, _) =>
        printTransition(transition._1, transition._2, transition._3),
      ()
    )
  }
}
