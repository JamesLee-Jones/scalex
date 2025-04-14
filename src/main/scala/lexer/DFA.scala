package lexer

import scala.collection.mutable

/** A state in the DFA graph.
  *
  * @param transitions
  *   the outgoing transitions from the current state.
  */
class DFAState(var transitions: Map[Option[Char], Id[DFAState]] = Map.empty)
    extends Iterable[DFAState]
    with AutomataState[DFAState, Id] {

  /** Add a transition from the current state to a new state.
    *
    * @param input
    *   The possibly empty character associated with the transition. None
    *   represents an epsilon transition.
    * @param state
    *   The state to transition to.
    */
  def addTransition(maybeChar: Option[Char], state: DFAState): Unit = {
    transitions += (maybeChar -> Id(state))
  }

  override def iterator: Iterator[DFAState] = Iterator.single(this)
}

/** A representation of an DFA.
  * @param initial
  *   The initial state.
  * @param accept
  *   The set of accepting states of the DFA.
  */
case class DFA(initial: DFAState, var accept: Set[DFAState])
    extends Automata[DFAState, Id]

def epsilonClosure(states: Set[NFAState]): Set[NFAState] = {
  var result = states
  states.foreach(state =>
    if state.transitions.contains(epsilon) then
      result = result.union(epsilonClosure(state.transitions(epsilon)))
  )
  result
}

def nfaToDfa(nfa: NFA): DFA = {
  val visited = mutable.Map[Set[Int], DFAState]()

  // Declare the starting DFA state and set of accepting states.
  val dfaInitial = DFAState()
  val dfaAccepting = mutable.Set[DFAState]()

  // Compute the epsilon closure of the initial NFA state marking each as visited.
  val q0 = epsilonClosure(Set(nfa.initial))
  visited += (q0.map(state => state.id) -> dfaInitial)

  // Create a workQueue for traversing the NFA
  val workQueue = mutable.Queue[(Set[NFAState], DFAState)]()
  workQueue.enqueue((q0, dfaInitial))

  while (workQueue.nonEmpty) {
    val (q, representative) = workQueue.dequeue()

    var combined: Map[Option[Char], Set[NFAState]] =
      q.foldLeft(Map.empty)((map: Map[Option[Char], Set[NFAState]], state) =>
        unionWith(
          (m1: Set[NFAState], m2: Set[NFAState]) => m1.union(m2),
          map,
          state.transitions
        )
      )

    combined = combined.removed(epsilon)
    combined.foreach((char, states) => {
      val reachable = epsilonClosure(states)
      val reachableIdSet = reachable.map(state => state.id)

      if (!visited.contains(reachableIdSet)) {
        val newRepresentative = DFAState()
        representative.addTransition(char, newRepresentative)
        visited += (reachableIdSet -> newRepresentative)
        workQueue.enqueue((reachable, newRepresentative))
      } else {
        representative.addTransition(
          char,
          visited(reachable.map(state => state.id))
        )
      }
    })
  }

  DFA(dfaInitial, dfaAccepting.toSet)
}
