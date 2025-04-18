package lexer

import scala.collection.mutable

/** A state in the DFA graph.
  *
  * @param transitions
  *   the outgoing transitions from the current state.
  */
class DFAState(var transitions: Map[Option[Char], Id[DFAState]] = Map.empty)
    extends AutomataState[DFAState, Id] {

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

}

/** A representation of an DFA.
  * @param initial
  *   The initial state.
  * @param accept
  *   The set of accepting states of the DFA.
  */
case class DFA(initial: DFAState, var accept: Set[DFAState])
    extends Automata[DFAState, Id]

/** Get all states reachable from the current set of states by zero or more
  * epsilon transitions.
  * @param states
  *   Initial states.
  * @return
  *   All states reachable from the current set of states by zero or more
  *   epsilon transitions.
  */
def epsilonClosure(states: Set[NFAState]): Set[NFAState] = {
  var result = states
  // Compute the epsilon closure of every state reachable by an epsilon transition
  // and accumulate the result
  states.foreach(state =>
    if state.transitions.contains(epsilon) then
      result = result.union(epsilonClosure(state.transitions(epsilon)))
  )

  result
}

/** Convert an NFA to a DFA using the subset construction.
  * @param nfa
  *   The NFA to be converted.
  * @return
  *   A pair of a DFA which is equivalent to the input nfa and a map from the
  *   NFAs accepting IDs to the respective DFA accepting IDs.
  */
def nfaToDfa(nfa: NFA): (DFA, Map[Int, Set[Int]]) = {
  // A map from the sets of states seen so far to their representative DFA state.
  val visited = mutable.Map[Set[Int], DFAState]()

  // Declare the starting DFA state and set of accepting states.
  val dfaInitial = DFAState()
  val dfaAccepting = mutable.Set[DFAState]()

  // Compute the epsilon closure of the initial NFA state marking q0 as visited.
  val q0 = epsilonClosure(Set(nfa.initial))
  visited += (q0.map(state => state.id) -> dfaInitial)

  // Create a workQueue for traversing the NFA
  val workQueue = mutable.Queue[(Set[NFAState], DFAState)]()
  workQueue.enqueue((q0, dfaInitial))

  val nfaAcceptingIds = nfa.accept.map(state => state.id)

  // TODO: Can this be combined with visited for efficiency.
  val nfaToDfaAccept = mutable.Map[Int, Set[Int]]()

  while (workQueue.nonEmpty) {
    // Get the next element form the workQueue
    val (q, representative) = workQueue.dequeue()

    // q is a set of states, each with their own transition map. Combine these transition maps
    // so that each key in the combined map maps to the union of the set of states in each individual
    // transition map.
    var combined: Map[Option[Char], Set[NFAState]] =
      q.foldLeft(Map.empty)((map: Map[Option[Char], Set[NFAState]], state) =>
        unionWith(
          (m1: Set[NFAState], m2: Set[NFAState]) => m1.union(m2),
          map,
          state.transitions
        )
      )

    // Don't consider epsilon transitions
    combined = combined.removed(epsilon)

    combined.foreach((char, states) => {
      // Compute the epsilon closure of the states reachable from the current transition.
      val reachable = epsilonClosure(states)
      val reachableIdSet = reachable.map(state => state.id)

      if (!visited.contains(reachableIdSet)) {
        // If this set of states hasn't been visited before, create a new representative
        // for these states and add a transition from the last representative to the new one.
        val newRepresentative = DFAState()
        representative.addTransition(char, newRepresentative)
        visited += (reachableIdSet -> newRepresentative)
        workQueue.enqueue((reachable, newRepresentative))

        val reachableAccepting = reachableIdSet.intersect(nfaAcceptingIds)
        if (reachableAccepting.nonEmpty) {
          dfaAccepting += newRepresentative
          // TODO: Tidy this up
          reachableAccepting.foreach(id => nfaToDfaAccept.updateWith(id) {
            case Some(states) => Some(states + newRepresentative.id)
            case None         => Some(Set(newRepresentative.id))
          })
        }
      } else {
        // If the set of states has been visited before, add a transition to the existing representative
        // to show that it can be reached from the current state.
        representative.addTransition(
          char,
          visited(reachable.map(state => state.id))
        )
      }
    })
  }

  val dfa = DFA(dfaInitial, dfaAccepting.toSet)

  (dfa, nfaToDfaAccept.toMap)
}
