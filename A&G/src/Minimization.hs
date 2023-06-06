module Minimization where
----

import Types
import Data.Set (Set)
import Data.Set qualified as Set

-- -- First we need to get rid of unreachable states 

-- -- A state q is un reachable if it has no ingoing transitions
-- -- We will also remove states that are non generating. i.e. if there is a state q that you reach and it has no outgoing transitions, we also remove it

notReachable :: State -> Set DFATransition_ -> State -> Bool
notReachable target transitions start =
     bfs (Set.singleton start) Set.empty
     where
     bfs current visited
          | Set.null current = False  -- Nothing to visit, return false
          | Set.member target current = True -- Reachable, return true
          | otherwise = bfs next (Set.union visited current)  -- Proceed with next transition
          where
          next = Set.fromList [s | DFATransition_ q _ s <- Set.toList transitions, Set.member q current, not (Set.member s visited)]

removeUselessTransitions :: Set DFATransition_ -> State -> Set DFATransition_
removeUselessTransitions transitions start =
    Set.filter useful transitions
     where
          useful (DFATransition_ q _ s) = -- Remove non generating transitions so a transition q c "" is useless
               s /= "" && notReachable q transitions start -- Remove unreachable states

removeUseless :: Set State -> Set DFATransition_ -> State -> Set State -- Deals with the states
removeUseless states transitions start = 
     Set.filter canReach states
     where 
          canReach q = notReachable q transitions start



-- testing funciton
newDFA :: DFA_ -> DFA_
newDFA (allStates, alphabet, transitions, starting, final) = (allStates, alphabet, newTransitions, starting, newFinal)
     where
          newTransitions = removeUselessTransitions transitions starting 
          newAllStates = removeUseless allStates newTransitions starting
          newFinal = removeUseless final newTransitions starting

--- Now we deal with the equivalent states
-- TODO