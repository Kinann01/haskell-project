module Minimization where
----

import Types
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad
import Data.List

-- -- First we need to get rid of unreachable states 

-- -- A state q is un reachable if it has no ingoing transitions
-- -- We will also remove states that are non generating. i.e. if there is a state q that you reach and it has no outgoing transitions, we also remove it

notReachable :: State -> Set DFATransition_ -> State -> Bool
notReachable target transitions start =
     reach (Set.singleton start) Set.empty
     where
     reach current visited
          | Set.null current = False  -- Nothing to visit, return false
          | Set.member target current = True -- Reachable, return true
          | otherwise = reach next (Set.union visited current)  -- Proceed with next transition
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

reduce :: DFA_ -> DFA_
reduce (states, symbols, transitions , start, finalStates) = (newAllStates, symbols, newTransitions , start , newFinal)
     where
          newTransitions = removeUselessTransitions transitions start 
          newAllStates = removeUseless states newTransitions start
          newFinal = removeUseless finalStates newTransitions start

-- Given a set of transitions, a state and a symbol, it returns the next state through the provided symbol
nextTransition :: Set DFATransition_ -> State -> Symbol -> State
nextTransition transitions currentState c = 
     case Set.lookupMin (Set.filter (\(DFATransition_ p symbol q) -> p == currentState && symbol == c ) transitions )

     of
          Just(DFATransition_ _ _ nextState) -> nextState
          Nothing -> error ("Transition not found for state " ++ show currentState ++ " and symbol " ++ show c)

-- Given a state p, (a string) a list of symbols returns a state q that is reached from p through the provided symbols
followTransitions :: Set DFATransition_ -> State -> [Symbol] -> State
followTransitions transitions currentState alphabet = foldl' (nextTransition transitions) currentState alphabet

-- Given a dfa, and two states, we will determine if they are equivalent.
-- We use replicateM to get all the possible combinations of symbols we can get. 
-- Then we run both of the states through them and if they both reach a final state then they are equivalent.

areEquivalent :: DFA_ -> State -> State -> Bool
areEquivalent (states, symbols, transitions , _, finalStates) p q = all (\(pStates, qStates) -> Set.member pStates finalStates == Set.member qStates finalStates) (zip pStates qStates)
     where
          symbolLists = replicateM (Set.size states) (Set.toList symbols)
          pStates = map (followTransitions transitions p) symbolLists 
          qStates = map (followTransitions transitions q) symbolLists                    

-- Since we are able to determine equivalance between states, 
-- we have to partition the set of states into classes now where each class contains all the equivalent states.

findEquivalent :: DFA_ -> State -> Set State
findEquivalent dfa@(states, symbols, transitions , start, finalStates) p = 
     Set.fromList( p : [q | q <- Set.toList states, areEquivalent dfa q p] )

partitionClasses :: DFA_ ->  [Set State]
partitionClasses dfa@(states, _ ,_, _,_) = 
   nub [findEquivalent dfa p | p <- Set.toList states]

-- Now since we are able to partition all the states into equivalence classes. 
-- We accordingly update the name of the states, the transitions and construct the new DFA

renameClasses :: DFA_ -> Set State
renameClasses dfa = Set.fromList (map renameState (partitionClasses dfa))
     where 
          equivalenceClasses = partitionClasses dfa
          renameState s = concat (sort (Set.toList s))

findClass :: State -> [Set State] -> Set State
findClass state classes = head [c | c <- classes, Set.member state c]

updateTransitions :: DFA_ -> Set DFATransition_
updateTransitions dfa = Set.map update transitions
     where
          equivalenceClasses = partitionClasses dfa
          newState s = concat (sort (Set.toList (findClass s equivalenceClasses)))
          update (DFATransition_ p c q) = DFATransition_ (newState p) c (newState q)
          (states, symbols, transitions, start, finalStates) = dfa

minimizeDFA :: DFA_ -> DFA_
minimizeDFA dfa = (newStates, symbols, newTransitions, newStart, newFinalStates)
     where
          newStates = renameClasses dfa
          newTransitions = updateTransitions dfa
          classes = partitionClasses dfa
          newStart = concat (sort (Set.toList (findClass start classes)))
          newFinalStates = Set.filter (flip Set.member finalStates) newStates
          (states, symbols, transitions, start, finalStates) = dfa

--- Reduced done
--- Divide into equivalence class done
--- Main Minimize -> 

mainMinimize :: DFA_ -> DFA_
mainMinimize dfa = minimizeDFA (reduce dfa)