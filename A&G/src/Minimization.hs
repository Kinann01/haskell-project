module Minimization where

----
import Types
    ( DFATransition_(DFATransition_), 
    DFA_, 
    State, 
    Symbol )

import Data.Set (Set)
import Data.Set qualified as Set

import Data.List ( foldl', 
               nub,
               sort)

-- -- First we need to get rid of unreachable states 
-- -- A state q is un reachable if it has no ingoing transitions
-- -- We will also remove states that are non generating. i.e. 
-- if there is a state q that you reach and it has no outgoing transitions, we also remove it (If it is not a final state)

nextStates :: Set DFATransition_ -> State -> Set State
nextStates transitions state = Set.fromList [s | DFATransition_ q _ s <- Set.toList transitions, q == state]

previousStates :: Set DFATransition_ -> State -> Set State
previousStates transitions state = Set.fromList [q | DFATransition_ q _ s <- Set.toList transitions, s == state]

transitionsFrom :: Set DFATransition_ -> State -> Set DFATransition_
transitionsFrom transitions state = Set.filter (\(DFATransition_ q _ _) -> q == state) transitions

transitionsTo :: Set DFATransition_ -> State -> Set DFATransition_
transitionsTo transitions state = Set.filter (\(DFATransition_ _ _ s) -> s == state) transitions

reachableStates :: Set DFATransition_ -> Set State -> Set State
reachableStates transitions states =
     if states == newStates then states else reachableStates transitions newStates
     where
          newStates = states `Set.union` Set.unions (Set.map (nextStates transitions) states)

generatingStates :: Set DFATransition_ -> Set State -> Set State
generatingStates transitions states =
     if states == newStates then states else generatingStates transitions newStates
     where
          newStates = Set.union states (Set.unions (Set.map (previousStates transitions) states))

reduce :: DFA_ -> DFA_
reduce (states, symbols, transitions, start, finalStates) = 
          (newStates, symbols, newTransitions, start, newFinalStates)
     where
          reachable = reachableStates transitions (Set.singleton start)
          generating = generatingStates transitions finalStates
          newStates = Set.intersection reachable  generating
          newTransitions = Set.filter (\(DFATransition_ q _ s) -> 
                    Set.member q newStates && Set.member s newStates) transitions
          newFinalStates = Set.intersection finalStates newStates

------ ||||||||
------ ||||||||
------ ||||||||
------ REDUCTION DONE
------ ||||||||
------ ||||||||
------ ||||||||

-- Given a set of transitions, a state and a symbol, it returns the next state through the provided symbol
-- In order to support non complete transitions, it will return Maybe a State.
-- If there is no transition from a state through a symbol c it will return nothing

nextTransition :: Set DFATransition_ -> Maybe State -> Symbol -> Maybe State
nextTransition transitions maybeCurrentState c =
     case maybeCurrentState 

     of
          Just currentState -> case Set.lookupMin (Set.filter (\(DFATransition_ p symbol q) -> p 
                                   == currentState && symbol == c ) transitions )

                              of
                                   Just(DFATransition_ _ _ nextState) -> Just nextState
                                   Nothing -> Nothing
          Nothing -> Nothing

-- Given a state p, (a string) a list of symbols returns a state q that is reached from p through the provided symbols
followTransitions :: Set DFATransition_ -> Maybe State -> [Symbol] -> Maybe State
followTransitions transitions currentState alphabet = 
          foldl' (nextTransition transitions) currentState alphabet

--
-- checks if two states are equivalent
-- Two states are equivalent iff, for any possible input string, 
-- the automaton ends up in a final state from one state if and only if 
-- it ends up in a final state from the other state.

areEquivalent :: DFA_ -> Set (State, State) -> Maybe State -> Maybe State -> Bool
areEquivalent dfa@(states, symbols, transitions, _, finalStates) visited maybeP maybeQ
          -- (one is a final state and the other is not), they are not equivalent.
          | fmap (`Set.member` finalStates) maybeP /= fmap (`Set.member` finalStates) maybeQ = False

          -- Otherwise, if they are either both final or both non-final states,
          | otherwise = case (maybeP, maybeQ) of

                         -- If both states are not nothing,
                         -- we have to check if we have already visited this pair 
                         -- to avoid loops. If visited, 
                         -- we already know they are equivalent, so we return 'True'. 
                         -- If not, we recursively check for all symbols, 
                         -- if the transitions from both states lead to equivalent states.

                         (Just p, Just q) -> Set.member (p, q) visited || 
                              and [ areEquivalent dfa (Set.insert (p, q) visited)
                                        (followTransitions transitions (Just p) [symbol])
                                        (followTransitions transitions (Just q) [symbol])
                                   | symbol <- Set.toList symbols ]

                              -- If either state is Nothing,
                              -- we return True because there is nowhere to go from either
                         _ -> True


-- Since we are able to determine equivalance between states, 
-- we have to partition the set of states into classes now where each class contains all the equivalent states.

findEquivalent :: DFA_ -> State -> Set State
findEquivalent dfa@(states, symbols, transitions , start, finalStates) p =
     Set.fromList ( p : [q | q <- Set.toList states, areEquivalent dfa Set.empty (Just q) (Just p)] )

partitionClasses :: DFA_ ->  [Set State]
partitionClasses dfa@(states,_ ,_, _,_) =
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

-- a method that updates the transitions based on the equivalence classes. It constructs new transitions. 
updateTransitions :: DFA_ -> Set DFATransition_
updateTransitions dfa@(states, symbols, transitions, start, finalStates) = Set.map update transitions
     where
          equivalenceClasses = partitionClasses dfa
          newState s = concat (sort (Set.toList (findClass s equivalenceClasses)))
          update (DFATransition_ p c q) = DFATransition_ (newState p) c (newState q)

--We get the final states based on the new equivalent classes we have formed and on the old final states
getFinalStates :: [Set State] -> Set State -> Set State
getFinalStates classes oldFinal = Set.fromList [renameState s | s <- classes, any (`Set.member` oldFinal) s]
    where renameState = concat . sort . Set.toList

-- final
minimizeDFA :: DFA_ -> DFA_
minimizeDFA dfa@(states, symbols, transitions, start, finalStates) = (newStates, symbols, newTransitions, newStart, newFinalStates)
     where
          classes = partitionClasses dfa
          newStates = renameClasses dfa
          newTransitions = updateTransitions dfa
          newStart = concat (sort (Set.toList (findClass start classes)))
          newFinalStates = getFinalStates classes finalStates


------ ||||||||
------ ||||||||
------ ||||||||
------ Equivalence DONE
------ ||||||||
------ ||||||||
------ ||||||||


--- Reduce then Minimize
mainMinimize :: DFA_ -> DFA_
mainMinimize dfa = minimizeDFA (reduce dfa)