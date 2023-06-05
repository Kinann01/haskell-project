module SubsetConstruction where

----

import Types
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List
import ReadWrite


----- SUBSET CONSTRUCTION ALGORITHM
-------------------------

-- Function move, takes a state p, a symbol c and a tranition and returns a set of 
-- states Q where every element of Q is a state that is reachable from p through c
move :: Set State -> Symbol -> Set Transition -> Set State
move q c transitions = Set.fromList [p | Transition q' sym p <- Set.toList transitions, sym == c, Set.member q' q]

-- Constructs a valid DFA transition
createDFATransition :: Set State -> Symbol -> Set Transition -> DFATransition
createDFATransition q c transitions = DFATransition q c (move q c transitions)

-- Creates a set of set of states from the provided NFA states
-- For example if NFA Q = {0, 1, 2}
-- DFA Q =  {{0}, {1}, {2}, {0, 1}, {0, 2}, {1, 2}, {0, 1, 2}}

getAllStates :: Set State -> Set (Set State)
getAllStates q = Set.fromList (map Set.fromList (delete [] (subsequences (Set.toList q))))

-- A function that checks if a current state is final
-- By definition, after subset construction, any set of states that contains a state q
-- which is orignally a final state in the NFA will be a final state in the DFA
isFinal :: Set State -> Set State -> Bool
isFinal states nfaFinalStates = not (Set.null (Set.intersection states nfaFinalStates))

-- Subset Construction
constructDFAFromNFA :: [String] -> DFA
constructDFAFromNFA input = (allStates, alphabet, transitions, starting, final)
     where
          -- NFA N = (QN, Alphabet, TransitionsN, q0, F2)
          (nfaStates, nfaAlphabet, nfaTransitions, nfaStart, nfaFinal) = processNFA input

          -- DFA D = (QD, Alphabet, TransitionsD, { q0 }, F2)
          starting = Set.singleton nfaStart
          allStates = getAllStates nfaStates
          alphabet = nfaAlphabet
          final = Set.filter (isFinal nfaFinal) allStates
          transitions = Set.fromList [createDFATransition state symbol nfaTransitions | state <- Set.toList allStates, symbol <- Set.toList alphabet]