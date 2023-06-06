module ReadWrite where
----

import Types
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List

-- Parse state and symbol - both represented as a string
parseState :: String -> State
parseState s = s

parseSymbol :: String -> Symbol
parseSymbol s = s

------------------------------------------
------------------------------------------
------------------------------------------

-- Helper function where given a string in the form "state symbol state", construts a transition of the valid type 
parseTransition :: String -> Transition
parseTransition transition =
     case words transition of
          [p, c, q] -> Transition (parseState p) (parseSymbol c) (parseState q)
          [p, c] -> Transition (parseState p) (parseSymbol c) (parseState "")
          _ -> error "Invalid transition format"

-- Helper function that takes the input NFA as a list of string and returns a set of all the transitions
getAllTransitions :: [String] -> Set Transition
getAllTransitions fileContent = Set.fromList (map parseTransition (drop 3 fileContent))

-- Helper function that returns a set of symbols in which the symbols is the alphabet used in the finite automaton
getAlphabet :: Set Transition -> Set Symbol
getAlphabet = Set.map (\(Transition _ symbol _) -> symbol)

-- Helper functions that takes a set of transitions and returns a set of states which contain all the states defined in the FA
getStates :: Set Transition -> Set State
getStates = Set.map (\(Transition p _ _) -> p)

-- Main functon that creates the NFA using the previously mentioned helper functions
processNFA :: [String] -> NFA
processNFA fileContent = (allStates, alphabet, transitions, starting, accepting)
     where
          starting = parseState (fileContent !! 1)
          accepting = Set.singleton (parseState (fileContent !! 2))
          transitions = getAllTransitions fileContent
          allStates = getStates transitions
          alphabet = getAlphabet transitions


-- Reading a DFA.
-- Idea is similar to reading an NFA.

getAlphabet_ :: Set DFATransition_ -> Set Symbol
getAlphabet_ = Set.map (\(DFATransition_ _ symbol _) -> symbol)

getStates_ :: Set DFATransition_ -> Set State
getStates_ = Set.map (\(DFATransition_ p _ _) -> p)

parseTransition_ :: String -> DFATransition_
parseTransition_ transition =
     case words transition of
          [p, c, q] -> DFATransition_ (parseState p) (parseSymbol c) (parseState q)
          [p, c] -> DFATransition_ (parseState p) (parseSymbol c) (parseState "")
          _ -> error "Invalid transition format"

getAllTransitions_ :: [String] -> Set DFATransition_
getAllTransitions_ transitions = Set.fromList (map parseTransition_ transitions)

processDFA :: [String] -> DFA_
processDFA fileContent = (allStates, alphabet, transitions, starting, accepting)
     where
          numStates : startState : finalStates : trans = fileContent
          starting = parseState startState
          accepting = Set.fromList (map parseState (words finalStates))
          transitions = getAllTransitions_ trans
          allStates = getStates_ transitions
          alphabet = getAlphabet_ transitions



------ PRINT DFA AFTER SUBSET CONSTRUCTION
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

showSetOfStates :: Set State -> String
showSetOfStates set = intercalate "" (Set.toList set)

showSymbol :: Symbol -> String
showSymbol = id

showState :: Symbol -> String
showState = id

showStartingState :: Set State -> String
showStartingState = showSetOfStates

showFinalState :: Set (Set State) -> String
showFinalState qs = unwords (map showSetOfStates (Set.toList qs))

showTransitions :: Set DFATransition -> String
showTransitions transitions =
     unlines (map makeString (Set.toList transitions))
     where
          makeString (DFATransition r sym s) = showSetOfStates r ++ " " ++ showSymbol sym ++ " " ++ showSetOfStates s

showDFA :: DFA -> [String]
showDFA (states, alphabet, transitions, start, finals) =
     [show (Set.size states) ++ "\n", showStartingState start ++ "\n", showFinalState finals ++ "\n", showTransitions transitions]

----------------
----------------
-- Print DFA_

showFinalState_ :: Set State -> String
showFinalState_ qs = unwords (Set.toList qs)

showTransitions_ :: Set DFATransition_ -> String
showTransitions_ transitions =
     unlines (map makeString (Set.toList transitions))
     where
          makeString (DFATransition_ r sym s) = r ++ " " ++ showSymbol sym ++ " " ++ s

showDFA_ :: DFA_ -> [String]
showDFA_ (states, alphabet, transitions, start, finals) =
     [show (Set.size states) ++ "\n", showState start ++ "\n", showFinalState_ finals ++ "\n", showTransitions_ transitions]

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------