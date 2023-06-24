module ReadWrite where
----

import Types
    ( Symbol,
      DFA,
      DFATransition(DFATransition),
      Transition(..),
      State,
      FiniteAutomata )
     
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List ( intercalate )


---- Helpers for reading an NFA and reading a DFA
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

parseNFAState :: String -> State
parseNFAState s
     | notElem '|' s = s
     | otherwise = error ("ERROR: State " ++ s ++ " can not" 
          ++ "contain the character '|' in its name")

parseDFAState :: String -> State
parseDFAState = id

parseSymbol :: String -> Symbol
parseSymbol = id

getAlphabet :: Set Transition -> Set Symbol
getAlphabet = Set.map tr_sym

getStates :: Set Transition -> Set State
getStates transitions = Set.union f t
     where
          f = Set.map tr_from transitions
          t = Set.map tr_to transitions

parseTransitionWith :: (String -> State) -> String -> Transition
parseTransitionWith parseStateFunction transition =
     case words transition of
          [p, c, q] -> Transition (parseStateFunction p) (parseSymbol c) (parseStateFunction q)
          [p, c] -> Transition (parseStateFunction p) (parseSymbol c) (parseStateFunction "")
          _ -> error "Invalid transition format"

getAllTransitionsWith :: (String -> Transition) -> [String] -> Set Transition
getAllTransitionsWith parseTransitionFunction transitions = 
     Set.fromList (map parseTransitionFunction transitions)


------ Reading an NFA
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

parseNFATransition :: String -> Transition
parseNFATransition = parseTransitionWith parseNFAState

getAllNFATransitions :: [String] -> Set Transition
getAllNFATransitions = getAllTransitionsWith parseNFATransition

processNFA :: [String] -> FiniteAutomata
processNFA fileContent = (allStates, alphabet, transitions, starting, accepting)
     where
          numStates : startState : finalStates : trans = fileContent
          starting = parseNFAState startState
          accepting = Set.fromList (map parseNFAState (words finalStates))
          transitions = getAllNFATransitions trans
          allStates = getStates transitions
          alphabet = getAlphabet transitions

------ Reading a DFA.
------ Similar to reading an NFA. Slight differences
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

parseDFATransition_ :: String -> Transition
parseDFATransition_ = parseTransitionWith parseDFAState

getAllDFATransitions :: [String] -> Set Transition
getAllDFATransitions = getAllTransitionsWith parseDFATransition_

processDFA :: [String] -> FiniteAutomata
processDFA fileContent = (allStates, alphabet, transitions, starting, accepting)
     where
          numStates : startState : finalStates : trans = fileContent
          starting = parseDFAState startState
          accepting = Set.fromList (map parseDFAState (words finalStates))
          transitions = getAllDFATransitions trans
          allStates = getStates transitions
          alphabet = getAlphabet transitions


------ PRINT DFA AFTER SUBSET CONSTRUCTION
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

showSymbol :: Symbol -> String
showSymbol = id

showState :: Symbol -> String
showState = id

----------------------------------------------------------------------------------------------------

showSetOfStates :: Set State -> String
showSetOfStates set = intercalate "|" (Set.toList set)

showStartingState :: Set State -> String
showStartingState = showSetOfStates

showFinalState :: Set (Set State) -> String
showFinalState qs = unwords (map showSetOfStates (Set.toList qs))

showTransitions :: Set DFATransition -> String
showTransitions transitions =
     unlines (map makeString (Set.toList transitions))
     where
          makeString (DFATransition r sym s) = showSetOfStates r ++ " " ++ 
               showSymbol sym ++ " " ++ showSetOfStates s

showDFA :: DFA -> [String]
showDFA (states, alphabet, transitions, start, finals) =
     [show (Set.size states) ++ "\n", showStartingState start ++ "\n", showFinalState finals 
          ++ "\n", showTransitions transitions]


------ Print Finite Automata
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

showFinalState_ :: Set State -> String
showFinalState_ qs = unwords (Set.toList qs)

showTransitions_ :: Set Transition -> String
showTransitions_ transitions =
     unlines (map makeString (Set.toList transitions))
     where
          makeString (Transition r sym s) = r ++ " " ++ showSymbol sym ++ " " ++ s

showDFA_ :: FiniteAutomata -> [String]
showDFA_ (states, alphabet, transitions, start, finals) =
     [show (Set.size states) ++ "\n", showState start ++ "\n", showFinalState_ finals
           ++ "\n", showTransitions_ transitions]

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------