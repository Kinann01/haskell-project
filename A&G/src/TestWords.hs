module TestWords where
----

import Types
    ( Transition (Transition),
     FiniteAutomata,
     State,
     Symbol )

import Data.Set (Set)
import Data.Set qualified as Set

import Data.List ( find )
import Control.Monad ( foldM )

-- simulate the DFA on a string
simulateDFA :: FiniteAutomata -> String -> Bool
simulateDFA (states, alphabet, transitionFunction, startState, finalStates) string =
     let symbols = map (:[]) string 
         maybeFinalState = foldM (applyTransition transitionFunction) startState symbols
     in
          any (\s -> Set.member s finalStates) maybeFinalState

-- look up the next state in the transition function
applyTransition :: Set Transition -> State -> Symbol -> Maybe State
applyTransition transitionFunction currentState symbol =
     let maybeTransition = find (\(Transition r sym s) -> r == currentState && sym == symbol) 
          (Set.toList transitionFunction)
     
     in
          (\(Transition _ _ nextState) -> nextState) <$> maybeTransition