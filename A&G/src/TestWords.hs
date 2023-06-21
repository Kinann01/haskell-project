module TestWords where
----

import Types
    ( DFATransition_(DFATransition_),
     DFA_,
     State,
     Symbol )

import Data.Set (Set)
import Data.Set qualified as Set

import Data.List ( find )
import Control.Monad ( foldM )

-- simulate the DFA on a string
simulateDFA_ :: DFA_ -> String -> Bool
simulateDFA_ (states, alphabet, transitionFunction, startState, finalStates) string =
     let symbols = map (:[]) string 
         maybeFinalState = foldM (applyTransition_ transitionFunction) startState symbols
     in
          any (\s -> Set.member s finalStates) maybeFinalState

-- look up the next state in the transition function
applyTransition_ :: Set DFATransition_ -> State -> Symbol -> Maybe State
applyTransition_ transitionFunction currentState symbol =
     let maybeTransition = find (\(DFATransition_ r sym s) -> r == currentState && sym == symbol) (Set.toList transitionFunction)
     
     in
          (\(DFATransition_ _ _ nextState) -> nextState) <$> maybeTransition