module Types where
----

import Data.Set (Set)
import Data.Set qualified as Set

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Data types for state or symbol
type State = String
type Symbol = String

-- INPUT NFA/DFA TYPE
type FiniteAutomaton = (Set State, Set Symbol, Set Transition, State, Set State)

-- Transition from a state p to a state q through a symbol c
data Transition = Transition { tr_from :: State, tr_sym :: Symbol, tr_to :: State }
    deriving (Eq, Show, Ord)

----------------------------
-- Type is used only to define a valid DFA after subset contstruction
-- DFA = (Q, alphabet, transition function, q0, F)
type DFA = (Set (Set State), Set Symbol, Set DFATransition, Set State, Set (Set State))

data DFATransition = DFATransition { tr_from_ :: Set State, tr_sym_ :: Symbol, tr_to_ :: Set State }
    deriving (Eq, Show, Ord)