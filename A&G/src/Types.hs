module Types where
----

import Data.Set (Set)
import Data.Set qualified as Set


------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- data types for state or symbol
type State = String
type Symbol = String

-- To maintain correct mathematical notations, two types of DFA were defined, one for subset construction and one for a normal DFA
-- The difference is after subset construction, the DFA is mainly all sets of sets
-- for example the starting state will not be q0 anymore, it will be {q0}

-- INPUT NFA TYPE
-- NFA = (Q, alphabet, transition function, q0, F)
type NFA = (Set State, Set Symbol, Set Transition, State, Set State)

-- Transition from a state p to a state q through a character c
data Transition = Transition { p :: State, c :: Symbol, q :: State }
    deriving (Eq, Show, Ord)


-- DFA type after subset construction
-- DFA = (Q, alphabet, transition function, q0, F)
type DFA = (Set (Set State), Set Symbol, Set DFATransition, Set State, Set (Set State))

data DFATransition = DFATransition { r :: Set State, sym :: Symbol, s :: Set State }
    deriving (Eq, Show, Ord)

----------------------------
-- DFA type for normal DFA
-- INPUT DFA TYPE
type DFA_ = (Set State, Set Symbol, Set DFATransition_, State, Set State)

data DFATransition_ = DFATransition_ { rs :: State, sym_:: Symbol, sr :: State }
    deriving (Eq, Show, Ord)
    


