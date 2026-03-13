module Surd.TransitiveGroup

import Surd.Permutation

import Data.List

%default covering

------------------------------------------------------------------------
-- Transitive group database for small degrees
------------------------------------------------------------------------

||| A transitive group entry in the database.
public export
record TransGroup where
  constructor MkTransGroup
  tgDegree     : Nat          -- degree n (acts on {0..n-1})
  tgIndex      : Nat          -- Butler-McKay index
  tgName       : String       -- short name (e.g. "C5", "D5", "F20")
  tgOrder      : Integer      -- group order
  tgSolvable   : Bool         -- is the group solvable?
  tgGenerators : List Perm    -- generators

export
Show TransGroup where
  show tg = tgName tg ++ " (order " ++ show (tgOrder tg) ++ ", "
            ++ (if tgSolvable tg then "solvable" else "non-solvable") ++ ")"

------------------------------------------------------------------------
-- Degree 5 transitive groups
------------------------------------------------------------------------

||| C5: cyclic group of order 5.
||| Generator: (0 1 2 3 4)
export
c5 : TransGroup
c5 = MkTransGroup 5 1 "C5" 5 True
       [fromCycles 5 [[0, 1, 2, 3, 4]]]

||| D5: dihedral group of order 10.
||| Generators: (0 1 2 3 4) and (1 4)(2 3)
export
d5 : TransGroup
d5 = MkTransGroup 5 2 "D5" 10 True
       [ fromCycles 5 [[0, 1, 2, 3, 4]]
       , fromCycles 5 [[1, 4], [2, 3]]
       ]

||| F20: Frobenius group of order 20.
||| Generators: (0 1 2 3 4) and (1 2 4 3)
export
f20 : TransGroup
f20 = MkTransGroup 5 3 "F20" 20 True
        [ fromCycles 5 [[0, 1, 2, 3, 4]]
        , fromCycles 5 [[1, 2, 4, 3]]
        ]

||| A5: alternating group of order 60.
export
a5 : TransGroup
a5 = MkTransGroup 5 4 "A5" 60 False
       [ fromCycles 5 [[0, 1, 2]]
       , fromCycles 5 [[0, 1, 2, 3, 4]]
       ]

||| S5: symmetric group of order 120.
export
s5 : TransGroup
s5 = MkTransGroup 5 5 "S5" 120 False
       [ fromCycles 5 [[0, 1]]
       , fromCycles 5 [[0, 1, 2, 3, 4]]
       ]

||| All transitive groups of degree 5, ordered by index.
export
transitiveGroups5 : List TransGroup
transitiveGroups5 = [c5, d5, f20, a5, s5]

||| Look up a transitive group by degree and index.
export
lookupTransGroup : Nat -> Nat -> Maybe TransGroup
lookupTransGroup 5 idx = find (\tg => tgIndex tg == idx) transitiveGroups5
lookupTransGroup _ _ = Nothing
