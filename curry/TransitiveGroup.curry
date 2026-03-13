--- Database of transitive subgroups of S_n for small n.
---
--- Groups are numbered following the Butler-McKay convention.
--- Currently only degree 5 is fully populated.
module TransitiveGroup
  ( TransitiveGroup(..)
  , tgC5, tgD5, tgF20, tgA5, tgS5
  , allTransitiveGroups5
  , tgSolvable
  , tgOrder
  , showTransitiveGroup
  ) where

import Permutation

--- A transitive subgroup description.
data TransitiveGroup = TransitiveGroup
  String    -- name (e.g. "C5", "D5", "F20", "A5", "S5")
  Int       -- order
  Bool      -- is solvable?
  [Perm]    -- generators

--- C5 = Z/5Z, cyclic group of order 5.
tgC5 :: TransitiveGroup
tgC5 = TransitiveGroup "C5" 5 True
  [permFromCycles 5 [[0, 1, 2, 3, 4]]]

--- D5 = dihedral group of order 10.
tgD5 :: TransitiveGroup
tgD5 = TransitiveGroup "D5" 10 True
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[1, 4], [2, 3]]
  ]

--- F20 = Frobenius group of order 20 (Z/5 ⋊ Z/4).
tgF20 :: TransitiveGroup
tgF20 = TransitiveGroup "F20" 20 True
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[1, 2, 4, 3]]
  ]

--- A5 = alternating group of order 60.
tgA5 :: TransitiveGroup
tgA5 = TransitiveGroup "A5" 60 False
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[0, 1, 2]]
  ]

--- S5 = symmetric group of order 120.
tgS5 :: TransitiveGroup
tgS5 = TransitiveGroup "S5" 120 False
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[0, 1]]
  ]

--- All transitive groups of degree 5 (smallest to largest).
allTransitiveGroups5 :: [TransitiveGroup]
allTransitiveGroups5 = [tgC5, tgD5, tgF20, tgA5, tgS5]

--- Is the group solvable?
tgSolvable :: TransitiveGroup -> Bool
tgSolvable (TransitiveGroup _ _ s _) = s

--- Order of the group.
tgOrder :: TransitiveGroup -> Int
tgOrder (TransitiveGroup _ o _ _) = o

--- Show.
showTransitiveGroup :: TransitiveGroup -> String
showTransitiveGroup (TransitiveGroup name ord solv _) =
  name ++ " (order " ++ show ord ++ ", "
  ++ (if solv then "solvable" else "non-solvable") ++ ")"

instance Eq TransitiveGroup where
  (TransitiveGroup n1 o1 _ _) == (TransitiveGroup n2 o2 _ _) =
    n1 == n2 && o1 == o2

instance Show TransitiveGroup where
  show = showTransitiveGroup
