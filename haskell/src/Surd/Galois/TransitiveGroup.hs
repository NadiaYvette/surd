{- | Database of transitive subgroups of S_n for small n.

For Galois group identification via Stauduhar descent, we need the
lattice of transitive subgroups: generators, orders, solvability,
and maximal-subgroup relationships.

Currently supports degree 5 (5 transitive groups) and degree 6
(16 transitive groups). The numbering follows the standard GAP/Magma
convention (T1 = smallest, T_last = S_n).
-}
module Surd.Galois.TransitiveGroup (
    TransitiveGroup (..),
    transGroupsOfDegree,
    transGroupByOrder,
    isSolvable,
    compositionSeries,
)
where

import Surd.Galois.Permutation

{- | A transitive subgroup of S_n, with metadata for Galois group
identification and radical tower construction.
-}
data TransitiveGroup = TransitiveGroup
    { tgName :: !String
    , tgDegree :: !Int
    , tgOrder :: !Integer
    , tgGenerators :: ![Perm]
    , tgSolvable :: !Bool
    , tgMaximalSupergroups :: ![Int]
    {- ^ Indices (0-based) into the degree's transitive group list of groups
    that contain this group as a maximal subgroup. Used for Stauduhar
    descent: start from S_n and descend via resolvents.
    -}
    , tgCompositionFactors :: ![Int]
    {- ^ For solvable groups: the cyclic factor sizes in a composition series
    (e.g., [5, 2] for D5 = Z/5 ⋊ Z/2). Empty for non-solvable.
    -}
    }
    deriving (Show)

-- | Is this transitive group solvable?
isSolvable :: TransitiveGroup -> Bool
isSolvable = tgSolvable

-- | All transitive groups of a given degree (sorted by order).
transGroupsOfDegree :: Int -> [TransitiveGroup]
transGroupsOfDegree 5 = degree5Groups
transGroupsOfDegree _ = []

-- | Find the transitive group(s) of given degree and order.
transGroupByOrder :: Int -> Integer -> [TransitiveGroup]
transGroupByOrder deg ord =
    [g | g <- transGroupsOfDegree deg, tgOrder g == ord]

------------------------------------------------------------------------
-- Degree 5 transitive groups
------------------------------------------------------------------------

-- The five transitive subgroups of S_5, in order of increasing order:
--
-- T1 = C5  (Z/5Z)       order  5   solvable
-- T2 = D5  (Dih(5))     order 10   solvable
-- T3 = F20 (Z/5 ⋊ Z/4) order 20   solvable
-- T4 = A5  (Alt(5))     order 60   not solvable
-- T5 = S5  (Sym(5))     order 120  not solvable
--
-- Labelling of roots: {0, 1, 2, 3, 4}.
-- Generators use the standard (0-indexed) permutation representation.

degree5Groups :: [TransitiveGroup]
degree5Groups =
    [ -- T1: C5 = ⟨(0 1 2 3 4)⟩
      TransitiveGroup
        { tgName = "C5"
        , tgDegree = 5
        , tgOrder = 5
        , tgGenerators = [fromCycles 5 [[0, 1, 2, 3, 4]]]
        , tgSolvable = True
        , tgMaximalSupergroups = [1] -- D5
        , tgCompositionFactors = [5]
        }
    , -- T2: D5 = ⟨(0 1 2 3 4), (1 4)(2 3)⟩
      TransitiveGroup
        { tgName = "D5"
        , tgDegree = 5
        , tgOrder = 10
        , tgGenerators =
            [ fromCycles 5 [[0, 1, 2, 3, 4]]
            , fromCycles 5 [[1, 4], [2, 3]]
            ]
        , tgSolvable = True
        , tgMaximalSupergroups = [2] -- F20
        , tgCompositionFactors = [5, 2]
        }
    , -- T3: F20 = GA(1,5) = ⟨(0 1 2 3 4), (1 2 4 3)⟩
      -- The Frobenius group of order 20 = Z/5 ⋊ Z/4.
      -- (1 2 4 3) acts as multiplication by 2 mod 5 on {0,1,2,3,4}.
      TransitiveGroup
        { tgName = "F20"
        , tgDegree = 5
        , tgOrder = 20
        , tgGenerators =
            [ fromCycles 5 [[0, 1, 2, 3, 4]]
            , fromCycles 5 [[1, 2, 4, 3]]
            ]
        , tgSolvable = True
        , tgMaximalSupergroups = [3, 4] -- A5, S5
        , tgCompositionFactors = [5, 2, 2]
        }
    , -- T4: A5 = ⟨(0 1 2 3 4), (0 1 2)⟩
      TransitiveGroup
        { tgName = "A5"
        , tgDegree = 5
        , tgOrder = 60
        , tgGenerators =
            [ fromCycles 5 [[0, 1, 2, 3, 4]]
            , fromCycles 5 [[0, 1, 2]]
            ]
        , tgSolvable = False
        , tgMaximalSupergroups = [4] -- S5
        , tgCompositionFactors = []
        }
    , -- T5: S5 = ⟨(0 1 2 3 4), (0 1)⟩
      TransitiveGroup
        { tgName = "S5"
        , tgDegree = 5
        , tgOrder = 120
        , tgGenerators =
            [ fromCycles 5 [[0, 1, 2, 3, 4]]
            , fromCycles 5 [[0, 1]]
            ]
        , tgSolvable = False
        , tgMaximalSupergroups = [] -- top
        , tgCompositionFactors = []
        }
    ]

------------------------------------------------------------------------
-- Composition series
------------------------------------------------------------------------

{- | For a solvable transitive group, return a composition series as
a list of subgroups (as generator lists), from the whole group down
to the trivial group. Each consecutive pair has cyclic quotient.

Returns Nothing for non-solvable groups.
-}
compositionSeries :: TransitiveGroup -> Maybe [[Perm]]
compositionSeries tg
    | not (tgSolvable tg) = Nothing
    | otherwise = case tgName tg of
        "C5" -> Just [tgGenerators tg, []]
        "D5" ->
            Just
                [ tgGenerators tg
                , [fromCycles 5 [[0, 1, 2, 3, 4]]] -- C5
                , []
                ]
        "F20" ->
            Just
                [ tgGenerators tg
                , -- D5 inside F20

                    [ fromCycles 5 [[0, 1, 2, 3, 4]]
                    , fromCycles 5 [[1, 4], [2, 3]]
                    ]
                , [fromCycles 5 [[0, 1, 2, 3, 4]]] -- C5
                , []
                ]
        _ -> Nothing
