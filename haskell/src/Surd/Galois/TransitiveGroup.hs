{- |
Module      : Surd.Galois.TransitiveGroup
Description : Database of transitive subgroups of \(S_n\) for small \(n\)
Stability   : experimental
License     : BSD-3-Clause

= Background

A /transitive subgroup/ of \(S_n\) is a permutation group acting on
\(\{1, \ldots, n\}\) whose action has a single orbit.  The transitive
subgroups of \(S_n\) (up to conjugacy) classify the possible Galois groups
of irreducible degree-\(n\) polynomials over \(\mathbb{Q}\).

For Galois group identification via Stauduhar's resolvent method, we need
the lattice of transitive subgroups together with generators, orders,
solvability flags, and maximal-supergroup relationships.

== Numbering convention

Groups are numbered following the Butler–McKay convention (1983), which
is also the standard in GAP, Magma, and the LMFDB.  Within a given
degree, \(T_1\) is the smallest group and \(T_k\) (where \(k\) is the
total number of conjugacy classes) is \(S_n\) itself.

== Degree-5 transitive group lattice

@
          S5  (T5, order 120)
         \/  \\
        A5    \\   (T4, order 60)
              F20  (T3, order 20, solvable)
               |
              D5   (T2, order 10, solvable)
               |
              C5   (T1, order  5, solvable)
@

Lines denote containment (subgroup below, supergroup above).
\(F_{20} = \mathbb{Z}/5 \rtimes \mathbb{Z}/4\) is the Frobenius group,
the largest /solvable/ transitive subgroup of \(S_5\).

== Solvability and radical solvability

A finite group \(G\) is /solvable/ if it admits a composition series

\[
  G = G_0 \trianglerighteq G_1 \trianglerighteq \cdots \trianglerighteq G_k = \{1\}
\]

where each quotient \(G_i / G_{i+1}\) is cyclic of prime order.
By Galois's theorem, a polynomial is solvable by radicals if and only if
its Galois group is solvable.  The composition factors determine the
radical tower: a cyclic quotient of order \(p\) corresponds to adjoining
a \(p\)-th root.  For degree 5, the groups \(C_5\), \(D_5\), and
\(F_{20}\) are solvable, while \(A_5\) and \(S_5\) are not.

== References

* Butler, G. & McKay, J. (1983). "The transitive groups of degree up to
  eleven." /Communications in Algebra/ 11(8), 863–911.
  DOI: 10.1080\/00927878308822884

* Hulpke, A. (2005). "Constructing transitive permutation groups."
  /J. Symbolic Computation/ 39(1), 1–30.
  DOI: 10.1016\/j.jsc.2004.08.002

* Cox, D. A. (2012). /Galois Theory/, 2nd ed. Wiley.
  DOI: 10.1002\/9781118218457
-}
module Surd.Galois.TransitiveGroup (
    -- * Transitive group type
    TransitiveGroup (..),

    -- * Database queries
    transGroupsOfDegree,
    transGroupByOrder,

    -- * Solvability
    isSolvable,

    -- * Composition series
    compositionSeries,
)
where

import Data.List (nub, sort)
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Galois.Permutation

{- | A transitive subgroup of \(S_n\), with metadata for Galois group
identification and radical tower construction.
-}
data TransitiveGroup = TransitiveGroup
    { tgName :: !String
    -- ^ Human-readable name, e.g. @\"C5\"@, @\"D5\"@, @\"F20\"@, @\"A5\"@, @\"S5\"@.
    , tgDegree :: !Int
    {- ^ The degree \(n\) of the symmetric group \(S_n\) in which this
    group acts (equivalently, the number of roots of the polynomial).
    -}
    , tgOrder :: !Integer
    -- ^ The order \(|G|\) of the group.
    , tgGenerators :: ![Perm]
    {- ^ A generating set for the group, given as permutations in cycle
    notation on the symbols \(\{0, 1, \ldots, n-1\}\).  For example,
    \(D_5 = \langle (0\;1\;2\;3\;4),\; (1\;4)(2\;3) \rangle\).
    -}
    , tgSolvable :: !Bool
    {- ^ Whether the group is solvable.  A group is solvable iff it has a
    composition series with cyclic (prime order) quotients, which by
    Galois's theorem is equivalent to the corresponding polynomial being
    solvable by radicals.
    -}
    , tgMaximalSupergroups :: ![Int]
    {- ^ Indices (0-based) into the list returned by 'transGroupsOfDegree'
    of groups that contain this group as a maximal transitive subgroup.
    This encodes the Hasse diagram of the transitive group lattice and
    drives the Stauduhar descent strategy: begin with \(S_n\) (the top
    of the lattice) and descend via resolvent polynomials, testing each
    maximal subgroup until the Galois group is identified.  Empty for
    \(S_n\) itself (the top element).
    -}
    , tgCompositionFactors :: ![Int]
    {- ^ For solvable groups: the prime orders of the cyclic quotients in a
    composition series

    \[
    G = G_0 \trianglerighteq G_1 \trianglerighteq \cdots
      \trianglerighteq G_k = \{1\}
    \]

    listed from top to bottom.  For example, @[5, 2, 2]@ for \(F_{20}\)
    means \(F_{20} / D_5 \cong \mathbb{Z}/2\),
    \(D_5 / C_5 \cong \mathbb{Z}/2\), \(C_5 / \{1\} \cong \mathbb{Z}/5\)
    (reading the factors in reverse gives the radical tower from innermost
    to outermost root extraction).  Empty for non-solvable groups.
    -}
    }
    deriving (Show, Eq)

{- | Test whether a transitive group is solvable (\(\equiv\) 'tgSolvable').
A solvable Galois group means the polynomial is solvable by radicals,
i.e.\ its roots can be expressed using nested \(p\)-th roots over
\(\mathbb{Q}\).
-}
isSolvable :: TransitiveGroup -> Bool
isSolvable = tgSolvable

{- | All transitive subgroups of \(S_n\) (up to conjugacy) for the given
degree \(n\), sorted by increasing group order.

For prime \(n\), the groups are computed at runtime from the structure
of \(\mathrm{AGL}(1,n)\). For degree 5, the hard-coded database is
used (as a fast path). For unsupported composite degrees, returns the
empty list.
-}
transGroupsOfDegree :: Int -> [TransitiveGroup]
transGroupsOfDegree 5 = degree5Groups
transGroupsOfDegree n
    | n >= 3 && isPrime (fromIntegral n) = transGroupsOfPrimeRT n
    | otherwise = []

-- | Test whether an integer is prime (trial division, sufficient for
-- degrees we care about).
isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n < 4 = True
    | even n = False
    | otherwise = all (\d -> n `mod` d /= 0) [3, 5 .. isqrt n]
  where
    isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

{- | Find the transitive group(s) of the given degree and order.
There may be more than one conjugacy class with the same order
(though this does not occur in degree 5).
-}
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
-- Runtime prime group computation
------------------------------------------------------------------------

-- | Modular exponentiation (internal).
modExpRT :: Integer -> Integer -> Integer -> Integer
modExpRT _ 0 _ = 1
modExpRT b e m
    | even e = let half = modExpRT b (e `div` 2) m in (half * half) `mod` m
    | otherwise = (b * modExpRT b (e - 1) m) `mod` m

-- | Primitive root modulo a prime (internal).
primitiveRootRT :: Integer -> Integer
primitiveRootRT p =
    let phi = p - 1
        factors = nub $ map fst (factorise (fromInteger phi :: Positive))
        isPrimRoot g = all (\q -> modExpRT g (phi `div` q) p /= 1) factors
     in head (filter isPrimRoot [2 .. p - 1])

-- | Sorted positive divisors of n (internal).
divisorsRT :: Integer -> [Integer]
divisorsRT n =
    let isqrt = floor . (sqrt :: Double -> Double) . fromIntegral
        small = [d | d <- [1 .. isqrt n], n `mod` d == 0]
     in nub $ sort $ concatMap (\d -> if d * d == n then [d] else [d, n `div` d]) small

-- | Prime factorisation as a flat list with multiplicity (internal).
primeFactorsWithMultRT :: Integer -> [Integer]
primeFactorsWithMultRT 1 = []
primeFactorsWithMultRT n =
    concatMap (\(q, e) -> replicate e q) (factorise (fromInteger n :: Positive))

{- | All transitive subgroups of \(S_p\) for a prime \(p\), computed at
runtime from the structure of \(\mathrm{AGL}(1,p)\).

The solvable subgroups are \(\mathbb{Z}/p \rtimes H\) for each divisor
\(d\) of \(p-1\), plus \(A_p\) and \(S_p\) (non-solvable for \(p \ge 5\)).
-}
transGroupsOfPrimeRT :: Int -> [TransitiveGroup]
transGroupsOfPrimeRT p' =
    let p = fromIntegral p' :: Integer
        g = primitiveRootRT p
        ds = divisorsRT (p - 1)
        n = p'
        -- Build one solvable group per divisor of p-1
        mkAffine d =
            let trans = fromMapping [fromIntegral ((i + 1) `mod` p) | i <- [0 .. p - 1]]
                scaleFactor = modExpRT g ((p - 1) `div` d) p
                scale = fromMapping [fromIntegral ((scaleFactor * i) `mod` p) | i <- [0 .. p - 1]]
                gens = if d == 1 then [trans] else [trans, scale]
                gName
                    | d == 1     = "Z" ++ show p
                    | d == 2     = "D" ++ show p
                    | d == p - 1 = "AGL(1," ++ show p ++ ")"
                    | otherwise  = "Z" ++ show p ++ ":Z" ++ show d
                cFactors = map fromIntegral (primeFactorsWithMultRT d ++ [p])
             in TransitiveGroup
                    { tgName = gName
                    , tgDegree = n
                    , tgOrder = p * d
                    , tgGenerators = gens
                    , tgSolvable = True
                    , tgMaximalSupergroups = []
                    , tgCompositionFactors = cFactors
                    }
        solvableGroups = map mkAffine ds
        -- A_p
        ap = TransitiveGroup
            { tgName = "A" ++ show p
            , tgDegree = n
            , tgOrder = product [1 .. p] `div` 2
            , tgGenerators =
                [ fromCycles n [[0 .. n - 1]]
                , fromCycles n [[0, 1, 2]]
                ]
            , tgSolvable = p' < 5
            , tgMaximalSupergroups = []
            , tgCompositionFactors = []
            }
        -- S_p
        sp = TransitiveGroup
            { tgName = "S" ++ show p
            , tgDegree = n
            , tgOrder = product [1 .. p]
            , tgGenerators =
                [ fromCycles n [[0 .. n - 1]]
                , fromCycles n [[0, 1]]
                ]
            , tgSolvable = p' < 4
            , tgMaximalSupergroups = []
            , tgCompositionFactors = []
            }
        allGroups = sort $ solvableGroups ++ [ap, sp]
        -- Assign maximal supergroups
        indexed = zip [0 :: Int ..] allGroups
        assignSuper (myIdx, tg) =
            let myOrd = tgOrder tg
                cands = [(i, tgOrder cg) | (i, cg) <- indexed, i /= myIdx, tgOrder cg > myOrd, tgOrder cg `mod` myOrd == 0]
                isMax (_, superOrd) =
                    not $ any (\(_, midOrd) -> midOrd > myOrd && midOrd < superOrd && superOrd `mod` midOrd == 0 && midOrd `mod` myOrd == 0) cands
             in tg{tgMaximalSupergroups = [i | (i, _) <- cands, isMax (i, tgOrder (snd (indexed !! i)))]}
     in map assignSuper indexed

instance Ord TransitiveGroup where
    compare a b = compare (tgOrder a) (tgOrder b)

------------------------------------------------------------------------
-- Composition series
------------------------------------------------------------------------

{- | For a solvable transitive group, return the composition series as a
list of subgroups (each given by its generating set), descending from
\(G\) to the trivial group \(\{1\}\):

\[
  G = G_0 \supset G_1 \supset \cdots \supset G_k = \{1\}
\]

Each consecutive quotient \(G_i / G_{i+1}\) is cyclic of prime order
(matching the entries of 'tgCompositionFactors').  This chain drives
radical tower descent: to express the roots in radicals, adjoin a
\(|G_i / G_{i+1}|\)-th root at each step, working from the bottom of
the chain upward.

Returns 'Nothing' for non-solvable groups.

For degree-5 groups, uses the hard-coded series. For prime-degree groups,
computes the series from the affine structure.
-}
compositionSeries :: TransitiveGroup -> Maybe [[Perm]]
compositionSeries tg
    | not (tgSolvable tg) = Nothing
    | otherwise = case tgName tg of
        -- Fast path: degree-5 hard-coded
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
        -- General prime-degree affine groups
        _ | isPrime (fromIntegral (tgDegree tg)) ->
                compositionSeriesPrime tg
          | otherwise -> Nothing

{- | Composition series for a solvable affine subgroup of \(S_p\).

The group has the form \(\mathbb{Z}/p \rtimes H\) where \(H\) is cyclic
of order \(d\). The series descends through subgroups of \(H\) by
removing one prime factor at a time, then drops to \(\{1\}\).
-}
compositionSeriesPrime :: TransitiveGroup -> Maybe [[Perm]]
compositionSeriesPrime tg =
    let p = fromIntegral (tgDegree tg) :: Integer
        n = tgDegree tg
        g = primitiveRootRT p
        d = tgOrder tg `div` p
        dFactors = primeFactorsWithMultRT d
        -- Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
        dChain = scanl (\acc q -> acc `div` q) d dFactors
        trans = fromCycles n [[0 .. n - 1]]
        mkGens d'
            | d' <= 1 = [trans]
            | otherwise =
                let sf = modExpRT g ((p - 1) `div` d') p
                    scale = fromMapping [fromIntegral ((sf * i) `mod` p) | i <- [0 .. p - 1]]
                 in [trans, scale]
     in Just (map mkGens dChain ++ [[]])

