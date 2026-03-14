{- |
Module      : Surd.Galois.AffineGroup
Description : Runtime computation of solvable transitive subgroups of \(S_p\) for primes \(p\)
Stability   : experimental
License     : BSD-3-Clause

For a prime \(p\), every transitive subgroup of \(S_p\) that is solvable
is conjugate to a subgroup of the affine group
\(\mathrm{AGL}(1,p) = \{ x \mapsto ax + b \mid a \in (\mathbb{Z}/p\mathbb{Z})^{\ast},\; b \in \mathbb{Z}/p\mathbb{Z} \}\).

Since \((\mathbb{Z}/p\mathbb{Z})^{\ast}\) is cyclic of order \(p-1\), the
solvable transitive subgroups are exactly
\(\mathbb{Z}/p\mathbb{Z} \rtimes H\) where \(H \le (\mathbb{Z}/p\mathbb{Z})^{\ast}\),
one for each divisor \(d\) of \(p-1\). The non-solvable transitive subgroups
(for the purpose of a polynomial solver) are simply \(A_p\) and \(S_p\).

This module computes these groups at runtime — no database needed — for
any prime \(p\).

== Reference

Galois, É. (1846). "Sur les conditions de résolubilité des équations
par radicaux." /J. Math. Pures Appl./ 11, 417–433.
-}
module Surd.Galois.AffineGroup (
    -- * Runtime group computation
    transGroupsOfPrime,
    affineSubgroup,

    -- * Composition series for affine subgroups
    compositionSeriesAffine,

    -- * Helpers
    primitiveRootP,
    divisors,
)
where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Galois.Permutation
import Surd.Galois.TransitiveGroup (TransitiveGroup (..))

------------------------------------------------------------------------
-- Primitive root and modular arithmetic
------------------------------------------------------------------------

-- | Modular exponentiation: @modExp b e m@ = \(b^e \bmod m\).
modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp b e m
    | even e = let half = modExp b (e `div` 2) m in (half * half) `mod` m
    | otherwise = (b * modExp b (e - 1) m) `mod` m

-- | Find a primitive root modulo a prime \(p\).
primitiveRootP :: Integer -> Integer
primitiveRootP p =
    let phi = p - 1
        factors = map fst (factorise (fromInteger phi :: Positive))
        isPrimRoot g = all (\q -> modExp g (phi `div` q) p /= 1) factors
     in case filter isPrimRoot [2 .. p - 1] of
            (g' : _) -> g'
            [] -> error "primitiveRootP: no primitive root"

-- | All positive divisors of a positive integer, sorted.
divisors :: Integer -> [Integer]
divisors n =
    sort $ concatMap (\d -> if d * d == n then [d] else [d, n `div` d])
           [d | d <- [1 .. isqrt n], n `mod` d == 0]
  where
    isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

------------------------------------------------------------------------
-- AGL(1,p) subgroups
------------------------------------------------------------------------

{- | Build the transitive subgroup \(\mathbb{Z}/p \rtimes H\) where
\(H \le (\mathbb{Z}/p\mathbb{Z})^{\ast}\) has order \(d\) (a divisor
of \(p-1\)).

The generators are:

* __Translation__: \(t(x) = x + 1 \pmod{p}\), a \(p\)-cycle.
* __Scaling__: \(s(x) = g^{(p-1)/d} \cdot x \pmod{p}\), where \(g\) is
  a primitive root mod \(p\). This generates a cyclic subgroup of order
  \(d\) in \((\mathbb{Z}/p\mathbb{Z})^{\ast}\).

The resulting group has order \(p \cdot d\) and is always solvable
(since \(\mathbb{Z}/p\) is normal with cyclic quotient \(H\)).
-}
affineSubgroup :: Integer -> Integer -> Integer -> TransitiveGroup
affineSubgroup p g d =
    let n = fromIntegral p :: Int
        pd = p * d
        -- Translation: x -> x + 1 mod p
        trans = fromMapping [fromIntegral ((i + 1) `mod` p) | i <- [0 .. p - 1]]
        -- Scaling: x -> g^((p-1)/d) · x mod p
        scaleFactor = modExp g ((p - 1) `div` d) p
        scale = fromMapping [fromIntegral ((scaleFactor * i) `mod` p) | i <- [0 .. p - 1]]
        gens
            | d == 1 = [trans]
            | otherwise = [trans, scale]
        name = groupName p d
        compFactors = compositionFactorsAffine p d
     in TransitiveGroup
            { tgName = name
            , tgDegree = n
            , tgOrder = pd
            , tgGenerators = gens
            , tgSolvable = True
            , tgMaximalSupergroups = [] -- filled in by transGroupsOfPrime
            , tgCompositionFactors = compFactors
            }

-- | Human-readable name for the affine subgroup of order \(p \cdot d\).
groupName :: Integer -> Integer -> String
groupName p 1 = "Z" ++ show p
groupName p 2 = "D" ++ show p
groupName p d
    | d == p - 1 = "AGL(1," ++ show p ++ ")"
    | otherwise = "Z" ++ show p ++ ":Z" ++ show d

-- | Composition factors (prime orders of cyclic quotients) for the
-- affine subgroup of order \(p \cdot d\).
--
-- The composition series is:
-- \(\mathbb{Z}/p \rtimes H \trianglerighteq \mathbb{Z}/p \trianglerighteq \{1\}\)
-- where \(H\) is cyclic of order \(d\), contributing prime factors of \(d\),
-- and the bottom step contributes \(p\).
compositionFactorsAffine :: Integer -> Integer -> [Int]
compositionFactorsAffine p d =
    let dFactors = primeFactorsWithMult d
     in map fromIntegral (dFactors ++ [p])

-- | Prime factorisation with multiplicity, as a flat list.
-- E.g. @primeFactorsWithMult 12 = [2, 2, 3]@.
primeFactorsWithMult :: Integer -> [Integer]
primeFactorsWithMult 1 = []
primeFactorsWithMult n =
    let fs = factorise (fromInteger n :: Positive)
     in concatMap (\(q, e) -> replicate e q) fs

------------------------------------------------------------------------
-- Full list of transitive subgroups for prime degree
------------------------------------------------------------------------

{- | All transitive subgroups of \(S_p\) for a prime \(p\), sorted by
increasing order.

Returns the solvable subgroups (one for each divisor of \(p-1\), living
in \(\mathrm{AGL}(1,p)\)), plus \(A_p\) and \(S_p\) at the top. The
maximal-supergroup lattice is computed from the divisibility relation.

For \(p = 5\) this recovers the standard 5-group lattice
\(C_5 \subset D_5 \subset F_{20}\) plus \(A_5, S_5\).
-}
transGroupsOfPrime :: Int -> [TransitiveGroup]
transGroupsOfPrime p' =
    let p = fromIntegral p' :: Integer
        g = primitiveRootP p
        ds = sort (divisors (p - 1))
        -- Build solvable groups (one per divisor of p-1)
        solvableGroups = [affineSubgroup p g d | d <- ds]
        -- A_p and S_p
        n = p'
        ap = TransitiveGroup
            { tgName = "A" ++ show p
            , tgDegree = n
            , tgOrder = product [1 .. p] `div` 2
            , tgGenerators =
                [ fromCycles n [[0 .. n - 1]] -- n-cycle
                , fromCycles n [[0, 1, 2]]    -- 3-cycle
                ]
            , tgSolvable = p' < 5  -- A_2, A_3 are solvable; A_p for p>=5 is not
            , tgMaximalSupergroups = []
            , tgCompositionFactors = if p' < 5 then [fromIntegral p `div` 2] else []
            }
        sp = TransitiveGroup
            { tgName = "S" ++ show p
            , tgDegree = n
            , tgOrder = product [1 .. p]
            , tgGenerators =
                [ fromCycles n [[0 .. n - 1]] -- n-cycle
                , fromCycles n [[0, 1]]       -- transposition
                ]
            , tgSolvable = p' < 4  -- S_2, S_3 are solvable; S_p for p>=4 is not (but 4 is not prime)
            , tgMaximalSupergroups = []
            , tgCompositionFactors = []
            }
        -- All groups, sorted by order
        allGroups = sortBy (comparing tgOrder) (solvableGroups ++ [ap, sp])
        -- Build index map: order -> position
        indexed = zip [0 :: Int ..] allGroups
        -- Compute maximal supergroups
        withSupergroups = map (assignSupergroups indexed) indexed
     in withSupergroups

-- | For each group, find its maximal supergroups in the list.
-- A group G is a maximal supergroup of H if G properly contains H
-- and there is no intermediate group K with H < K < G.
assignSupergroups :: [(Int, TransitiveGroup)] -> (Int, TransitiveGroup) -> TransitiveGroup
assignSupergroups allIndexed (myIdx, tg) =
    let myOrder = tgOrder tg
        -- Groups that could contain us (larger order, our order divides theirs)
        candidates = [(i, g) | (i, g) <- allIndexed, i /= myIdx, tgOrder g > myOrder, tgOrder g `mod` myOrder == 0]
        -- A supergroup is maximal if no other candidate has order strictly between
        isMaximal (_, superG) =
            not $ any (\(j, midG) -> j /= myIdx && tgOrder midG > myOrder && tgOrder midG < tgOrder superG && tgOrder superG `mod` tgOrder midG == 0 && tgOrder midG `mod` myOrder == 0) candidates
        maxSupers = [i | (i, g) <- candidates, isMaximal (i, g)]
     in tg{tgMaximalSupergroups = maxSupers}

------------------------------------------------------------------------
-- Composition series for affine subgroups
------------------------------------------------------------------------

{- | Compute the composition series for a solvable affine subgroup of
\(S_p\) as a list of generating sets, descending from \(G\) to \(\{1\}\).

The series is:
\[
  \mathbb{Z}/p \rtimes H \supset \mathbb{Z}/p \rtimes H' \supset \cdots
  \supset \mathbb{Z}/p \supset \{1\}
\]

where each step removes one prime factor from \(H\).
-}
compositionSeriesAffine :: Integer -> Integer -> Integer -> Maybe [[Perm]]
compositionSeriesAffine p g d =
    let n = fromIntegral p :: Int
        trans = fromCycles n [[0 .. n - 1]]
        -- Subgroup chain: d, d/q1, d/q1/q2, ..., 1
        dFactors = primeFactorsWithMult d
        -- Build chain of divisors by progressively dividing by prime factors
        dChain = scanl (\acc q -> acc `div` q) d dFactors
        -- For each divisor d', build the scaling generator
        mkGens d'
            | d' <= 1 = [trans]
            | otherwise =
                let scaleFactor = modExp g ((p - 1) `div` d') p
                    scale = fromMapping [fromIntegral ((scaleFactor * i) `mod` p) | i <- [0 .. p - 1]]
                 in [trans, scale]
     in Just (map mkGens dChain ++ [[]])

