||| Database of transitive subgroups of S_n for small n and runtime
||| computation for prime degrees via AGL(1,p).
|||
||| For prime p, the solvable transitive subgroups of S_p are exactly
||| Z/p ⋊ H where H ranges over subgroups of (Z/pZ)*, one per divisor
||| d of p-1. The non-solvable transitive groups are A_p and S_p.
module Surd.TransitiveGroup

import Surd.Permutation
import Surd.PrimeFactors
import Surd.Positive

import Data.List
import Data.SortedMap

%default covering

------------------------------------------------------------------------
-- Transitive group (degree-erased for runtime dispatch)
------------------------------------------------------------------------

||| A transitive group entry, not indexed by degree (enables runtime
||| construction for arbitrary primes). Generators are stored as raw
||| Int-list permutations to avoid dependently-typed Perm n at unknown n.
public export
record TransGroupRT where
  constructor MkTransGroupRT
  tgName       : String
  tgDegree     : Int
  tgOrder      : Integer
  tgSolvable   : Bool
  tgGenerators : List (List Int)  -- each generator as images [sigma(0), sigma(1), ...]
  tgMaximalSupergroups : List Int -- indices into the group list
  tgCompositionFactors : List Int -- prime factors of the composition series

export
Show TransGroupRT where
  show tg = tgName tg ++ " (order " ++ show (tgOrder tg) ++ ", "
            ++ (if tgSolvable tg then "solvable" else "non-solvable") ++ ")"

------------------------------------------------------------------------
-- Degree 5 transitive groups (fast path)
------------------------------------------------------------------------

||| A transitive group entry in the database.
public export
record TransGroup (n : Nat) where
  constructor MkTransGroup
  tgIndex      : Nat          -- Butler-McKay index
  tgName       : String       -- short name (e.g. "C5", "D5", "F20")
  tgOrder      : Integer      -- group order
  tgSolvable   : Bool         -- is the group solvable?
  tgGenerators : List (Perm n) -- generators

export
{n : Nat} -> Show (TransGroup n) where
  show tg = tgName tg ++ " (order " ++ show (tgOrder tg) ++ ", "
            ++ (if tgSolvable tg then "solvable" else "non-solvable") ++ ")"

||| C5: cyclic group of order 5.
export
c5 : TransGroup 5
c5 = MkTransGroup 1 "C5" 5 True
       [fromCycles 5 [[0, 1, 2, 3, 4]]]

||| D5: dihedral group of order 10.
export
d5 : TransGroup 5
d5 = MkTransGroup 2 "D5" 10 True
       [ fromCycles 5 [[0, 1, 2, 3, 4]]
       , fromCycles 5 [[1, 4], [2, 3]]
       ]

||| F20: Frobenius group of order 20.
export
f20 : TransGroup 5
f20 = MkTransGroup 3 "F20" 20 True
        [ fromCycles 5 [[0, 1, 2, 3, 4]]
        , fromCycles 5 [[1, 2, 4, 3]]
        ]

||| A5: alternating group of order 60.
export
a5 : TransGroup 5
a5 = MkTransGroup 4 "A5" 60 False
       [ fromCycles 5 [[0, 1, 2]]
       , fromCycles 5 [[0, 1, 2, 3, 4]]
       ]

||| S5: symmetric group of order 120.
export
s5 : TransGroup 5
s5 = MkTransGroup 5 "S5" 120 False
       [ fromCycles 5 [[0, 1]]
       , fromCycles 5 [[0, 1, 2, 3, 4]]
       ]

||| All transitive groups of degree 5, ordered by index.
export
transitiveGroups5 : List (TransGroup 5)
transitiveGroups5 = [c5, d5, f20, a5, s5]

||| Look up a transitive group of degree 5 by index.
export
lookupTransGroup5 : Nat -> Maybe (TransGroup 5)
lookupTransGroup5 idx = find (\tg => tgIndex tg == idx) transitiveGroups5

------------------------------------------------------------------------
-- Number theory helpers for runtime prime computation
------------------------------------------------------------------------

||| Modular exponentiation: base^exp mod m.
modExpRT : Integer -> Integer -> Integer -> Integer
modExpRT _ 0 _ = 1
modExpRT b e m =
  if mod e 2 == 0
    then let half = modExpRT b (div e 2) m in mod (half * half) m
    else mod (b * modExpRT b (e - 1) m) m

||| Primitive root modulo a prime p, via trial on candidates 2..p-1.
primitiveRootRT : Integer -> Integer
primitiveRootRT p =
  let phi = p - 1
      factors : List Integer
      factors = case positive (cast phi) of
                  Nothing  => []
                  Just pos => nub (map fst (factorise pos))
      isPrimRoot : Integer -> Bool
      isPrimRoot g = all (\q => modExpRT g (div phi q) p /= 1) factors
  in case find isPrimRoot [2 .. p - 1] of
       Just g  => g
       Nothing => 2  -- fallback (should not happen for primes)

||| Sorted positive divisors of n.
divisorsRT : Integer -> List Integer
divisorsRT n =
  let isqrtN = cast {to = Integer} (cast {to = Int} (sqrt (cast {to = Double} n)))
      small = filter (\d => d > 0 && mod n d == 0) [1 .. isqrtN]
      withLarge = concatMap (\d => if d * d == n then [d] else [d, div n d]) small
  in sort (nub withLarge)

||| Prime factorisation as a flat list with multiplicity.
primeFactorsWithMultRT : Integer -> List Integer
primeFactorsWithMultRT 1 = []
primeFactorsWithMultRT n =
  case positive (cast n) of
    Nothing  => []
    Just pos => concatMap (\pf => replicate (cast (snd pf)) (fst pf)) (factorise pos)

||| Factorial.
factorial : Integer -> Integer
factorial n = if n <= 1 then 1 else n * factorial (n - 1)

------------------------------------------------------------------------
-- Runtime transitive group computation for prime p
------------------------------------------------------------------------

||| All transitive subgroups of S_p for a prime p, computed at runtime
||| from the structure of AGL(1,p).
|||
||| The solvable subgroups are Z/p ⋊ H for each divisor d of p-1,
||| plus A_p and S_p (non-solvable for p >= 5).
export
transGroupsOfPrimeRT : Int -> List TransGroupRT
transGroupsOfPrimeRT p' =
  let p  = cast {to = Integer} p'
      g  = primitiveRootRT p
      ds = divisorsRT (p - 1)
      n  = p'

      -- Build one solvable group per divisor of p-1
      mkAffine : Integer -> TransGroupRT
      mkAffine d =
        let toInt : Integer -> Int
            toInt = cast
            trans = map (\i => toInt (mod (i + 1) p)) [0 .. p - 1]
            scaleFactor = modExpRT g (div (p - 1) d) p
            scale = map (\i => toInt (mod (scaleFactor * i) p)) [0 .. p - 1]
            gens = if d == 1 then [trans] else [trans, scale]
            gName =
              if d == 1 then "Z" ++ show p
              else if d == 2 then "D" ++ show p
              else if d == p - 1 then "AGL(1," ++ show p ++ ")"
              else "Z" ++ show p ++ ":Z" ++ show d
            cFactors : List Int
            cFactors = map (\x => cast {to = Int} x) (primeFactorsWithMultRT d ++ [p])
        in MkTransGroupRT gName (cast n) (p * d) True gens [] cFactors

      solvableGroups = map mkAffine ds

      toI : Integer -> Int
      toI = cast
      -- A_p
      transGen = map (\i => toI (mod (i + 1) p)) [0 .. p - 1]
      threeCycle = let base = map toI [0 .. p - 1]
                       -- (0 1 2): 0->1, 1->2, 2->0, rest fixed
                       updateAt : Nat -> Int -> List Int -> List Int
                       updateAt Z v (_ :: xs) = v :: xs
                       updateAt (S k) v (x :: xs) = x :: updateAt k v xs
                       updateAt _ _ xs = xs
                   in updateAt 0 1 (updateAt 1 2 (updateAt 2 0 base))
      ap = MkTransGroupRT ("A" ++ show p) (cast n)
             (div (factorial p) 2) (p' < 5) [transGen, threeCycle] [] []

      -- S_p
      twoSwap = let base = map toI [0 .. p - 1]
                    -- (0 1): swap 0 and 1
                    updateAt : Nat -> Int -> List Int -> List Int
                    updateAt Z v (_ :: xs) = v :: xs
                    updateAt (S k) v (x :: xs) = x :: updateAt k v xs
                    updateAt _ _ xs = xs
                in updateAt 0 1 (updateAt 1 0 base)
      sp = MkTransGroupRT ("S" ++ show p) (cast n)
             (factorial p) (p' < 4) [transGen, twoSwap] [] []

      allGroups = sortBy (\a, b => compare (tgOrder a) (tgOrder b))
                         (solvableGroups ++ [ap, sp])

      -- Assign maximal supergroups
      indexed : List (Int, TransGroupRT)
      indexed = zip (iterateN (length allGroups) (+ 1) 0) allGroups

      assignSuper : (Int, TransGroupRT) -> TransGroupRT
      assignSuper (myIdx, tg) =
        let myOrd = tgOrder tg
            cands : List (Int, Integer)
            cands = [(i, tgOrder cg) | (i, cg) <- indexed,
                      i /= myIdx, tgOrder cg > myOrd, mod (tgOrder cg) myOrd == 0]
            isMax : (Int, Integer) -> Bool
            isMax (_, superOrd) =
              not (any (\ci => let midOrd = snd ci in
                   midOrd > myOrd && midOrd < superOrd &&
                   mod superOrd midOrd == 0 && mod midOrd myOrd == 0) cands)
        in { tgMaximalSupergroups := [i | (i, _) <- filter isMax cands] } tg

  in map assignSuper indexed

------------------------------------------------------------------------
-- General interface: transitive groups of degree n
------------------------------------------------------------------------

||| All transitive subgroups of S_n (up to conjugacy) for the given degree.
||| For prime n, computes at runtime from AGL(1,n).
||| For n=5, also provides the Fin-indexed fast path.
export
transGroupsOfDegreeRT : Int -> List TransGroupRT
transGroupsOfDegreeRT n =
  if n >= 3 && isPrime (cast n)
    then transGroupsOfPrimeRT n
    else []

||| Look up a transitive group of degree n by name.
export
lookupByNameRT : Int -> String -> Maybe TransGroupRT
lookupByNameRT deg name =
  find (\tg => tgName tg == name) (transGroupsOfDegreeRT deg)

||| Look up solvable transitive groups of degree n by group order.
export
lookupByOrderRT : Int -> Integer -> List TransGroupRT
lookupByOrderRT deg ord =
  filter (\tg => tgOrder tg == ord) (transGroupsOfDegreeRT deg)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

||| Left scan: produces the list of accumulator values.
scanlHelper : (b -> a -> b) -> b -> List a -> List b
scanlHelper _ acc [] = [acc]
scanlHelper f acc (x :: xs) = acc :: scanlHelper f (f acc x) xs

------------------------------------------------------------------------
-- Composition series for prime-degree affine groups
------------------------------------------------------------------------

||| Composition series for a solvable affine subgroup of S_p.
|||
||| The group has the form Z/p ⋊ H where H is cyclic of order d.
||| The series descends through subgroups of H by removing one prime
||| factor at a time, then drops to {1}.
|||
||| Returns generator lists for each subgroup in the chain, descending
||| from G to {1}.
export
compositionSeriesPrimeRT : TransGroupRT -> Maybe (List (List (List Int)))
compositionSeriesPrimeRT tg =
  if not (tgSolvable tg) then Nothing
  else
    let p = cast {to = Integer} (tgDegree tg)
        g = primitiveRootRT p
        d = div (tgOrder tg) p
        dFactors = primeFactorsWithMultRT d
        -- Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
        dChain = scanlHelper (\acc, q => div acc q) d dFactors
        toI : Integer -> Int
        toI = cast
        transGen = map (\i => toI (mod (i + 1) p)) [0 .. p - 1]
        mkGens : Integer -> List (List Int)
        mkGens d' =
          if d' <= 1 then [transGen]
          else
            let sf = modExpRT g (div (p - 1) d') p
                scaleGen = map (\i => toI (mod (sf * i) p)) [0 .. p - 1]
            in [transGen, scaleGen]
    in Just (map mkGens dChain ++ [[]])
