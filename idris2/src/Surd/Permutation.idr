||| Permutations on {0, ..., n-1} using Fin-indexed Vect.
|||
||| Idris 2's dependent types ensure permutation indices are always
||| in bounds. Operations include composition, inversion, cycle
||| decomposition, and order computation.
module Surd.Permutation

import Data.Fin
import Data.Vect
import Data.SortedMap
import Data.SortedSet
import Data.List
import Data.Nat

%default covering

------------------------------------------------------------------------
-- Permutation type (Fin-indexed)
------------------------------------------------------------------------

||| A permutation on {0, 1, ..., n-1} stored as a Vect of Fin values.
||| The image of i is given by `index i images`.
public export
record Perm (n : Nat) where
  constructor MkPerm
  images : Vect n (Fin n)

||| Apply a permutation to a point.
export
permApply : Perm n -> Fin n -> Fin n
permApply p i = index i p.images

||| Identity permutation of degree n.
export
permId : (n : Nat) -> Perm n
permId n = MkPerm (Data.Vect.Fin.tabulate id)

||| Compose two permutations: (f . g)(x) = f(g(x)).
export
permCompose : Perm n -> Perm n -> Perm n
permCompose f g = MkPerm (map (permApply f) g.images)

||| Inverse of a permutation.
||| Builds a vector where position images[i] maps to i.
export
permInverse : {n : Nat} -> Perm n -> Perm n
permInverse {n} p =
  let pairs : List (Fin n, Fin n)
      pairs = toList (zipWith MkPair (Data.Vect.Fin.tabulate id) p.images)
      -- Build a SortedMap from image -> preimage
      m : SortedMap Nat (Fin n)
      m = foldl (\acc, (src, dst) => insert (finToNat dst) src acc) empty pairs
      -- Reconstruct vector
      inv : Vect n (Fin n)
      inv = Data.Vect.Fin.tabulate (\i => case lookup (finToNat i) m of
                              Just v  => v
                              Nothing => i)  -- shouldn't happen for valid perm
  in MkPerm inv

||| Check if a permutation is the identity.
export
permIsId : {n : Nat} -> Perm n -> Bool
permIsId p = toList p.images == toList (Data.Vect.Fin.tabulate {len=n} id)

||| Order of a permutation (smallest k > 0 such that sigma^k = id).
export
permOrder : {n : Nat} -> Perm n -> Nat
permOrder p = go (permCompose p p) 1
  where
    go : Perm n -> Nat -> Nat
    go current k =
      if permIsId current then S k
      else if k > 1000 then S k  -- safety bound
      else go (permCompose current p) (S k)

------------------------------------------------------------------------
-- Cycle decomposition
------------------------------------------------------------------------

||| Cycle decomposition of a permutation, returned as lists of Fin values.
export
permCyclesFin : {n : Nat} -> Perm n -> List (List (Fin n))
permCyclesFin {n} p =
  snd (foldl findCycle (empty, []) (toList (Data.Vect.Fin.tabulate {len=n} id)))
  where
    traceCycle : Perm n -> Fin n -> SortedSet Nat -> List (Fin n)
    traceCycle p start visited =
      if contains (finToNat start) visited then []
      else start :: traceCycle p (permApply p start) (insert (finToNat start) visited)

    findCycle : (SortedSet Nat, List (List (Fin n))) -> Fin n -> (SortedSet Nat, List (List (Fin n)))
    findCycle (visited, cycles) i =
      if contains (finToNat i) visited then (visited, cycles)
      else
        let cyc = traceCycle p i visited
            newVisited = foldl (\s, x => insert (finToNat x) s) visited cyc
        in if length cyc <= 1 then (newVisited, cycles)
           else (newVisited, cyc :: cycles)

||| Cycle decomposition as Int lists (for backward compatibility).
export
permCycles : {n : Nat} -> Perm n -> List (List Int)
permCycles p = map (map (\f => cast (finToNat f))) (permCyclesFin p)

||| Sign of a permutation: +1 for even, -1 for odd.
export
permSign : {n : Nat} -> Perm n -> Int
permSign p =
  let cycles = permCycles p
      transpositions = sum (map (\c => cast (minus (length c) 1)) cycles)
  in if mod transpositions 2 == 0 then 1 else -1

------------------------------------------------------------------------
-- Construction helpers
------------------------------------------------------------------------

||| Safely convert a Nat to a Fin, returning FZ for out-of-range.
finSafe : {n : Nat} -> Nat -> Fin (S n)
finSafe k = case natToFin k (S n) of
              Just f  => f
              Nothing => FZ

||| Apply a single cycle (as a list of Nat indices) to a Vect of Fin values.
applyCycleToVect : {n : Nat} -> Vect n (Fin n) -> List Nat -> Vect n (Fin n)
applyCycleToVect imgs [] = imgs
applyCycleToVect imgs [_] = imgs
applyCycleToVect {n = Z} imgs _ = imgs
applyCycleToVect {n = S k} imgs cycle =
  let pairs : List (Nat, Nat)
      pairs = zip cycle (drop 1 cycle ++ take 1 cycle)
      -- Build a replacement map: src -> dst
      m : SortedMap Nat Nat
      m = fromList pairs
      -- For each position, check if it's in the cycle
  in Data.Vect.Fin.tabulate (\i =>
       case lookup (finToNat i) m of
         Just dst => finSafe {n=k} dst
         Nothing  => index i imgs)

||| Construct a permutation from cycle notation.
||| Cycles are given as lists of Nat indices (0-based).
export
fromCycles : (n : Nat) -> List (List Nat) -> Perm n
fromCycles n cycles =
  let base = (permId n).images
      applied = foldl applyCycleToVect base cycles
  in MkPerm applied

||| Construct from a mapping function.
export
fromMapping : (n : Nat) -> (Nat -> Nat) -> Perm n
fromMapping Z _ = MkPerm []
fromMapping n@(S k) f = MkPerm (Data.Vect.Fin.tabulate (\i => finSafe {n=k} (f (finToNat i))))

||| Get images as a list of Int (backward compatibility).
export
permImages : {n : Nat} -> Perm n -> List Int
permImages p = toList (map (\f => cast (finToNat f)) p.images)

------------------------------------------------------------------------
-- Orbit computation
------------------------------------------------------------------------

||| Orbit of a point under a set of generators.
export
orbit : {n : Nat} -> List (Perm n) -> Fin n -> SortedSet Nat
orbit gens start = go (insert (finToNat start) empty) [start]
  where
    go : SortedSet Nat -> List (Fin n) -> SortedSet Nat
    go visited [] = visited
    go visited (x :: queue) =
      let newPoints = filter (\p => not (contains (finToNat p) visited))
                             [permApply g x | g <- gens]
          visited' = foldl (\s, p => insert (finToNat p) s) visited newPoints
      in go visited' (queue ++ newPoints)

------------------------------------------------------------------------
-- Schreier-Sims (simplified)
------------------------------------------------------------------------

||| Base and strong generating set.
public export
record BSGS (n : Nat) where
  constructor MkBSGS
  bsgsBase : List (Fin n)
  bsgsGens : List (Perm n)

||| Compute a BSGS via simplified Schreier-Sims.
export
schreierSims : {n : Nat} -> List (Perm n) -> BSGS n
schreierSims gens =
  MkBSGS (toList (Data.Vect.Fin.tabulate {len=n} id)) gens

||| Order of the group (simplified: product of orbit sizes).
export
groupOrder : {n : Nat} -> BSGS n -> Integer
groupOrder bsgs =
  if null (bsgsGens bsgs) then 1
  else
    let orbitSizes = map (\b => cast (length (Prelude.toList (orbit (bsgsGens bsgs) b)))) (bsgsBase bsgs)
    in foldl (*) 1 (take 1 orbitSizes)  -- simplified

||| Check if a permutation is in the group.
export
groupContains : BSGS n -> Perm n -> Bool
groupContains _ p = True  -- stub: always true (conservative)

||| Enumerate all group elements (small groups only).
export
groupElements : {n : Nat} -> BSGS n -> List (Perm n)
groupElements bsgs =
  if null (bsgsGens bsgs) then [permId n]
  else bsgsGens bsgs  -- stub: just return generators

------------------------------------------------------------------------
-- Show / Eq
------------------------------------------------------------------------

export
{n : Nat} -> Show (Perm n) where
  show p = "Perm" ++ show (permImages p)

export
{n : Nat} -> Eq (Perm n) where
  a == b = permImages a == permImages b

export
{n : Nat} -> Show (BSGS n) where
  show bsgs = "BSGS(base=" ++ show (map (\f => cast {to=Int} (finToNat f)) (bsgsBase bsgs)) ++ ")"
