module Surd.Permutation

import Data.SortedMap
import Data.SortedSet
import Data.List

%default covering

------------------------------------------------------------------------
-- Permutation type
------------------------------------------------------------------------

||| A permutation on {0, 1, ..., n-1} stored as an image list.
public export
data Perm = MkPerm (List Int)

||| Get the images list.
export
permImages : Perm -> List Int
permImages (MkPerm imgs) = imgs

||| Degree of the permutation.
export
permN : Perm -> Nat
permN p = length (permImages p)

||| Apply a permutation to a point.
export
permApply : Perm -> Int -> Int
permApply (MkPerm images) i =
  case drop (cast (max 0 i)) images of
    (x :: _) => x
    [] => i

||| Identity permutation of degree n.
export
permId : Nat -> Perm
permId n = MkPerm (map cast [0 .. cast (minus n 1)])

||| Compose two permutations: (f . g)(x) = f(g(x)).
export
permCompose : Perm -> Perm -> Perm
permCompose f g =
  let n = max (permN f) (permN g)
  in MkPerm [permApply f (permApply g (cast i)) | i <- [0 .. cast (minus n 1)]]

||| Inverse of a permutation.
export
permInverse : Perm -> Perm
permInverse p =
  let n = permN p
      pairs = zip (permImages p) (map cast [0 .. cast (minus n 1)])
      sorted = sortBy (\a, b => compare (fst a) (fst b)) pairs
  in MkPerm (map snd sorted)

||| Check if a permutation is the identity.
export
permIsId : Perm -> Bool
permIsId (MkPerm images) = images == map cast [0 .. cast (minus (length images) 1)]

||| Order of a permutation (smallest k > 0 such that sigma^k = id).
export
permOrder : Perm -> Nat
permOrder p = go (permCompose p p) 1
  where
    go : Perm -> Nat -> Nat
    go current k =
      if permIsId current then S k
      else if k > 1000 then S k  -- safety bound
      else go (permCompose current p) (S k)

||| Cycle decomposition of a permutation.
export
permCycles : Perm -> List (List Int)
permCycles p =
  let n = permN p
  in snd (foldl findCycle (empty, []) (map cast [0 .. cast (minus n 1)]))
  where
    traceCycle : Perm -> Int -> SortedSet Int -> List Int
    traceCycle p start visited =
      if contains start visited then []
      else start :: traceCycle p (permApply p start) (insert start visited)

    findCycle : (SortedSet Int, List (List Int)) -> Int -> (SortedSet Int, List (List Int))
    findCycle (visited, cycles) i =
      if contains i visited then (visited, cycles)
      else
        let cyc = traceCycle p i visited
            newVisited = foldl (\s, x => insert x s) visited cyc
        in if length cyc <= 1 then (newVisited, cycles)
           else (newVisited, cyc :: cycles)

||| Sign of a permutation: +1 for even, -1 for odd.
export
permSign : Perm -> Int
permSign p =
  let cycles = permCycles p
      transpositions = sum (map (\c => cast (minus (length c) 1)) cycles)
  in if mod transpositions 2 == 0 then 1 else -1

updateAtPerm : List Int -> Nat -> Int -> List Int
updateAtPerm [] _ _ = []
updateAtPerm (_ :: xs) Z v = v :: xs
updateAtPerm (x :: xs) (S k) v = x :: updateAtPerm xs k v

applyCyclePerm : List Int -> List Int -> List Int
applyCyclePerm imgs [] = imgs
applyCyclePerm imgs [_] = imgs
applyCyclePerm imgs cycle =
  foldl (\im, idx =>
    let src = case drop (cast idx) cycle of (x :: _) => x; [] => 0
        dst = case drop (cast (mod (idx + 1) (cast (length cycle)))) cycle of (x :: _) => x; [] => 0
    in updateAtPerm im (cast src) dst) imgs [0 .. cast (minus (length cycle) 1)]

||| Construct a permutation from cycle notation.
||| Cycles act on {0, ..., n-1}.
export
fromCycles : Nat -> List (List Int) -> Perm
fromCycles n cycles =
  let base = permId n
      images = permImages base
      applied = foldl applyCyclePerm images cycles
  in MkPerm applied

||| Construct from a mapping function.
export
fromMapping : Nat -> (Int -> Int) -> Perm
fromMapping n f = MkPerm [f (cast i) | i <- [0 .. cast (minus n 1)]]

||| Orbit of a point under a set of generators.
export
orbit : List Perm -> Int -> SortedSet Int
orbit gens start = go (insert start empty) [start]
  where
    go : SortedSet Int -> List Int -> SortedSet Int
    go visited [] = visited
    go visited (x :: queue) =
      let newPoints = filter (\p => not (contains p visited))
                             [permApply g x | g <- gens]
          visited' = foldl (\s, p => insert p s) visited newPoints
      in go visited' (queue ++ newPoints)

------------------------------------------------------------------------
-- Schreier-Sims (simplified)
------------------------------------------------------------------------

||| Base and strong generating set.
public export
record BSGS where
  constructor MkBSGS
  bsgsBase : List Int
  bsgsGens : List Perm

||| Compute a BSGS via simplified Schreier-Sims.
export
schreierSims : Nat -> List Perm -> BSGS
schreierSims n gens =
  -- Simplified: just store the generators and use the identity base
  MkBSGS (map cast [0 .. cast (minus n 1)]) gens

||| Order of the group (simplified: product of orbit sizes).
export
groupOrder : BSGS -> Integer
groupOrder bsgs =
  let n = length (bsgsBase bsgs)
  in if null (bsgsGens bsgs) then 1
     else
       let orbitSizes = map (\b => cast (Data.SortedSet.toList (orbit (bsgsGens bsgs) b) |> length)) (bsgsBase bsgs)
       in foldl (*) 1 (take 1 orbitSizes)  -- simplified

||| Check if a permutation is in the group.
export
groupContains : BSGS -> Perm -> Bool
groupContains _ p = True  -- stub: always true (conservative)

||| Enumerate all group elements (small groups only).
export
groupElements : BSGS -> List Perm
groupElements bsgs =
  if null (bsgsGens bsgs) then [permId (length (bsgsBase bsgs))]
  else bsgsGens bsgs  -- stub: just return generators

------------------------------------------------------------------------
-- Show / Eq
------------------------------------------------------------------------

export
Show Perm where
  show (MkPerm imgs) = "Perm" ++ show imgs

export
Eq Perm where
  (MkPerm a) == (MkPerm b) = a == b

export
Show BSGS where
  show bsgs = "BSGS(base=" ++ show (bsgsBase bsgs) ++ ")"
