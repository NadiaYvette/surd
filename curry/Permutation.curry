--- Permutation groups on finite sets.
---
--- Permutations acting on {0, 1, ..., n-1}, stored as image lists.
--- Provides composition, inverse, order, cycle decomposition, and sign.
module Permutation
  ( Perm(..)
  , permN
  , permApply
  , permCompose
  , permInverse
  , permOrder
  , permId
  , permCycles
  , permSign
  , permIsId
  , permFromCycles
  , showPerm
  ) where

--- A permutation on {0, ..., n-1} stored as its image list.
data Perm = Perm [Int]

--- Degree of the permutation (size of the set it acts on).
permN :: Perm -> Int
permN (Perm xs) = length xs

--- Apply permutation to a point.
permApply :: Perm -> Int -> Int
permApply (Perm xs) i
  | i < 0 || i >= length xs = error "permApply: index out of range"
  | otherwise               = xs !! i

--- Identity permutation of degree n.
permId :: Int -> Perm
permId n = Perm [0 .. n - 1]

--- Compose two permutations: (f . g)(x) = f(g(x)).
permCompose :: Perm -> Perm -> Perm
permCompose f g =
  let n = permN f
  in Perm [permApply f (permApply g i) | i <- [0 .. n - 1]]

--- Inverse of a permutation.
permInverse :: Perm -> Perm
permInverse p@(Perm xs) =
  let n = permN p
      result = replicate n 0
  in Perm (foldl (\acc i -> setAt (xs !! i) i acc) result [0 .. n - 1])

--- Set element at index.
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = case xs of
  []       -> []
  (x:rest) -> if i == 0 then v : rest else x : setAt (i - 1) v rest

--- Order of a permutation (smallest positive k such that p^k = id).
permOrder :: Perm -> Int
permOrder p =
  let cycles = permCycles p
      lengths = map length cycles
  in foldl lcmInt 1 lengths

--- Cycle decomposition of a permutation.
--- Returns a list of cycles (each cycle is a list of points).
permCycles :: Perm -> [[Int]]
permCycles p@(Perm _) =
  let n = permN p
  in go [] [0 .. n - 1]
  where
    go visited remaining = case remaining of
      [] -> []
      (i:rest) ->
        if elem i visited
        then go visited rest
        else let cycle = traceCycle p i
             in if length cycle <= 1
                then go (visited ++ cycle) rest
                else cycle : go (visited ++ cycle) rest

--- Trace a single cycle starting from a point.
traceCycle :: Perm -> Int -> [Int]
traceCycle p start = go start []
  where
    go i acc =
      let next = permApply p i
      in if next == start
         then reverse (i : acc)
         else go next (i : acc)

--- Sign of a permutation: +1 for even, -1 for odd.
permSign :: Perm -> Int
permSign p =
  let cycles = permCycles p
      -- A cycle of length k has sign (-1)^(k-1)
      totalTranspositions = foldl (+) 0 (map (\c -> length c - 1) cycles)
  in if even totalTranspositions then 1 else negate 1

--- Check if a permutation is the identity.
permIsId :: Perm -> Bool
permIsId (Perm xs) = xs == [0 .. length xs - 1]

--- Construct a permutation from cycle notation.
--- Cycles are lists of points; points not mentioned are fixed.
permFromCycles :: Int -> [[Int]] -> Perm
permFromCycles n cycles =
  let base = [0 .. n - 1]
  in Perm (foldl applyCycle base cycles)

applyCycle :: [Int] -> [Int] -> [Int]
applyCycle xs cycle = case cycle of
  []  -> xs
  [_] -> xs
  _   ->
    let pairs = zip cycle (drop 1 cycle ++ [head cycle])
    in foldl (\acc (from, to) -> setAt from to acc) xs pairs

--- LCM of two integers.
lcmInt :: Int -> Int -> Int
lcmInt a b
  | a == 0    = 0
  | b == 0    = 0
  | otherwise = abs (a * b) `div` gcdInt (abs a) (abs b)

--- GCD.
gcdInt :: Int -> Int -> Int
gcdInt a b
  | b == 0    = a
  | otherwise = gcdInt b (a `mod` b)

--- Show a permutation.
showPerm :: Perm -> String
showPerm (Perm xs) = "Perm " ++ show xs

instance Eq Perm where
  (Perm xs) == (Perm ys) = xs == ys

instance Show Perm where
  show = showPerm
