--- Database of transitive subgroups of S_n for small n.
---
--- Groups are numbered following the Butler-McKay convention.
--- For prime p, solvable transitive subgroups are computed at runtime
--- from the structure of AGL(1,p) = Z/p ⋊ (Z/p)*.
--- Degree 5 retains a hard-coded fast path.
module TransitiveGroup
  ( TransitiveGroup(..)
  , tgName, tgDegree, tgOrder, tgGenerators, tgSolvable
  , tgCompositionFactors
  , tgC5, tgD5, tgF20, tgA5, tgS5
  , allTransitiveGroups5
  , allTransitiveGroups
  , transGroupByOrder
  , compositionSeries
  , showTransitiveGroup
  ) where

import Permutation
import PrimeFactors (isPrime, factorise)
import Positive (unsafePositive)

--- A transitive subgroup description.
data TransitiveGroup = TransitiveGroup
  String    -- name (e.g. "C5", "D5", "F20", "A5", "S5")
  Int       -- degree n
  Int       -- order |G|
  Bool      -- is solvable?
  [Perm]    -- generators
  [Int]     -- maximal supergroup indices (into the sorted list)
  [Int]     -- composition factors (prime orders, top-down)

--- Accessors.
tgName :: TransitiveGroup -> String
tgName (TransitiveGroup n _ _ _ _ _ _) = n

tgDegree :: TransitiveGroup -> Int
tgDegree (TransitiveGroup _ d _ _ _ _ _) = d

tgOrder :: TransitiveGroup -> Int
tgOrder (TransitiveGroup _ _ o _ _ _ _) = o

tgSolvable :: TransitiveGroup -> Bool
tgSolvable (TransitiveGroup _ _ _ s _ _ _) = s

tgGenerators :: TransitiveGroup -> [Perm]
tgGenerators (TransitiveGroup _ _ _ _ g _ _) = g

tgCompositionFactors :: TransitiveGroup -> [Int]
tgCompositionFactors (TransitiveGroup _ _ _ _ _ _ cf) = cf

--- C5 = Z/5Z, cyclic group of order 5.
tgC5 :: TransitiveGroup
tgC5 = TransitiveGroup "C5" 5 5 True
  [permFromCycles 5 [[0, 1, 2, 3, 4]]]
  [1] [5]

--- D5 = dihedral group of order 10.
tgD5 :: TransitiveGroup
tgD5 = TransitiveGroup "D5" 5 10 True
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[1, 4], [2, 3]]
  ]
  [2] [5, 2]

--- F20 = Frobenius group of order 20 (Z/5 ⋊ Z/4).
tgF20 :: TransitiveGroup
tgF20 = TransitiveGroup "F20" 5 20 True
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[1, 2, 4, 3]]
  ]
  [3, 4] [5, 2, 2]

--- A5 = alternating group of order 60.
tgA5 :: TransitiveGroup
tgA5 = TransitiveGroup "A5" 5 60 False
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[0, 1, 2]]
  ]
  [4] []

--- S5 = symmetric group of order 120.
tgS5 :: TransitiveGroup
tgS5 = TransitiveGroup "S5" 5 120 False
  [ permFromCycles 5 [[0, 1, 2, 3, 4]]
  , permFromCycles 5 [[0, 1]]
  ]
  [] []

--- All transitive groups of degree 5 (smallest to largest).
allTransitiveGroups5 :: [TransitiveGroup]
allTransitiveGroups5 = [tgC5, tgD5, tgF20, tgA5, tgS5]

--- All transitive groups of degree n.
--- For prime n, computes at runtime from the AGL(1,p) structure.
--- For degree 5, uses the hard-coded database (fast path).
--- For unsupported composite degrees, returns [].
allTransitiveGroups :: Int -> [TransitiveGroup]
allTransitiveGroups 5 = allTransitiveGroups5
allTransitiveGroups n
  | n >= 3 && isPrime n = transGroupsOfPrimeRT n
  | otherwise           = []

--- Find transitive group(s) of given degree and order.
transGroupByOrder :: Int -> Int -> [TransitiveGroup]
transGroupByOrder deg ord =
  [g | g <- allTransitiveGroups deg, tgOrder g == ord]

------------------------------------------------------------------------
-- Runtime prime group computation
------------------------------------------------------------------------

--- Modular exponentiation.
modExpRT :: Int -> Int -> Int -> Int
modExpRT _ 0 _ = 1
modExpRT b e m
  | even e    = let half = modExpRT b (e `div` 2) m in (half * half) `mod` m
  | otherwise = (b * modExpRT b (e - 1) m) `mod` m

--- Primitive root modulo a prime.
primitiveRootRT :: Int -> Int
primitiveRootRT p =
  let phi = p - 1
      factors = nubInt (map fst (factorise (unsafePositive phi)))
      isPrimRoot g = all (\q -> modExpRT g (phi `div` q) p /= 1) factors
  in headList (filter isPrimRoot [2 .. p - 1])

--- Remove duplicates from a sorted Int list (or unsorted, by membership).
nubInt :: [Int] -> [Int]
nubInt [] = []
nubInt (x:xs) = x : nubInt (filter (/= x) xs)

headList :: [a] -> a
headList (x:_) = x
headList []    = error "headList: empty"

--- Sorted positive divisors of n.
divisorsRT :: Int -> [Int]
divisorsRT n =
  let s = isqrtRT n
      small = [d | d <- [1 .. s], n `mod` d == 0]
  in sortInt (nubInt (concatMap (\d -> if d * d == n then [d] else [d, n `div` d]) small))

isqrtRT :: Int -> Int
isqrtRT n
  | n <= 0    = 0
  | otherwise = go n
  where
    go x = let x' = (x + n `div` x) `div` 2
           in if x' >= x then x else go x'

--- Sort a list of integers (insertion sort, sufficient for small lists).
sortInt :: [Int] -> [Int]
sortInt [] = []
sortInt (x:xs) = insertInt x (sortInt xs)

insertInt :: Int -> [Int] -> [Int]
insertInt x [] = [x]
insertInt x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertInt x ys

--- Prime factorisation as a flat list with multiplicity.
primeFactorsWithMultRT :: Int -> [Int]
primeFactorsWithMultRT 1 = []
primeFactorsWithMultRT n =
  concatMap (\(q, e) -> replicate e q) (factorise (unsafePositive n))

--- Factorial.
factorial :: Int -> Int
factorial n
  | n <= 1    = 1
  | otherwise = n * factorial (n - 1)

--- Build all transitive subgroups of S_p for a prime p.
---
--- Solvable subgroups: Z/p ⋊ H for each divisor d of p-1.
--- Non-solvable: A_p and S_p (for p >= 5).
transGroupsOfPrimeRT :: Int -> [TransitiveGroup]
transGroupsOfPrimeRT p =
  let g = primitiveRootRT p
      ds = divisorsRT (p - 1)
      -- Build one solvable group per divisor of p-1
      mkAffine d =
        let trans = Perm [((i + 1) `mod` p) | i <- [0 .. p - 1]]
            sf = modExpRT g ((p - 1) `div` d) p
            scale = Perm [((sf * i) `mod` p) | i <- [0 .. p - 1]]
            gens = if d == 1 then [trans] else [trans, scale]
            gName
              | d == 1     = "Z" ++ show p
              | d == 2     = "D" ++ show p
              | d == p - 1 = "AGL(1," ++ show p ++ ")"
              | otherwise  = "Z" ++ show p ++ ":Z" ++ show d
            cFactors = primeFactorsWithMultRT d ++ [p]
        in TransitiveGroup gName p (p * d) True gens [] cFactors
      solvableGroups = map mkAffine ds
      -- A_p
      ap = TransitiveGroup ("A" ++ show p) p (factorial p `div` 2) (p < 5)
             [ permFromCycles p [[0 .. p - 1]]
             , permFromCycles p [[0, 1, 2]]
             ]
             [] []
      -- S_p
      sp = TransitiveGroup ("S" ++ show p) p (factorial p) (p < 4)
             [ permFromCycles p [[0 .. p - 1]]
             , permFromCycles p [[0, 1]]
             ]
             [] []
      allGroups = sortByOrder (solvableGroups ++ [ap, sp])
      -- Assign maximal supergroups
      indexed = zip [0..] allGroups
      assignSuper (myIdx, tg) =
        let myOrd = tgOrder tg
            cands = [(i, tgOrder cg) | (i, cg) <- indexed,
                     i /= myIdx, tgOrder cg > myOrd,
                     tgOrder cg `mod` myOrd == 0]
            isMax (_, superOrd) =
              not (any (\(_, midOrd) -> midOrd > myOrd && midOrd < superOrd
                        && superOrd `mod` midOrd == 0
                        && midOrd `mod` myOrd == 0) cands)
            maxSupers = [i | (i, _) <- cands, isMax (i, tgOrder (snd (indexed !! i)))]
        in TransitiveGroup (tgName tg) (tgDegree tg) (tgOrder tg)
             (tgSolvable tg) (tgGenerators tg) maxSupers
             (tgCompositionFactors tg)
  in map (\pair -> assignSuper pair) indexed

--- Sort transitive groups by order.
sortByOrder :: [TransitiveGroup] -> [TransitiveGroup]
sortByOrder [] = []
sortByOrder (x:xs) =
  sortByOrder [y | y <- xs, tgOrder y <= tgOrder x]
  ++ [x]
  ++ sortByOrder [y | y <- xs, tgOrder y > tgOrder x]

------------------------------------------------------------------------
-- Composition series
------------------------------------------------------------------------

--- For a solvable transitive group, return the composition series as a
--- list of generating sets, descending from G to {1}.
---
--- Each consecutive quotient G_i / G_{i+1} is cyclic of prime order.
--- Returns Nothing for non-solvable groups.
compositionSeries :: TransitiveGroup -> Maybe [[Perm]]
compositionSeries tg
  | not (tgSolvable tg) = Nothing
  | tgName tg == "C5" = Just [tgGenerators tg, []]
  | tgName tg == "D5" =
      Just [ tgGenerators tg
           , [permFromCycles 5 [[0, 1, 2, 3, 4]]]
           , []
           ]
  | tgName tg == "F20" =
      Just [ tgGenerators tg
           , [ permFromCycles 5 [[0, 1, 2, 3, 4]]
             , permFromCycles 5 [[1, 4], [2, 3]]
             ]
           , [permFromCycles 5 [[0, 1, 2, 3, 4]]]
           , []
           ]
  | isPrime (tgDegree tg) = compositionSeriesPrime tg
  | otherwise = Nothing

--- Composition series for a solvable affine subgroup of S_p.
---
--- The group has the form Z/p ⋊ H where H is cyclic of order d.
--- The series descends through subgroups of H by removing one prime
--- factor at a time, then drops to {1}.
compositionSeriesPrime :: TransitiveGroup -> Maybe [[Perm]]
compositionSeriesPrime tg =
  let p = tgDegree tg
      g = primitiveRootRT p
      d = tgOrder tg `div` p
      dFactors = primeFactorsWithMultRT d
      -- Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
      dChain = scanl1 (\acc q -> acc `div` q) (d : dFactors)
      trans = permFromCycles p [[0 .. p - 1]]
      mkGens d'
        | d' <= 1   = [trans]
        | otherwise =
            let sf = modExpRT g ((p - 1) `div` d') p
                scl = Perm [((sf * i) `mod` p) | i <- [0 .. p - 1]]
            in [trans, scl]
  in Just (map mkGens dChain ++ [[]])

--- scanl1 for lists.
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 _ [] = []
scanl1 f (x:xs) = scanl f x xs

--- Show.
showTransitiveGroup :: TransitiveGroup -> String
showTransitiveGroup tg =
  tgName tg ++ " (degree " ++ show (tgDegree tg)
  ++ ", order " ++ show (tgOrder tg) ++ ", "
  ++ (if tgSolvable tg then "solvable" else "non-solvable") ++ ")"

instance Eq TransitiveGroup where
  g1 == g2 = tgName g1 == tgName g2 && tgOrder g1 == tgOrder g2

instance Show TransitiveGroup where
  show = showTransitiveGroup
