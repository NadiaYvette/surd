{- | Permutations on {0, 1, ..., n-1} with Schreier–Sims machinery.

Permutations are stored as lists for O(n) application and O(n)
composition. Group-theoretic operations (order, membership) use
a base and strong generating set (BSGS).
-}
module Surd.Galois.Permutation (
    Perm,
    permN,
    permApply,
    permCompose,
    permInverse,
    permOrder,
    permId,
    permCycles,
    permSign,
    permIsId,
    fromCycles,
    fromMapping,
    orbit,
    BSGS (..),
    schreierSims,
    groupOrder,
    groupContains,
    groupElements,
)
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

------------------------------------------------------------------------
-- Permutations
------------------------------------------------------------------------

-- | A permutation on {0, ..., n-1}.
data Perm = Perm
    { permN :: !Int
    , _permImg :: ![Int]
    }
    deriving (Show)

instance Eq Perm where
    Perm n1 img1 == Perm n2 img2 = n1 == n2 && img1 == img2

instance Ord Perm where
    compare (Perm n1 img1) (Perm n2 img2) = compare n1 n2 <> compare img1 img2

permApply :: Perm -> Int -> Int
permApply (Perm _ img) i = img !! i

-- | (σ · τ)(i) = σ(τ(i))
permCompose :: Perm -> Perm -> Perm
permCompose (Perm n img1) (Perm _ img2) =
    Perm n [img1 !! (img2 !! i) | i <- [0 .. n - 1]]

permInverse :: Perm -> Perm
permInverse (Perm n img) =
    let inv = map snd $ Map.toAscList $ Map.fromList [(img !! i, i) | i <- [0 .. n - 1]]
     in Perm n inv

permId :: Int -> Perm
permId n = Perm n [0 .. n - 1]

permIsId :: Perm -> Bool
permIsId (Perm n img) = img == [0 .. n - 1]

-- | LCM of cycle lengths.
permOrder :: Perm -> Int
permOrder p
    | permIsId p = 1
    | otherwise = foldl' lcm 1 (map length (permCycles p))

{- | Cycle decomposition (non-trivial cycles only, each starting from
the smallest element).
-}
permCycles :: Perm -> [[Int]]
permCycles (Perm n img) = go Set.empty 0
  where
    go visited i
        | i >= n = []
        | Set.member i visited = go visited (i + 1)
        | img !! i == i = go (Set.insert i visited) (i + 1)
        | otherwise =
            let cyc = traceCycle i
             in cyc : go (foldl' (flip Set.insert) visited cyc) (i + 1)
    traceCycle start = start : takeWhile (/= start) (iterate (img !!) (img !! start))

-- | 1 for even permutations, -1 for odd.
permSign :: Perm -> Int
permSign p =
    let k = sum [length c - 1 | c <- permCycles p]
     in if even k then 1 else -1

-- | Build from cycle notation. @fromCycles 5 [[0,1,2],[3,4]]@ = (0 1 2)(3 4).
fromCycles :: Int -> [[Int]] -> Perm
fromCycles n cycles =
    let base = Map.fromList [(i, i) | i <- [0 .. n - 1]]
        applyCyc m [] = m
        applyCyc m cyc@(c0 : _) =
            let pairs = zip cyc (drop 1 cyc ++ [c0])
             in foldl' (\m' (a, b) -> Map.insert a b m') m pairs
        final = foldl' applyCyc base cycles
     in Perm n (map snd $ Map.toAscList final)

-- | Build from a full mapping [σ(0), σ(1), ...].
fromMapping :: [Int] -> Perm
fromMapping img = Perm (length img) img

-- | Orbit of a point under a list of generators.
orbit :: Int -> [Perm] -> Set.Set Int
orbit pt gens = bfs (Set.singleton pt) [pt]
  where
    bfs visited [] = visited
    bfs visited (x : queue) =
        let new = [permApply g x | g <- gens, not (Set.member (permApply g x) visited)]
            visited' = foldl' (flip Set.insert) visited new
         in bfs visited' (queue ++ new)

------------------------------------------------------------------------
-- Schreier–Sims
------------------------------------------------------------------------

-- | Base and strong generating set.
data BSGS = BSGS
    { bsgsBase :: ![Int]
    , bsgsTransversals :: ![Map.Map Int Perm]
    {- ^ @transversals !! i@ maps each orbit point to a coset rep
    taking @base !! i@ to that point.
    -}
    , bsgsN :: !Int
    }
    deriving (Show)

instance Eq BSGS where
    a == b =
        bsgsBase a == bsgsBase b
            && bsgsTransversals a == bsgsTransversals b

-- | Build a BSGS from generators via the incremental Schreier–Sims algorithm.
schreierSims :: Int -> [Perm] -> BSGS
schreierSims n gens
    | null gens || all permIsId gens = BSGS [] [] n
    | otherwise = saturate (foldl' siftInsert (BSGS [] [] n) gens)
  where
    siftInsert :: BSGS -> Perm -> BSGS
    siftInsert bsgs g =
        let r = sift bsgs g
         in if permIsId r then bsgs else extendBSGS bsgs r

    -- Sift g through the BSGS, returning the residue.
    sift :: BSGS -> Perm -> Perm
    sift (BSGS base trans _) = go 0
      where
        go i h
            | i >= length base = h
            | otherwise =
                let img = permApply h (base !! i)
                 in case Map.lookup img (trans !! i) of
                        Nothing -> h
                        Just u -> go (i + 1) (permCompose (permInverse u) h)

    -- Extend the BSGS with a non-identity residue.
    extendBSGS :: BSGS -> Perm -> BSGS
    extendBSGS (BSGS base trans _) g =
        let fixedLevels = length $ takeWhile (\b -> permApply g b == b) base
         in if fixedLevels >= length base
                then
                    -- g fixes all base points; add new level
                    let pt = case [i | i <- [0 .. n - 1], permApply g i /= i] of
                            (p : _) -> p
                            [] -> error "extendBSGS: non-identity fixes all points"
                     in BSGS
                            (base ++ [pt])
                            (trans ++ [buildTransversal pt [g]])
                            n
                else
                    -- Rebuild transversal at fixedLevels with g added
                    let existingReps = Map.elems (trans !! fixedLevels)
                        newTrans = buildTransversal (base !! fixedLevels) (g : existingReps)
                     in BSGS
                            base
                            (take fixedLevels trans ++ [newTrans] ++ drop (fixedLevels + 1) trans)
                            n

    -- BFS from base point to build orbit + coset reps.
    buildTransversal :: Int -> [Perm] -> Map.Map Int Perm
    buildTransversal pt gs = bfs (Map.singleton pt (permId n)) [pt]
      where
        bfs visited [] = visited
        bfs visited (x : queue) =
            let ux = visited Map.! x
                candidates = [(permApply g x, permCompose g ux) | g <- gs]
                new = filter (\(y, _) -> not (Map.member y visited)) candidates
                visited' = foldl' (\m (k, v) -> Map.insert k v m) visited new
             in bfs visited' (queue ++ map fst new)

    -- Saturate by computing Schreier generators until stable.
    saturate :: BSGS -> BSGS
    saturate bsgs =
        let sGens = schreierGens bsgs
            bsgs' = foldl' siftInsert bsgs sGens
         in if bsgsTransversals bsgs' == bsgsTransversals bsgs
                then bsgs'
                else saturate bsgs'

    -- Compute Schreier generators at each level.
    schreierGens :: BSGS -> [Perm]
    schreierGens (BSGS base trans _) =
        [ sg
        | (i, transversal) <- zip [0 ..] trans
        , let b = base !! i
        , u <- Map.elems transversal
        , let beta = permApply u b
        , s <- gens
        , let s_beta = permApply s beta -- s(β)
        , let u_s_beta = Map.findWithDefault (permId n) s_beta transversal
        , let sg = permCompose (permInverse u_s_beta) (permCompose s u)
        , not (permIsId sg)
        ]

-- | Group order = product of orbit sizes.
groupOrder :: BSGS -> Integer
groupOrder (BSGS _ trans _) =
    product [fromIntegral (Map.size t) | t <- trans]

-- | Test group membership.
groupContains :: BSGS -> Perm -> Bool
groupContains (BSGS base trans _n) = go 0
  where
    go i h
        | permIsId h = True
        | i >= length base = False
        | otherwise =
            let img = permApply h (base !! i)
             in case Map.lookup img (trans !! i) of
                    Nothing -> False
                    Just u -> go (i + 1) (permCompose (permInverse u) h)

-- | Enumerate all group elements.
groupElements :: BSGS -> [Perm]
groupElements (BSGS _ [] n) = [permId n]
groupElements (BSGS _ trans n) =
    [foldl' permCompose (permId n) combo | combo <- mapM Map.elems trans]
