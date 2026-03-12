{- |
Module      : Surd.Galois.Permutation
Description : Permutation groups on finite sets with Schreier–Sims machinery
License     : BSD-3-Clause

Permutations acting on the set \(\{0, 1, \ldots, n-1\}\), stored internally
as image lists (i.e.\ the permutation \(\sigma\) is represented by the list
\([\sigma(0), \sigma(1), \ldots, \sigma(n-1)]\)).  This gives \(O(n)\) point
application and \(O(n)\) composition, at the cost of \(O(n)\) space per
element.

The main group-theoretic functionality is provided by the __Schreier–Sims
algorithm__, which computes a /base and strong generating set/ (BSGS) for
a permutation group \(G \le S_n\) given by generators.  A BSGS consists of
a sequence of base points \(\beta_1, \ldots, \beta_k\) and a strong
generating set \(S\) such that

\[
  \langle S \cap G^{(i)} \rangle = G^{(i)}
  \quad \text{for each } i,
\]

where \(G^{(i)} = \mathrm{Stab}_G(\beta_1, \ldots, \beta_i)\) is the
pointwise stabiliser of the first \(i\) base points.

The stabiliser chain is:

@
  G = G^{(0)} ≥ G^{(1)} ≥ G^{(2)} ≥ ··· ≥ G^{(k)} = {id}
  │               │               │
  Δ₁ = β₁^G⁽⁰⁾   Δ₂ = β₂^G⁽¹⁾   Δ₃ = β₃^G⁽²⁾
@

The group order factorises as \(|G| = \prod_{i=1}^{k} |\Delta_i|\), and
membership testing reduces to sifting through the chain.

== References

  * Seress, A. (2003). /Permutation Group Algorithms./ Cambridge University
    Press. DOI: <https://doi.org/10.1017/CBO9780511546549 10.1017/CBO9780511546549>

  * Knuth, D. E. (1991). "Efficient representation of perm groups."
    /Combinatorica/ 11(1), 33–43.
    DOI: <https://doi.org/10.1007/BF01375472 10.1007/BF01375472>

  * Sims, C. C. (1970). "Computational methods in the study of permutation
    groups." In /Computational Problems in Abstract Algebra/, pp. 169–183.
    Pergamon.
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

{- | A permutation \(\sigma \in S_n\) acting on \(\{0, 1, \ldots, n-1\}\).

__Invariant:__ '_permImg' is a permutation of @[0 .. n-1]@, i.e.\ a list
of length \(n\) containing each element of \(\{0, \ldots, n-1\}\) exactly
once.

The internal representation is the image list
\([\sigma(0), \sigma(1), \ldots, \sigma(n-1)]\).
-}
data Perm = Perm
    { permN :: !Int
    {- ^ The degree of the permutation, i.e.\ the size \(n\) of the set
    \(\{0, \ldots, n-1\}\) on which \(\sigma\) acts.
    -}
    , _permImg :: ![Int]
    }
    deriving (Show)

instance Eq Perm where
    Perm n1 img1 == Perm n2 img2 = n1 == n2 && img1 == img2

instance Ord Perm where
    compare (Perm n1 img1) (Perm n2 img2) = compare n1 n2 <> compare img1 img2

{- | Apply \(\sigma\) to a point: \(\sigma(i)\).

\(O(n)\) due to list indexing.  For repeated application on the same
permutation, consider converting to a 'Data.IntMap.IntMap' or array
externally.
-}
permApply :: Perm -> Int -> Int
permApply (Perm _ img) i = img !! i

{- | Compose two permutations (right-to-left convention):

\[
  (\sigma \cdot \tau)(i) = \sigma(\tau(i)).
\]

\(O(n)\) where \(n\) is the degree.
-}
permCompose :: Perm -> Perm -> Perm
permCompose (Perm n img1) (Perm _ img2) =
    Perm n [img1 !! (img2 !! i) | i <- [0 .. n - 1]]

{- | The inverse permutation \(\sigma^{ -1}\), satisfying
\(\sigma \cdot \sigma^{ -1} = \mathrm{id}\).

\(O(n \log n)\) via 'Data.Map.Strict.Map' construction.
-}
permInverse :: Perm -> Perm
permInverse (Perm n img) =
    let inv = map snd $ Map.toAscList $ Map.fromList [(img !! i, i) | i <- [0 .. n - 1]]
     in Perm n inv

{- | The identity permutation on \(\{0, \ldots, n-1\}\).

\(\mathrm{id}(i) = i\) for all \(i\).
-}
permId :: Int -> Perm
permId n = Perm n [0 .. n - 1]

{- | Test whether a permutation is the identity.

\(O(n)\).
-}
permIsId :: Perm -> Bool
permIsId (Perm n img) = img == [0 .. n - 1]

{- | The order of a permutation, i.e.\ the smallest positive \(k\) with
\(\sigma^k = \mathrm{id}\).  Equals the least common multiple of the
cycle lengths:

\[
  |\langle \sigma \rangle| = \mathrm{lcm}(\ell_1, \ldots, \ell_m).
\]
-}
permOrder :: Perm -> Int
permOrder p
    | permIsId p = 1
    | otherwise = foldl' lcm 1 (map length (permCycles p))

{- | Cycle decomposition of \(\sigma\), excluding fixed points.

Each cycle is listed starting from its smallest element (canonical form).
For example, the permutation \((0\;2\;4)(1\;3)\) on \(\{0,\ldots,4\}\)
returns @[[0,2,4],[1,3]]@.
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

{- | The sign (parity) of a permutation:

\[
  \mathrm{sgn}(\sigma) = (-1)^{N}
\]

where \(N\) is the number of transpositions in any decomposition of
\(\sigma\) (equivalently, \(N = \sum_i (\ell_i - 1)\) over cycle
lengths).  Returns @1@ for even permutations, @-1@ for odd.
-}
permSign :: Perm -> Int
permSign p =
    let k = sum [length c - 1 | c <- permCycles p]
     in if even k then 1 else -1

{- | Build a permutation from cycle notation.

>>> fromCycles 5 [[0,1,2],[3,4]]

constructs the permutation \((0\;1\;2)(3\;4) \in S_5\).  Unlisted
points are taken as fixed.
-}
fromCycles :: Int -> [[Int]] -> Perm
fromCycles n cycles =
    let base = Map.fromList [(i, i) | i <- [0 .. n - 1]]
        applyCyc m [] = m
        applyCyc m cyc@(c0 : _) =
            let pairs = zip cyc (drop 1 cyc ++ [c0])
             in foldl' (\m' (a, b) -> Map.insert a b m') m pairs
        final = foldl' applyCyc base cycles
     in Perm n (map snd $ Map.toAscList final)

{- | Build a permutation from a full image list
\([\sigma(0), \sigma(1), \ldots, \sigma(n-1)]\).

No validation is performed; the caller must ensure the list is a valid
permutation of @[0 .. n-1]@.
-}
fromMapping :: [Int] -> Perm
fromMapping img = Perm (length img) img

{- | The orbit of a point \(x\) under a group generated by the given
permutations:

\[
  \mathrm{Orb}_{\langle g_1, \ldots, g_k \rangle}(x)
  = \{ g(x) \mid g \in \langle g_1, \ldots, g_k \rangle \}.
\]

Computed by breadth-first search.  \(O(|\mathrm{Orb}| \cdot k)\) where
\(k\) is the number of generators.
-}
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

{- | A base and strong generating set (BSGS) for a permutation group
\(G \le S_n\).

A BSGS encodes the stabiliser chain

\[
  G = G^{(0)} \ge G^{(1)} \ge \cdots \ge G^{(k)} = \{1\}
\]

where \(G^{(i)} = \mathrm{Stab}_G(\beta_1, \ldots, \beta_i)\).
Each level stores a /transversal/: a map from each orbit point
\(\gamma \in \Delta_i = \beta_i^{G^{(i-1)}}\) to a coset representative
\(u_\gamma\) satisfying \(u_\gamma(\beta_i) = \gamma\).

The group order factorises as:

\[
  |G| = \prod_{i=1}^{k} |\Delta_i|
\]

and membership testing runs in \(O(k \cdot n)\) via sifting.
-}
data BSGS = BSGS
    { bsgsBase :: ![Int]
    {- ^ The base \((\beta_1, \ldots, \beta_k)\): a sequence of points
    such that the only element of \(G\) fixing all base points is
    the identity.
    -}
    , bsgsTransversals :: ![Map.Map Int Perm]
    {- ^ @bsgsTransversals !! i@ maps each point \(\gamma\) in the
    orbit \(\Delta_{i+1} = \beta_{i+1}^{G^{(i)}}\) to a coset
    representative \(u_\gamma\) with \(u_\gamma(\beta_{i+1}) = \gamma\).
    -}
    , bsgsN :: !Int
    -- ^ The degree \(n\) of the symmetric group \(S_n\) containing \(G\).
    }
    deriving (Show)

instance Eq BSGS where
    a == b =
        bsgsBase a == bsgsBase b
            && bsgsTransversals a == bsgsTransversals b

{- | Build a BSGS from generators via the incremental Schreier–Sims
algorithm.

Given generators \(g_1, \ldots, g_m\) of \(G \le S_n\), this function:

1. Sifts each generator through the current (initially empty) BSGS.
2. Extends the BSGS with any non-identity residue.
3. Saturates by computing /Schreier generators/
   \(u_\gamma^{ -1} \cdot s \cdot u_{\beta}\) at each level and sifting
   them, repeating until no new coset representatives are discovered.

The resulting BSGS is complete: it correctly represents \(G\).

Worst-case complexity is \(O(n^2 |S|^2 \log^3 |G|)\) (Seress, 2003,
§4.4), but is fast in practice for small groups.
-}
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

{- | The order of the group represented by the BSGS:

\[
  |G| = \prod_{i=1}^{k} |\Delta_i|.
\]

\(O(k)\) where \(k\) is the length of the base.
-}
groupOrder :: BSGS -> Integer
groupOrder (BSGS _ trans _) =
    product [fromIntegral (Map.size t) | t <- trans]

{- | Test whether a permutation belongs to the group.

Sifts the element through the stabiliser chain; membership holds iff
the residue is the identity.

\(O(k \cdot n)\) where \(k\) is the base length and \(n\) is the degree.
-}
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

{- | Enumerate all elements of the group by forming every combination
of coset representatives across all levels.

Returns \(|G|\) elements.  \(O(|G| \cdot k \cdot n)\) total.
-}
groupElements :: BSGS -> [Perm]
groupElements (BSGS _ [] n) = [permId n]
groupElements (BSGS _ trans n) =
    [foldl' permCompose (permId n) combo | combo <- mapM Map.elems trans]
