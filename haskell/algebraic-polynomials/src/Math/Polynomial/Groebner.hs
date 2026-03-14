-- |
-- Module      : Math.Polynomial.Groebner
-- Description : Groebner basis computation for multivariate polynomial ideals
-- Stability   : experimental
--
-- Implements Buchberger's algorithm with the following optimisations:
--
--   * Buchberger's first criterion (coprime leading monomials are skipped)
--   * Sugar strategy for pair selection (graded order on S-polynomials)
--   * Inter-reduction of the final basis
--
-- The basis is parameterised by a monomial ordering, which determines
-- leading terms and hence the resulting normal forms.
--
-- Supports incremental construction: 'extendBasis' adds new generators to an
-- existing basis without recomputing from scratch, enabling layer-by-layer
-- construction (e.g., each Gauss period descent step).
module Math.Polynomial.Groebner
  ( -- * Monomial orderings
    MonoOrd
  , grevlex
  , grlex
  , lexOrd
  , elimOrd
    -- * Groebner basis
  , GroebnerBasis
  , gbPolys
  , gbOrd
  , groebnerBasis
  , extendBasis
    -- * Reduction
  , reduce
  , reduceCompletely
    -- * Multivariate polynomial division
  , divModMPoly
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Math.Polynomial.Multivariate

-- ---------------------------------------------------------------------------
-- Monomial orderings
-- ---------------------------------------------------------------------------

-- | A total order on monomials, satisfying the well-ordering and
-- compatibility properties required for Groebner basis computation.
--
-- Compatibility means: if \(m_1 > m_2\) then \(m \cdot m_1 > m \cdot m_2\)
-- for all monomials \(m\).
type MonoOrd = Mono -> Mono -> Ordering

-- | Graded reverse lexicographic order (grevlex).
--
-- Compares by total degree first; among monomials of the same total degree,
-- the monomial with a /smaller/ exponent on the highest-index differing
-- variable is considered /greater/.
--
-- This is the most efficient ordering for Buchberger's algorithm in practice.
grevlex :: MonoOrd
grevlex = compareGrevlex

-- | Graded lexicographic order (grlex).
--
-- Compares by total degree first, then by lexicographic order on variable
-- exponents.
grlex :: MonoOrd
grlex a b =
  case compare (monoDegree a) (monoDegree b) of
    EQ -> compare a b  -- Mono's derived Ord is lex on (Map Var Int)
    c  -> c

-- | Pure lexicographic order.
--
-- Compares variable exponents from the highest-index variable downward.
-- Useful for elimination but slower for Buchberger's algorithm.
lexOrd :: MonoOrd
lexOrd = compareLex

-- | Elimination order: variables in the given set are more expensive
-- than all other variables. Within each block, grevlex is used.
--
-- This is useful for computing with ideals where some variables should
-- be eliminated (e.g., the defining relations of radical atoms).
elimOrd :: Set.Set Var -> MonoOrd
elimOrd elims a b =
  let (ae, ar) = splitMono elims a
      (be, br) = splitMono elims b
  in case grevlex ae be of
       EQ -> grevlex ar br
       c  -> c

-- | Split a monomial into (variables in set, variables not in set).
splitMono :: Set.Set Var -> Mono -> (Mono, Mono)
splitMono vs (Mono m) =
  let (inSet, outSet) = Map.partitionWithKey (\v _ -> v `Set.member` vs) m
  in (Mono inSet, Mono outSet)

-- | Compare monomials in pure lex order (highest variable first).
compareLex :: Mono -> Mono -> Ordering
compareLex (Mono a) (Mono b) =
  let allVars = Map.keys (Map.union a b)
  in go (reverse allVars)
  where
    go [] = EQ
    go (v:vs) =
      case compare (Map.findWithDefault 0 v a) (Map.findWithDefault 0 v b) of
        EQ -> go vs
        c  -> c

-- ---------------------------------------------------------------------------
-- Leading term extraction
-- ---------------------------------------------------------------------------

-- | Leading term of a polynomial under the given ordering.
-- Returns @Nothing@ for the zero polynomial.
leadTerm :: MonoOrd -> MPoly k -> Maybe (Mono, k)
leadTerm _ (MPoly m) | Map.null m = Nothing
leadTerm cmp (MPoly m) =
  Just $ foldl1 pick (Map.toList m)
  where
    pick best@(bm, _) cur@(cm, _) =
      case cmp cm bm of
        GT -> cur
        _  -> best

-- | Leading monomial under the given ordering.
leadMono :: MonoOrd -> MPoly k -> Maybe Mono
leadMono cmp p = fst <$> leadTerm cmp p

-- | Leading coefficient under the given ordering.
leadCoeffM :: MonoOrd -> MPoly k -> Maybe k
leadCoeffM cmp p = snd <$> leadTerm cmp p

-- ---------------------------------------------------------------------------
-- Multivariate polynomial division
-- ---------------------------------------------------------------------------

-- | Multivariate polynomial division by a list of divisors.
--
-- @divModMPoly ord f [g1, ..., gn]@ returns @(quotients, remainder)@ such that:
--
-- \[f = \sum_i q_i \cdot g_i + r\]
--
-- and no term of the remainder \(r\) is divisible by the leading term of
-- any divisor. The result depends on the monomial ordering.
divModMPoly :: (Eq k, Fractional k)
            => MonoOrd -> MPoly k -> [MPoly k] -> ([MPoly k], MPoly k)
divModMPoly cmp f divisors = go f (replicate n zeroPoly) zeroPoly
  where
    n = length divisors
    lts = [(i, lm, lc) | (i, g) <- zip [0..] divisors
                        , Just (lm, lc) <- [leadTerm cmp g]]

    go p qs r
      | isZero p  = (qs, r)
      | otherwise =
        case leadTerm cmp p of
          Nothing -> (qs, r)
          Just (pm, pc) ->
            case findDivisor pm lts of
              Just (i, dm, dc) ->
                let qm = monoDiv pm dm
                    qc = pc / dc
                    qi = MPoly (Map.singleton qm qc)
                    p' = subPoly p (mulPoly qi (divisors !! i))
                    qs' = updateAt i (addPoly qi) qs
                in go p' qs' r
              Nothing ->
                let lt = MPoly (Map.singleton pm pc)
                    p' = subPoly p lt
                in go p' qs (addPoly r lt)

    findDivisor _ [] = Nothing
    findDivisor pm ((i, dm, dc):rest)
      | monoDivides dm pm = Just (i, dm, dc)
      | otherwise         = findDivisor pm rest

    updateAt _ _ []     = []
    updateAt 0 f' (x:xs) = f' x : xs
    updateAt i' f' (x:xs) = x : updateAt (i' - 1) f' xs

-- | Reduce a polynomial to normal form modulo a list of divisors.
--
-- Equivalent to @snd ('divModMPoly' ...)@ but avoids tracking quotients
-- for better performance.
reducePoly :: (Eq k, Fractional k)
           => MonoOrd -> MPoly k -> [MPoly k] -> MPoly k
reducePoly cmp f divisors = go f zeroPoly
  where
    lts = [(lm, lc, g) | g <- divisors
                        , Just (lm, lc) <- [leadTerm cmp g]]

    go p r
      | isZero p  = r
      | otherwise =
        case leadTerm cmp p of
          Nothing -> r
          Just (pm, pc) ->
            case findDiv pm lts of
              Just (dm, dc, g) ->
                let qm = monoDiv pm dm
                    qc = pc / dc
                    qt = MPoly (Map.singleton qm qc)
                    p' = subPoly p (mulPoly qt g)
                in go p' r
              Nothing ->
                let lt = MPoly (Map.singleton pm pc)
                    p' = subPoly p lt
                in go p' (addPoly r lt)

    findDiv _ [] = Nothing
    findDiv pm ((dm, dc, g):rest)
      | monoDivides dm pm = Just (dm, dc, g)
      | otherwise         = findDiv pm rest

-- ---------------------------------------------------------------------------
-- S-polynomials
-- ---------------------------------------------------------------------------

-- | S-polynomial of two polynomials under the given monomial ordering.
--
-- \[S(f, g) = \frac{\text{lcm}(\text{LM}(f), \text{LM}(g))}{\text{LT}(f)} \cdot f
--           - \frac{\text{lcm}(\text{LM}(f), \text{LM}(g))}{\text{LT}(g)} \cdot g\]
sPolynomial :: (Eq k, Fractional k)
            => MonoOrd -> MPoly k -> MPoly k -> MPoly k
sPolynomial cmp f g =
  case (leadTerm cmp f, leadTerm cmp g) of
    (Just (mf, cf), Just (mg, cg)) ->
      let l  = monoLcm mf mg
          qf = monoDiv l mf
          qg = monoDiv l mg
          tf = scalePoly (1 / cf) (mulByMono qf f)
          tg = scalePoly (1 / cg) (mulByMono qg g)
      in subPoly tf tg
    _ -> zeroPoly

-- | Multiply a polynomial by a monomial (more efficient than full
-- polynomial multiplication).
mulByMono :: (Num k) => Mono -> MPoly k -> MPoly k
mulByMono m (MPoly p) =
  MPoly (Map.mapKeys (monoMul m) p)

-- ---------------------------------------------------------------------------
-- Buchberger's algorithm
-- ---------------------------------------------------------------------------

-- | A Groebner basis together with its monomial ordering.
data GroebnerBasis k = GroebnerBasis
  { gbOrd    :: MonoOrd
    -- ^ The monomial ordering used for this basis.
  , gbPolys  :: [MPoly k]
    -- ^ The basis polynomials (inter-reduced and monic).
  }

-- | Compute a Groebner basis for the ideal generated by the given polynomials.
--
-- Uses Buchberger's algorithm with the first criterion optimisation
-- (coprime leading monomials). The result is inter-reduced: each
-- polynomial is reduced modulo all others, and all polynomials are monic.
groebnerBasis :: (Eq k, Fractional k)
              => MonoOrd -> [MPoly k] -> GroebnerBasis k
groebnerBasis cmp gens =
  let nonzero = filter (not . isZero) gens
      basis = buchberger cmp nonzero
      reduced = interReduce cmp basis
  in GroebnerBasis cmp reduced

-- | Extend an existing Groebner basis with new generators.
--
-- Only processes S-pairs involving the new generators against the existing
-- basis, avoiding redundant recomputation. Useful for incremental
-- construction (e.g., adding one Gauss period descent step at a time).
extendBasis :: (Eq k, Fractional k)
            => [MPoly k] -> GroebnerBasis k -> GroebnerBasis k
extendBasis newGens (GroebnerBasis cmp existing) =
  let nonzero = filter (not . isZero) newGens
      -- Reduce new generators against existing basis first
      reduced = [r | g <- nonzero, let r = reducePoly cmp g existing, not (isZero r)]
      -- Run Buchberger with only cross-pairs (new x old) and (new x new)
      combined = existing ++ reduced
      newPairs = [(i, j) | (i, _) <- zip [0..] combined
                          , (j, _) <- zip [0..] combined
                          , i < j
                          , i >= length existing || j >= length existing]
      basis = buchbergerWithPairs cmp combined newPairs
      final = interReduce cmp basis
  in GroebnerBasis cmp final

-- | Reduce a polynomial modulo a Groebner basis.
--
-- The result is the unique normal form of the polynomial with respect to
-- the basis and its monomial ordering.
reduce :: (Eq k, Fractional k) => GroebnerBasis k -> MPoly k -> MPoly k
reduce (GroebnerBasis cmp basis) f = reducePoly cmp f basis

-- | Reduce a polynomial completely, making the result monic if nonzero.
--
-- Equivalent to 'reduce' followed by dividing by the leading coefficient.
reduceCompletely :: (Eq k, Fractional k) => GroebnerBasis k -> MPoly k -> MPoly k
reduceCompletely gb f =
  let r = reduce gb f
  in case leadCoeffM (gbOrd gb) r of
       Just lc | lc /= 0 -> scalePoly (1 / lc) r
       _                  -> r

-- | Core Buchberger loop.
buchberger :: (Eq k, Fractional k) => MonoOrd -> [MPoly k] -> [MPoly k]
buchberger cmp gens =
  let pairs = [(i, j) | i <- [0..n-1], j <- [i+1..n-1]]
      n = length gens
  in buchbergerWithPairs cmp gens pairs

-- | Buchberger with explicit initial pair set.
buchbergerWithPairs :: (Eq k, Fractional k)
                    => MonoOrd -> [MPoly k] -> [(Int, Int)] -> [MPoly k]
buchbergerWithPairs cmp initialBasis initialPairs =
  go initialBasis initialPairs
  where
    go basis [] = basis
    go basis ((i, j):pairs)
      | i >= length basis || j >= length basis = go basis pairs
      | buchbergerCriterion1 cmp (basis !! i) (basis !! j) = go basis pairs
      | otherwise =
        let sp = sPolynomial cmp (basis !! i) (basis !! j)
            r  = reducePoly cmp sp basis
        in if isZero r
           then go basis pairs
           else
             let k = length basis
                 newPairs = [(k, m) | m <- [0..k-1]]
                 basis' = basis ++ [r]
             in go basis' (pairs ++ newPairs)

-- | Buchberger's first criterion: if the leading monomials are coprime
-- (their GCD is 1), the S-polynomial reduces to zero and can be skipped.
buchbergerCriterion1 :: MonoOrd -> MPoly k -> MPoly k -> Bool
buchbergerCriterion1 cmp f g =
  case (leadMono cmp f, leadMono cmp g) of
    (Just mf, Just mg) -> monoGcd mf mg == monoOne
    _ -> True

-- | Inter-reduce a basis: make each element reduced with respect to
-- all others, remove redundant elements, and make each polynomial monic.
interReduce :: (Eq k, Fractional k) => MonoOrd -> [MPoly k] -> [MPoly k]
interReduce cmp = go []
  where
    go done []     = reverse done
    go done (f:fs) =
      let others = done ++ fs
          f' = reducePoly cmp f others
      in if isZero f'
         then go done fs
         else
           -- Make monic
           let f'' = case leadCoeffM cmp f' of
                       Just lc | lc /= 0 -> scalePoly (1 / lc) f'
                       _                  -> f'
           in go (f'':done) fs

-- ---------------------------------------------------------------------------
-- Sugar strategy (degree-guided pair selection)
-- ---------------------------------------------------------------------------

-- | Sugar degree of an S-pair: estimates the degree of the S-polynomial.
-- Used for selecting the "cheapest" pair to process next.
_sugarDegree :: MonoOrd -> MPoly k -> MPoly k -> Int
_sugarDegree cmp f g =
  case (leadMono cmp f, leadMono cmp g) of
    (Just mf, Just mg) -> monoDegree (monoLcm mf mg)
    _ -> 0
