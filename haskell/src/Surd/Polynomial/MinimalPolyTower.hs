-- | Fast minimal polynomial computation via extension tower.
--
-- Instead of computing resultants bottom-up at every AST node (which
-- causes exponential blowup when radicals are shared), this module:
--
-- 1. Collects distinct radicals in the expression
-- 2. Builds an extension tower Q(α₁)(α₂)...
-- 3. Evaluates the expression in the tower
-- 4. Computes the annihilating polynomial via iterated norms
--    (one resultant per tower level)
-- 5. Factors over Q to get the minimal polynomial
--
-- For cos(2π/7) (degree 3), the old approach takes minutes because
-- it computes resultants of degree-6+ polynomials at every Add/Mul node.
-- The tower approach identifies 2-3 radicals and does 2-3 small resultants.
module Surd.Polynomial.MinimalPolyTower
  ( minimalPolyTower
  , annihilatingPolyTower
  , collectRadicals
  ) where

import Surd.Types
import Surd.Polynomial.Univariate
import Surd.Polynomial.Factoring (factorSquareFree)
import Surd.Polynomial.MinimalPoly (polyResultant)
import Surd.Field.Extension
import Surd.Radical.Eval (eval, evalComplex)
import Surd.Internal.PSLQ (findMinPoly)
import Data.Complex (realPart)
import Surd.Radical.Normalize (normalize)
import Data.List (nub)

-- | Compute the minimal polynomial of a radical expression over Q,
-- using the extension tower approach.
minimalPolyTower :: RadExpr Rational -> Poly Rational
minimalPolyTower expr =
  let ann = annihilatingPolyTower expr
      vd = eval expr :: Double
      v = if isNaN vd then realPart (evalComplex expr) else vd
      factors = factorSquareFree (monicPoly ann)
  in case factors of
       [] -> ann
       _  -> monicPoly $ pickClosest v factors

-- | Compute an annihilating polynomial (not necessarily minimal) via
-- the extension tower approach.
annihilatingPolyTower :: RadExpr Rational -> Poly Rational
annihilatingPolyTower expr =
  let expr' = normalize expr
      radicals = collectRadicals expr'
  in case radicals of
       [] ->
         -- Expression is purely rational
         case evalRational expr' of
           Just r  -> mkPoly [-r, 1]
           Nothing -> error "annihilatingPolyTower: no radicals but not rational?"
       _ ->
         -- Build tower and compute
         computeViaTower expr' radicals

-- | Collect all distinct radical subexpressions, ordered leaves-first.
-- Each entry is (root degree, radicand expression).
-- Uses structural equality to identify shared radicals.
-- Radicands are normalized to improve sharing.
collectRadicals :: RadExpr Rational -> [(Int, RadExpr Rational)]
collectRadicals = dedup . go . normalize
  where
    go (Lit _)    = []
    go (Neg a)    = go a
    go (Add a b)  = go a ++ go b
    go (Mul a b)  = go a ++ go b
    go (Inv a)    = go a
    go (Pow a _)  = go a
    go (Root n a) = go a ++ [(n, a)]  -- a is already normalized by outer normalize

    dedup = nub

-- | Try to evaluate a RadExpr as a pure rational (no radicals).
evalRational :: RadExpr Rational -> Maybe Rational
evalRational (Lit r)    = Just r
evalRational (Neg a)    = negate <$> evalRational a
evalRational (Add a b)  = (+) <$> evalRational a <*> evalRational b
evalRational (Mul a b)  = (*) <$> evalRational a <*> evalRational b
evalRational (Inv a)    = recip <$> evalRational a
evalRational (Pow a n)  = (^^ n) <$> evalRational a
evalRational (Root _ _) = Nothing

-- | Build the tower, evaluate the expression, and compute the
-- annihilating polynomial via iterated norms.
computeViaTower :: RadExpr Rational -> [(Int, RadExpr Rational)] -> Poly Rational
computeViaTower expr radicals =
  case length radicals of
    0 -> error "computeViaTower: no radicals"
    _ | length radicals > 3 ->
        -- For many radicals, try numerical approach first (much faster)
        let towerDeg = product (map fst radicals)
        in case numericMinPoly expr towerDeg of
             Just p  -> p
             Nothing -> computeIterative expr radicals
      | otherwise -> computeIterative expr radicals

-- | Iterative computation using a single level of ExtElem at a time.
--
-- Process radicals from innermost to outermost. At each step:
-- 1. We have the target expressed as an element of Q(α₁,...,αₖ)
-- 2. We eliminate αₖ by computing the norm (resultant with its minpoly)
-- 3. This gives us a polynomial whose root is the target, over Q(α₁,...,αₖ₋₁)
--
-- Since we process one level at a time, we always work with
-- ExtElem Rational (single-level extension).
computeIterative :: RadExpr Rational -> [(Int, RadExpr Rational)] -> Poly Rational
computeIterative expr radicals =
  -- Build mapping from radical to its position
  let numRads = length radicals
  in if numRads == 1
     then case radicals of
            (r1:_) -> computeDepth1 expr r1
            _      -> error "impossible"
     else if numRads == 2
     then computeDepth2 expr (radicals !! 0) (radicals !! 1)
     else if numRads == 3
     then computeDepth3 expr (radicals !! 0) (radicals !! 1) (radicals !! 2)
     else -- For deeper towers, fall back to pairwise resultant
          -- (still better than per-node because we share radicals)
          computeDeepFallback expr radicals

-- | Depth 1: one radical. Q(α) where α = ⁿ√r.
computeDepth1 :: RadExpr Rational -> (Int, RadExpr Rational) -> Poly Rational
computeDepth1 expr (n, radicand) =
  case evalRational radicand of
    Nothing -> error "computeDepth1: radicand is not rational"
    Just r ->
      let -- Build Q(α) with α^n = r
          minpoly = mkPoly $ [-r] ++ replicate (n - 1) 0 ++ [1]
          field = mkExtField minpoly "α"
          alpha = generator field
          -- Evaluate expression in Q(α)
          elem' = evalInExt field alpha (n, radicand) expr
          -- f(x) = x - elem
          fPoly = mkPolyExt field [negate elem', embed field 1]
          -- Norm: Res_t(minpoly(t), f(x, t))
      in normDown1 field fPoly

-- | Depth 2: two radicals. Inner α₁ = ⁿ¹√r₁, outer α₂ = ⁿ²√r₂(α₁).
computeDepth2 :: RadExpr Rational
              -> (Int, RadExpr Rational)  -- inner radical
              -> (Int, RadExpr Rational)  -- outer radical
              -> Poly Rational
computeDepth2 expr rad1@(n1, radicand1) rad2@(n2, radicand2) =
  case evalRational radicand1 of
    Nothing -> error "computeDepth2: inner radicand not rational"
    Just r1 ->
      let -- Level 1: Q(α₁) with α₁^n1 = r1
          mp1 = mkPoly $ [-r1] ++ replicate (n1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1
          -- Evaluate radicand2 in Q(α₁)
          r2InField1 = evalInExt field1 alpha1 rad1 radicand2
          -- Level 2: Q(α₁)(α₂) with α₂^n2 = r2InField1
          mp2 = mkPoly $ [negate r2InField1] ++ replicate (n2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          -- Evaluate expression in Q(α₁)(α₂)
          -- Need to lift alpha1 into Q(α₁)(α₂)
          alpha1Lifted = embed field2 alpha1
          elem' = evalInExt2 field1 field2 alpha1Lifted alpha2 rad1 rad2 expr
          -- f(x) = x - elem in Q(α₁)(α₂)[x]
          fPoly2 = mkPoly [negate elem', embed field2 (embed field1 1)]
          -- Norm down: level 2 gives Poly (ExtElem Rational),
          -- then level 1 gives Poly Rational.
      in normDown1 field1 (normDown1 field2 fPoly2)

-- | Depth 3: three radicals.
computeDepth3 :: RadExpr Rational
              -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational)
              -> Poly Rational
computeDepth3 expr rad1@(n1, radicand1) rad2@(n2, radicand2) rad3@(n3, radicand3) =
  case evalRational radicand1 of
    Nothing -> error "computeDepth3: innermost radicand not rational"
    Just r1 ->
      let mp1 = mkPoly $ [-r1] ++ replicate (n1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1

          r2InF1 = evalInExt field1 alpha1 rad1 radicand2
          mp2 = mkPoly $ [negate r2InF1] ++ replicate (n2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1

          r3InF2 = evalInExt2 field1 field2 alpha1InF2 alpha2 rad1 rad2 radicand3
          mp3 = mkPoly $ [negate r3InF2] ++ replicate (n3 - 1) 0 ++ [embed field2 (embed field1 1)]
          field3 = mkExtField mp3 "α₃"
          alpha3 = generator field3
          alpha1InF3 = embed field3 (embed field2 alpha1)
          alpha2InF3 = embed field3 alpha2

          elem' = evalInExt3 field1 field2 field3 alpha1InF3 alpha2InF3 alpha3 rad1 rad2 rad3 expr
          fPoly3 = mkPoly [negate elem', embed field3 (embed field2 (embed field1 1))]
      in normDown1 field1 (normDown1 field2 (normDown1 field3 fPoly3))

-- | Fallback for depth > 3: build a dependency chain of radicals.
-- A radical R₂ "depends on" R₁ if R₁'s radicand contains R₁.
-- We order radicals so dependencies come first, then build the
-- tower using only the chain of dependent radicals.
computeDeepFallback :: RadExpr Rational -> [(Int, RadExpr Rational)] -> Poly Rational
computeDeepFallback expr radicals =
  -- Build a dependency chain: start with leaf radicals (rational radicands),
  -- then add radicals whose radicands only involve previously-added radicals.
  case buildChain radicals of
    []                       -> simpleAnnihilating expr
    [r1]                     -> computeDepth1 expr r1
    [r1, r2]                 -> computeDepth2 expr r1 r2
    [r1, r2, r3]             -> computeDepth3 expr r1 r2 r3
    [r1, r2, r3, r4]         -> computeDepth4 expr r1 r2 r3 r4
    [r1, r2, r3, r4, r5]     -> computeDepth5 expr r1 r2 r3 r4 r5
    [r1, r2, r3, r4, r5, r6] -> computeDepth6 expr r1 r2 r3 r4 r5 r6
    _                        -> simpleAnnihilating expr  -- truly deep tower, fallback

-- | Build dependency chain: order radicals so that each radical's
-- radicand only uses radicals earlier in the chain.
buildChain :: [(Int, RadExpr Rational)] -> [(Int, RadExpr Rational)]
buildChain rads = go [] rads
  where
    go chain [] = chain
    go chain remaining =
      let ready = [r | r <- remaining, radicandResolvable chain (snd r)]
          remaining' = filter (`notElem` ready) remaining
      in if null ready then chain
         else go (chain ++ ready) remaining'

    radicandResolvable chain = allRootsIn (map snd chain)

    allRootsIn :: [RadExpr Rational] -> RadExpr Rational -> Bool
    allRootsIn _ (Lit _) = True
    allRootsIn resolved (Neg a) = allRootsIn resolved a
    allRootsIn resolved (Add a b) = allRootsIn resolved a && allRootsIn resolved b
    allRootsIn resolved (Mul a b) = allRootsIn resolved a && allRootsIn resolved b
    allRootsIn resolved (Inv a) = allRootsIn resolved a
    allRootsIn resolved (Pow a _) = allRootsIn resolved a
    allRootsIn resolved (Root _ a) = a `elem` resolved && allRootsIn resolved a

-- | Simple annihilating polynomial (non-tower, for fallback).
-- Same as the original annihilatingPoly but defined here to avoid circular imports.
simpleAnnihilating :: RadExpr Rational -> Poly Rational
simpleAnnihilating (Lit r) = mkPoly [-r, 1]
simpleAnnihilating (Neg e) =
  let p = simpleAnnihilating e
  in mkPoly $ zipWith (\i c -> if odd (i :: Int) then negate c else c) [0..] (unPoly p)
simpleAnnihilating (Add a b) =
  composedSumLocal (simpleAnnihilating a) (simpleAnnihilating b)
simpleAnnihilating (Mul a b) =
  composedProductLocal (simpleAnnihilating a) (simpleAnnihilating b)
simpleAnnihilating (Inv e) =
  let p = simpleAnnihilating e
  in mkPoly (reverse (unPoly p))
simpleAnnihilating (Root n (Lit r)) =
  mkPoly $ [-r] ++ replicate (n - 1) 0 ++ [1]
simpleAnnihilating (Root n e) =
  substituteXN' n (simpleAnnihilating e)
simpleAnnihilating (Pow e n)
  | n >= 0 = annihPow (simpleAnnihilating e) n
  | otherwise = annihPow (mkPoly (reverse (unPoly (simpleAnnihilating e)))) (negate n)

-- | Norm down one level: given f(x) ∈ K(α)[x], compute
-- N(x) = Res_t(m(t), f_lifted(x, t)) ∈ K[x].
--
-- Uses evaluation+interpolation: evaluate at deg(f)*deg(m)+1 points.
normDown1 :: (Eq k, Fractional k)
          => ExtField k -> Poly (ExtElem k) -> Poly k
normDown1 field f =
  let df = degree f
      dm = extDegree field
      resultDeg = df * dm
      m = genMinPoly field
      -- We need to evaluate N(x₀) = Res_t(m(t), f_lifted(x₀, t))
      -- at rational points. But x₀ must be in k, not necessarily Rational.
      -- For k = Rational, we use fromInteger.
      -- For k = ExtElem Rational, we use fromInteger (which embeds via sentinel).
      points = [fromInteger i | i <- [0..fromIntegral resultDeg]]
      values = [normAtPt m (evalPolyExt f x0) | (x0 :: k) <- points]
  in lagrangeInterpK (zip points values)

-- | Evaluate f(x₀) where f has ExtElem coefficients and x₀ is in the base field.
-- Returns the result as a Poly k (the element's polynomial representation),
-- which is the "lifted" value f(x₀) viewed as a polynomial in α.
evalPolyExt :: (Eq k, Fractional k) => Poly (ExtElem k) -> k -> Poly k
evalPolyExt (Poly []) _ = zeroPoly
evalPolyExt (Poly cs) x0 =
  -- f(x₀) = c_0 + c_1*x₀ + c_2*x₀² + ...
  -- Each c_i is an ExtElem k = polynomial in α
  -- f(x₀) is also a polynomial in α
  let result = foldl (\acc (i, ExtElem p _) ->
                 addPoly acc (scalePoly (x0 ^ i) p))
               zeroPoly
               (zip [0 :: Int ..] cs)
  in result

-- | Compute Res_t(m(t), g(t)) where both are univariate in t over k.
-- For k = Rational, this is polyResultant.
-- For k = ExtElem Rational, we use the same Euclidean resultant algorithm.
normAtPt :: (Eq k, Fractional k) => Poly k -> Poly k -> k
normAtPt m g = polyResultantK m g

-- | Resultant of two polynomials over any field k.
polyResultantK :: (Eq k, Fractional k) => Poly k -> Poly k -> k
polyResultantK f g
  | degree f < 0 || degree g < 0 = 0
  | degree g == 0 = case leadCoeff g of
      Just c  -> c ^ degree f
      Nothing -> 0
  | otherwise =
      let (_, r) = divModPoly f g
          df = degree f
          dg = degree g
          lg = case leadCoeff g of Just c -> c; Nothing -> 1
          dr = degree r
          sign = if odd (df * dg) then -1 else 1
      in if degree r < 0
         then 0
         else sign * (lg ^ (df - dr)) * polyResultantK g r

-- | Lagrange interpolation over any field k.
lagrangeInterpK :: (Eq k, Fractional k) => [(k, k)] -> Poly k
lagrangeInterpK points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (recip (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Evaluate a RadExpr in Q(α) given one radical.
evalInExt :: ExtField Rational
          -> ExtElem Rational   -- α (the generator)
          -> (Int, RadExpr Rational)  -- the radical this α represents
          -> RadExpr Rational   -- expression to evaluate
          -> ExtElem Rational
evalInExt field alpha rad = go
  where
    go (Lit r)    = embed field r
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a) = if (n, a) == rad then alpha
                    else error $ "evalInExt: unexpected radical Root " ++ show n
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Evaluate a RadExpr in Q(α₁)(α₂) given two radicals.
evalInExt2 :: ExtField Rational
           -> ExtField (ExtElem Rational)
           -> ExtElem (ExtElem Rational)  -- α₁ lifted
           -> ExtElem (ExtElem Rational)  -- α₂
           -> (Int, RadExpr Rational)     -- radical 1
           -> (Int, RadExpr Rational)     -- radical 2
           -> RadExpr Rational
           -> ExtElem (ExtElem Rational)
evalInExt2 field1 field2 alpha1 alpha2 rad1 rad2 = go
  where
    go (Lit r)    = embed field2 (embed field1 r)
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad1 = alpha1
      | (n, a) == rad2 = alpha2
      | otherwise = error $ "evalInExt2: unexpected radical"
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Evaluate a RadExpr in Q(α₁)(α₂)(α₃) given three radicals.
evalInExt3 :: ExtField Rational
           -> ExtField (ExtElem Rational)
           -> ExtField (ExtElem (ExtElem Rational))
           -> ExtElem (ExtElem (ExtElem Rational))  -- α₁
           -> ExtElem (ExtElem (ExtElem Rational))  -- α₂
           -> ExtElem (ExtElem (ExtElem Rational))  -- α₃
           -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational)
           -> RadExpr Rational
           -> ExtElem (ExtElem (ExtElem Rational))
evalInExt3 field1 _field2 _field3 alpha1 alpha2 alpha3 rad1 rad2 rad3 = go
  where
    one = embed _field3 (embed _field2 (embed field1 1))
    go (Lit r)    = fromRational r * one
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad1 = alpha1
      | (n, a) == rad2 = alpha2
      | (n, a) == rad3 = alpha3
      | otherwise = error "evalInExt3: unexpected radical"
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- Type aliases for readability
type E1 = ExtElem Rational
type E2 = ExtElem E1
type E3 = ExtElem E2
type E4 = ExtElem E3
type E5 = ExtElem E4
type E6 = ExtElem E5

-- | Depth 4: four radicals.
computeDepth4 :: RadExpr Rational
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> Poly Rational
computeDepth4 expr rad1@(n1, radicand1) rad2@(n2, radicand2) rad3@(n3, radicand3) rad4@(n4, radicand4) =
  case evalRational radicand1 of
    Nothing -> error "computeDepth4: innermost radicand not rational"
    Just r1 ->
      let mp1 = mkPoly $ [-r1] ++ replicate (n1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1

          r2InF1 = evalInExt field1 alpha1 rad1 radicand2
          mp2 = mkPoly $ [negate r2InF1] ++ replicate (n2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1

          r3InF2 = evalInExt2 field1 field2 alpha1InF2 alpha2 rad1 rad2 radicand3
          mp3 = mkPoly $ [negate r3InF2] ++ replicate (n3 - 1) 0 ++ [embed field2 (embed field1 1)]
          field3 = mkExtField mp3 "α₃"
          alpha3 = generator field3
          alpha1InF3 = embed field3 (embed field2 alpha1)
          alpha2InF3 = embed field3 alpha2

          r4InF3 = evalInExt3 field1 field2 field3 alpha1InF3 alpha2InF3 alpha3 rad1 rad2 rad3 radicand4
          mp4 = mkPoly $ [negate r4InF3] ++ replicate (n4 - 1) 0 ++ [embed field3 (embed field2 (embed field1 1))]
          field4 = mkExtField mp4 "α₄"
          alpha4 = generator field4
          alpha1InF4 = embed field4 (embed field3 (embed field2 alpha1))
          alpha2InF4 = embed field4 (embed field3 alpha2)
          alpha3InF4 = embed field4 alpha3

          elem' = evalInExt4 field1 field2 field3 field4
                    alpha1InF4 alpha2InF4 alpha3InF4 alpha4
                    rad1 rad2 rad3 rad4 expr
          fPoly4 = mkPoly [negate elem', embed field4 (embed field3 (embed field2 (embed field1 1)))]
      in normDown1 field1 (normDown1 field2 (normDown1 field3 (normDown1 field4 fPoly4)))

-- | Evaluate a RadExpr in Q(α₁)(α₂)(α₃)(α₄) given four radicals.
evalInExt4 :: ExtField Rational -> ExtField E1 -> ExtField E2 -> ExtField E3
           -> E4 -> E4 -> E4 -> E4
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> RadExpr Rational -> E4
evalInExt4 field1 field2 field3 field4
           alpha1 alpha2 alpha3 alpha4
           rad1 rad2 rad3 rad4 = go
  where
    one = embed field4 (embed field3 (embed field2 (embed field1 1)))
    go (Lit r)    = fromRational r * one
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad1 = alpha1
      | (n, a) == rad2 = alpha2
      | (n, a) == rad3 = alpha3
      | (n, a) == rad4 = alpha4
      | otherwise = error "evalInExt4: unexpected radical"
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Depth 5: five radicals.
computeDepth5 :: RadExpr Rational
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational)
              -> Poly Rational
computeDepth5 expr rad1@(n1, radicand1) rad2@(n2, radicand2) rad3@(n3, radicand3) rad4@(n4, radicand4) rad5@(n5, radicand5) =
  case evalRational radicand1 of
    Nothing -> error "computeDepth5: innermost radicand not rational"
    Just r1 ->
      let mp1 = mkPoly $ [-r1] ++ replicate (n1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1

          r2InF1 = evalInExt field1 alpha1 rad1 radicand2
          mp2 = mkPoly $ [negate r2InF1] ++ replicate (n2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1

          r3InF2 = evalInExt2 field1 field2 alpha1InF2 alpha2 rad1 rad2 radicand3
          mp3 = mkPoly $ [negate r3InF2] ++ replicate (n3 - 1) 0 ++ [embed field2 (embed field1 1)]
          field3 = mkExtField mp3 "α₃"
          alpha3 = generator field3
          alpha1InF3 = embed field3 (embed field2 alpha1)
          alpha2InF3 = embed field3 alpha2

          r4InF3 = evalInExt3 field1 field2 field3 alpha1InF3 alpha2InF3 alpha3 rad1 rad2 rad3 radicand4
          mp4 = mkPoly $ [negate r4InF3] ++ replicate (n4 - 1) 0 ++ [embed field3 (embed field2 (embed field1 1))]
          field4 = mkExtField mp4 "α₄"
          alpha4 = generator field4
          alpha1InF4 = embed field4 (embed field3 (embed field2 alpha1))
          alpha2InF4 = embed field4 (embed field3 alpha2)
          alpha3InF4 = embed field4 alpha3

          r5InF4 = evalInExt4 field1 field2 field3 field4
                     alpha1InF4 alpha2InF4 alpha3InF4 alpha4
                     rad1 rad2 rad3 rad4 radicand5
          mp5 = mkPoly $ [negate r5InF4] ++ replicate (n5 - 1) 0 ++ [embed field4 (embed field3 (embed field2 (embed field1 1)))]
          field5 = mkExtField mp5 "α₅"
          alpha5 = generator field5
          alpha1InF5 = embed field5 (embed field4 (embed field3 (embed field2 alpha1)))
          alpha2InF5 = embed field5 (embed field4 (embed field3 alpha2))
          alpha3InF5 = embed field5 (embed field4 alpha3)
          alpha4InF5 = embed field5 alpha4

          elem' = evalInExt5 field1 field2 field3 field4 field5
                    alpha1InF5 alpha2InF5 alpha3InF5 alpha4InF5 alpha5
                    rad1 rad2 rad3 rad4 rad5 expr
          fPoly5 = mkPoly [negate elem', embed field5 (embed field4 (embed field3 (embed field2 (embed field1 1))))]
      in normDown1 field1 (normDown1 field2 (normDown1 field3 (normDown1 field4 (normDown1 field5 fPoly5))))

-- | Evaluate a RadExpr in Q(α₁)(α₂)(α₃)(α₄)(α₅) given five radicals.
evalInExt5 :: ExtField Rational -> ExtField E1 -> ExtField E2 -> ExtField E3 -> ExtField E4
           -> E5 -> E5 -> E5 -> E5 -> E5
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational)
           -> RadExpr Rational -> E5
evalInExt5 field1 field2 field3 field4 field5
           alpha1 alpha2 alpha3 alpha4 alpha5
           rad1 rad2 rad3 rad4 rad5 = go
  where
    one = embed field5 (embed field4 (embed field3 (embed field2 (embed field1 1))))
    go (Lit r)    = fromRational r * one
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad1 = alpha1
      | (n, a) == rad2 = alpha2
      | (n, a) == rad3 = alpha3
      | (n, a) == rad4 = alpha4
      | (n, a) == rad5 = alpha5
      | otherwise = error "evalInExt5: unexpected radical"
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Depth 6: six radicals.
computeDepth6 :: RadExpr Rational
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
              -> Poly Rational
computeDepth6 expr rad1@(n1, radicand1) rad2@(n2, radicand2) rad3@(n3, radicand3) rad4@(n4, radicand4) rad5@(n5, radicand5) rad6@(n6, radicand6) =
  case evalRational radicand1 of
    Nothing -> error "computeDepth6: innermost radicand not rational"
    Just r1 ->
      let mp1 = mkPoly $ [-r1] ++ replicate (n1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1

          r2InF1 = evalInExt field1 alpha1 rad1 radicand2
          mp2 = mkPoly $ [negate r2InF1] ++ replicate (n2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1

          r3InF2 = evalInExt2 field1 field2 alpha1InF2 alpha2 rad1 rad2 radicand3
          mp3 = mkPoly $ [negate r3InF2] ++ replicate (n3 - 1) 0 ++ [embed field2 (embed field1 1)]
          field3 = mkExtField mp3 "α₃"
          alpha3 = generator field3
          alpha1InF3 = embed field3 (embed field2 alpha1)
          alpha2InF3 = embed field3 alpha2

          r4InF3 = evalInExt3 field1 field2 field3 alpha1InF3 alpha2InF3 alpha3 rad1 rad2 rad3 radicand4
          mp4 = mkPoly $ [negate r4InF3] ++ replicate (n4 - 1) 0 ++ [embed field3 (embed field2 (embed field1 1))]
          field4 = mkExtField mp4 "α₄"
          alpha4 = generator field4
          alpha1InF4 = embed field4 (embed field3 (embed field2 alpha1))
          alpha2InF4 = embed field4 (embed field3 alpha2)
          alpha3InF4 = embed field4 alpha3

          r5InF4 = evalInExt4 field1 field2 field3 field4
                     alpha1InF4 alpha2InF4 alpha3InF4 alpha4
                     rad1 rad2 rad3 rad4 radicand5
          mp5 = mkPoly $ [negate r5InF4] ++ replicate (n5 - 1) 0 ++ [embed field4 (embed field3 (embed field2 (embed field1 1)))]
          field5 = mkExtField mp5 "α₅"
          alpha5 = generator field5
          alpha1InF5 = embed field5 (embed field4 (embed field3 (embed field2 alpha1)))
          alpha2InF5 = embed field5 (embed field4 (embed field3 alpha2))
          alpha3InF5 = embed field5 (embed field4 alpha3)
          alpha4InF5 = embed field5 alpha4

          r6InF5 = evalInExt5 field1 field2 field3 field4 field5
                     alpha1InF5 alpha2InF5 alpha3InF5 alpha4InF5 alpha5
                     rad1 rad2 rad3 rad4 rad5 radicand6
          mp6 = mkPoly $ [negate r6InF5] ++ replicate (n6 - 1) 0 ++ [embed field5 (embed field4 (embed field3 (embed field2 (embed field1 1))))]
          field6 = mkExtField mp6 "α₆"
          alpha6 = generator field6
          alpha1InF6 = embed field6 (embed field5 (embed field4 (embed field3 (embed field2 alpha1))))
          alpha2InF6 = embed field6 (embed field5 (embed field4 (embed field3 alpha2)))
          alpha3InF6 = embed field6 (embed field5 (embed field4 alpha3))
          alpha4InF6 = embed field6 (embed field5 alpha4)
          alpha5InF6 = embed field6 alpha5

          elem' = evalInExt6 field1 field2 field3 field4 field5 field6
                    alpha1InF6 alpha2InF6 alpha3InF6 alpha4InF6 alpha5InF6 alpha6
                    rad1 rad2 rad3 rad4 rad5 rad6 expr
          fPoly6 = mkPoly [negate elem', embed field6 (embed field5 (embed field4 (embed field3 (embed field2 (embed field1 1)))))]
      in normDown1 field1 (normDown1 field2 (normDown1 field3 (normDown1 field4 (normDown1 field5 (normDown1 field6 fPoly6)))))

-- | Evaluate a RadExpr in Q(α₁)(α₂)(α₃)(α₄)(α₅)(α₆) given six radicals.
evalInExt6 :: ExtField Rational -> ExtField E1 -> ExtField E2 -> ExtField E3 -> ExtField E4 -> ExtField E5
           -> E6 -> E6 -> E6 -> E6 -> E6 -> E6
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
           -> RadExpr Rational -> E6
evalInExt6 field1 field2 field3 field4 field5 field6
           alpha1 alpha2 alpha3 alpha4 alpha5 alpha6
           rad1 rad2 rad3 rad4 rad5 rad6 = go
  where
    one = embed field6 (embed field5 (embed field4 (embed field3 (embed field2 (embed field1 1)))))
    go (Lit r)    = fromRational r * one
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad1 = alpha1
      | (n, a) == rad2 = alpha2
      | (n, a) == rad3 = alpha3
      | (n, a) == rad4 = alpha4
      | (n, a) == rad5 = alpha5
      | (n, a) == rad6 = alpha6
      | otherwise = error "evalInExt6: unexpected radical"
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Find the minimal polynomial of a radical expression numerically
-- using the PSLQ algorithm for integer relation finding.
--
-- Evaluates the expression to Double, then uses PSLQ to find an
-- integer relation among (1, α, α², ..., αᵈ) for increasing degrees d.
-- Much more robust than brute-force coefficient search.
numericMinPoly :: RadExpr Rational -> Int -> Maybe (Poly Rational)
numericMinPoly expr maxDeg =
  let alphaD = eval expr :: Double
      -- If Double eval gives NaN (e.g. √(-7)), use Complex eval and take real part
      alpha = if isNaN alphaD
              then realPart (evalComplex expr)
              else alphaD
  in case findMinPoly alpha (min maxDeg 20) of
       Just coeffs -> Just (mkPoly (map fromIntegral coeffs))
       Nothing -> Nothing

-- | Helper: make a polynomial in Q(α)[x] from coefficient list.
mkPolyExt :: (Eq k, Fractional k) => ExtField k -> [ExtElem k] -> Poly (ExtElem k)
mkPolyExt _ cs = mkPoly cs

-- | Pick the factor whose root is closest to the target value.
pickClosest :: Double -> [Poly Rational] -> Poly Rational
pickClosest _ [f] = f
pickClosest target factors =
  let scored = [(f, minimum $ map (\r -> abs (fromRational r - target)) (approxRootsLocal f)) | f <- factors]
  in fst $ foldl1 (\(f1, d1) (f2, d2) -> if d1 <= d2 then (f1, d1) else (f2, d2)) scored

-- | Find approximate real roots.
approxRootsLocal :: Poly Rational -> [Rational]
approxRootsLocal p
  | degree p <= 0 = [0]
  | degree p == 1 =
      case unPoly p of
        [a, b] -> [-a / b]
        _      -> [0]
  | otherwise =
      let bound = rootBoundLocal p
          pts = [fromIntegral i / 10 | i <- [floor (-bound * 10) :: Integer .. ceiling (bound * 10)]]
          signs = [(x, signum (evalPoly p x)) | x <- pts]
          changes = [(x1, x2) | ((x1, s1), (x2, s2)) <- zip signs (drop 1 signs), s1 /= s2, s1 /= 0, s2 /= 0]
          roots = [bisect p lo hi 50 | (lo, hi) <- changes]
      in if null roots then [0] else roots

rootBoundLocal :: Poly Rational -> Rational
rootBoundLocal (Poly []) = 0
rootBoundLocal (Poly cs) =
  let lc = last cs
      ratios = map (\c -> abs (c / lc)) (init cs)
  in 1 + maximum (0 : ratios)

bisect :: Poly Rational -> Rational -> Rational -> Int -> Rational
bisect _ lo _ 0 = lo
bisect p lo hi n' =
  let mid = (lo + hi) / 2
      fmid = evalPoly p mid
      flo = evalPoly p lo
  in if fmid == 0 then mid
     else if signum fmid == signum flo
          then bisect p mid hi (n' - 1)
          else bisect p lo mid (n' - 1)

-- Helpers for simpleAnnihilating

composedSumLocal :: Poly Rational -> Poly Rational -> Poly Rational
composedSumLocal p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromIntegral i | i <- [0..resultDeg]]
      values = [resSumAt p q x | x <- points]
  in lagrangeInterp' (zip points values)

resSumAt :: Poly Rational -> Poly Rational -> Rational -> Rational
resSumAt p q x0 =
  let qShifted = substLinear q x0 (-1)
  in polyResultant p qShifted

substLinear :: Poly Rational -> Rational -> Rational -> Poly Rational
substLinear (Poly []) _ _ = zeroPoly
substLinear (Poly cs) a b =
  let binomial = mkPoly [a, b]
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly binomial acc)) zeroPoly cs

composedProductLocal :: Poly Rational -> Poly Rational -> Poly Rational
composedProductLocal p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromIntegral i | i <- [1..resultDeg + 1]]
      values = [resProdAt p q x | x <- points]
  in lagrangeInterp' (zip points values)

resProdAt :: Poly Rational -> Poly Rational -> Rational -> Rational
resProdAt p q x0 =
  let pRev = scaledRecipAt p x0
  in polyResultant pRev q

scaledRecipAt :: Poly Rational -> Rational -> Poly Rational
scaledRecipAt (Poly cs) x0 =
  let n' = length cs - 1
      newCs = [c * x0 ^ (n' - k) | (k, c) <- zip [0 :: Int ..] (reverse cs)]
  in mkPoly newCs

substituteXN' :: Int -> Poly Rational -> Poly Rational
substituteXN' n' (Poly cs) =
  let indexed = zip [0 :: Int ..] cs
      maxDeg = (length cs - 1) * n'
      result = replicate (maxDeg + 1) 0
      set' xs (i, c) =
        let pos = i * n'
        in take pos xs ++ [c] ++ drop (pos + 1) xs
  in mkPoly $ foldl set' result indexed

annihPow :: Poly Rational -> Int -> Poly Rational
annihPow p n'
  | n' == 0 = mkPoly [-1, 1]
  | n' == 1 = p
  | otherwise =
      let dp = degree p
          points = [fromIntegral i | i <- [0..dp]]
          values = [powResAt p n' x | x <- points]
      in lagrangeInterp' (zip points values)

powResAt :: Poly Rational -> Int -> Rational -> Rational
powResAt p n' x0 =
  let ynMinusX0 = mkPoly $ [-x0] ++ replicate (n' - 1) 0 ++ [1]
  in polyResultant p ynMinusX0

lagrangeInterp' :: [(Rational, Rational)] -> Poly Rational
lagrangeInterp' points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (1 / (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]
