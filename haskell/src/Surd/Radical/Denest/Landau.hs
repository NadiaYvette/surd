-- | Landau's denesting algorithm for nested radicals.
--
-- Given ⁿ√a where a is in a radical extension K/Q, determines whether
-- ⁿ√a can be expressed using radicals of lower nesting depth by
-- factoring x^n - a over K using Trager's algorithm.
--
-- If x^n - a has a factor of degree d < n over K, then ⁿ√a can be
-- expressed using d-th roots instead of n-th roots.
module Surd.Radical.Denest.Landau
  ( denestLandau
  , denestRadical
  ) where

import Surd.Types
import Surd.Polynomial.Univariate
import Surd.Polynomial.Factoring (factorSquareFree)
import Surd.Polynomial.TragerFactoring (factorSFOverExtension, factorSFOverExtensionK)
import Surd.Field.Extension
import Surd.Radical.Eval (eval, evalComplex)
import Data.Complex (realPart)
import Data.List (nub)

-- | Try to denest all radicals in an expression using Landau's algorithm.
-- Applies denesting recursively, bottom-up.
denestLandau :: RadExpr Rational -> RadExpr Rational
denestLandau = go
  where
    go (Lit r)    = Lit r
    go (Neg a)    = Neg (go a)
    go (Add a b)  = Add (go a) (go b)
    go (Mul a b)  = Mul (go a) (go b)
    go (Inv a)    = Inv (go a)
    go (Pow a n)  = Pow (go a) n
    go (Root n a) =
      let a' = go a  -- denest inner first
      in case denestRadical n a' of
           Just simplified -> go simplified  -- recurse on result
           Nothing         -> Root n a'

-- | Try to denest a specific radical: ⁿ√a.
--
-- Strategy:
-- 1. If a is rational, try perfect power extraction
-- 2. Collect radicals in a, build extension K over Q
-- 3. Factor x^n - a over K
-- 4. If a factor of degree d < n exists, extract the simpler root
denestRadical :: Int -> RadExpr Rational -> Maybe (RadExpr Rational)
denestRadical n radicand =
  -- First try simple cases
  case trySimpleDenest n radicand of
    Just r  -> Just r
    Nothing -> tryTragerDenest n radicand

-- | Simple denesting: rational radicands, perfect powers.
trySimpleDenest :: Int -> RadExpr Rational -> Maybe (RadExpr Rational)
trySimpleDenest _n (Lit _r) = Nothing  -- handled by normalize already
trySimpleDenest 2 (Add (Lit _) (Mul (Lit _) (Root 2 (Lit _)))) =
  -- √(a + b√c): classic Borodin denesting
  -- Already handled by Denest.Sqrt, skip here
  Nothing
trySimpleDenest _ _ = Nothing

-- | Try denesting via Trager factoring of x^n - a over the radical extension.
tryTragerDenest :: Int -> RadExpr Rational -> Maybe (RadExpr Rational)
tryTragerDenest n radicand =
  let radicals = collectRadicalsSimple radicand
  in case radicals of
       [] -> Nothing  -- radicand is rational, nothing to denest
       [(deg, rad)] -> tryDenestDepth1 n radicand deg rad
       [(d1, r1), (d2, r2)] -> tryDenestDepth2 n radicand (d1, r1) (d2, r2)
       _ -> Nothing  -- deeper multi-radical denesting not yet implemented

-- | Denest ⁿ√(expr) where expr involves a single radical α = ᵈ√r.
tryDenestDepth1 :: Int -> RadExpr Rational -> Int -> RadExpr Rational -> Maybe (RadExpr Rational)
tryDenestDepth1 n radicand deg rad =
  case evalRat rad of
    Nothing -> Nothing
    Just r ->
      let -- Build K = Q(α) where α^deg = r
          mp = mkPoly $ [-r] ++ replicate (deg - 1) 0 ++ [1]
          field = mkExtField mp "α"
          alpha = generator field
          -- Evaluate radicand in K
          aInK = evalInK field alpha (deg, rad) radicand
          -- Form x^n - a in K[x]
          xnMinusA = mkPoly $ [negate aInK] ++ replicate (n - 1) 0 ++ [embed field 1]
          -- Factor over K
          factors = factorSFOverExtension field xnMinusA
      in case [f | f <- factors, degree f > 0, degree f < n] of
           [] -> Nothing  -- irreducible, no denesting possible
           (f : _) ->
             -- f is a factor of x^n - a of degree d < n
             -- The root of f is an expression involving α of lower radical degree
             extractRoot field alpha (deg, rad) f

-- | Extract a radical expression from a low-degree factor over K.
extractRoot :: ExtField Rational
            -> ExtElem Rational
            -> (Int, RadExpr Rational)
            -> Poly (ExtElem Rational)
            -> Maybe (RadExpr Rational)
extractRoot field alpha rad f
  | degree f == 1 =
      -- f(x) = x + c, root is -c
      case unPoly f of
        [c, _] ->
          let negC = negate c
          in Just (extElemToRadExpr field alpha rad negC)
        _ -> Nothing
  | degree f == 2 =
      -- f(x) = x^2 + bx + c, roots are (-b ± √(b²-4c))/2
      case unPoly f of
        [c, b, _] ->
          let disc = b * b - 4 * c
              -- Pick the root closest to the numerical value
              target = eval (Root (degree f) (extElemToRadExpr field alpha rad (negate c))) :: Double
              sqrtDisc = extElemToRadExpr field alpha rad disc
              negB = extElemToRadExpr field alpha rad (negate b)
              r1 = Mul (Inv (Lit 2)) (Add negB (Root 2 sqrtDisc))
              r2 = Mul (Inv (Lit 2)) (Add negB (Neg (Root 2 sqrtDisc)))
              v1 = eval r1 :: Double
              v2 = eval r2 :: Double
          in if abs (v1 - target) < abs (v2 - target)
             then Just r1
             else Just r2
        _ -> Nothing
  | otherwise = Nothing  -- higher degree factors not yet handled

-- | Convert an ExtElem back to a RadExpr.
extElemToRadExpr :: ExtField Rational
                 -> ExtElem Rational
                 -> (Int, RadExpr Rational)
                 -> ExtElem Rational
                 -> RadExpr Rational
extElemToRadExpr _ _ (deg, rad) (ExtElem (Poly cs) _) =
  let radical = Root deg rad
      -- elem = c_0 + c_1*α + c_2*α² + ...
      terms = [(c, i) | (i, c) <- zip [0 :: Int ..] cs, c /= 0]
  in case terms of
       [] -> Lit 0
       _  -> foldl1 Add [mkTerm c i radical | (c, i) <- terms]
  where
    mkTerm c 0 _ = Lit c
    mkTerm 1 1 a = a
    mkTerm c 1 a = Mul (Lit c) a
    mkTerm 1 i a = Pow a i
    mkTerm c i a = Mul (Lit c) (Pow a i)

-- | Collect distinct radicals from an expression (simple version).
collectRadicalsSimple :: RadExpr Rational -> [(Int, RadExpr Rational)]
collectRadicalsSimple = dedup . go
  where
    go (Lit _)    = []
    go (Neg a)    = go a
    go (Add a b)  = go a ++ go b
    go (Mul a b)  = go a ++ go b
    go (Inv a)    = go a
    go (Pow a _)  = go a
    go (Root n a) = go a ++ [(n, a)]
    dedup = nub

-- | Evaluate a RadExpr in Q(α).
evalInK :: ExtField Rational
        -> ExtElem Rational
        -> (Int, RadExpr Rational)
        -> RadExpr Rational
        -> ExtElem Rational
evalInK field alpha rad = go
  where
    go (Lit r)    = embed field r
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad = alpha
      | otherwise     = error "evalInK: unexpected radical"
    go (Pow a n)
      | n >= 0    = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Denest ⁿ√(expr) where expr involves two radicals α₁ = ᵈ¹√r₁ and α₂ = ᵈ²√r₂.
-- Builds a two-level extension Q(α₁)(α₂) and factors x^n - a over it.
tryDenestDepth2 :: Int -> RadExpr Rational
               -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
               -> Maybe (RadExpr Rational)
tryDenestDepth2 n radicand rad1@(d1, r1) rad2@(d2, r2) =
  case (evalRat r1, evalRat r2) of
    (Just q1, Just q2) ->
      -- Both radicands are rational: build Q(α₁)(α₂)
      let -- Level 1: Q(α₁) where α₁^d1 = q1
          mp1 = mkPoly $ [-q1] ++ replicate (d1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1
          -- Evaluate r2 in Q(α₁) — it's rational, so just embed
          q2InF1 = embed field1 q2
          -- Level 2: Q(α₁)(α₂) where α₂^d2 = q2
          mp2 = mkPoly $ [negate q2InF1] ++ replicate (d2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1
          -- Evaluate radicand in Q(α₁)(α₂)
          aInK = evalInK2 field1 field2 alpha1InF2 alpha2 rad1 rad2 radicand
          -- Form x^n - a in Q(α₁)(α₂)[x]
          one2 = embed field2 (embed field1 1)
          xnMinusA = mkPoly $ [negate aInK] ++ replicate (n - 1) 0 ++ [one2]
          -- Factor over Q(α₁)(α₂) using generalized Trager.
          -- Base field factoring for K = Q(α₁) uses standard Trager.
          factorOverK = factorSFOverExtension field1
          factors = factorSFOverExtensionK factorOverK field2 xnMinusA
      in case [f | f <- factors, degree f > 0, degree f < n] of
           [] -> Nothing
           (f : _) -> extractRoot2 field1 field2 alpha1InF2 alpha2 rad1 rad2 f
    (Just q1, Nothing) ->
      -- r2 depends on α₁: build tower where α₁ comes first
      let mp1 = mkPoly $ [-q1] ++ replicate (d1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1
          -- Evaluate r2 in Q(α₁) — r2 may involve α₁
          r2InF1 = evalInK field1 alpha1 rad1 r2
          mp2 = mkPoly $ [negate r2InF1] ++ replicate (d2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1
          aInK = evalInK2 field1 field2 alpha1InF2 alpha2 rad1 rad2 radicand
          one2 = embed field2 (embed field1 1)
          xnMinusA = mkPoly $ [negate aInK] ++ replicate (n - 1) 0 ++ [one2]
          factorOverK = factorSFOverExtension field1
          factors = factorSFOverExtensionK factorOverK field2 xnMinusA
      in case [f | f <- factors, degree f > 0, degree f < n] of
           [] -> Nothing
           (f : _) -> extractRoot2 field1 field2 alpha1InF2 alpha2 rad1 rad2 f
    _ -> Nothing  -- Can't handle if first radicand depends on second

-- | Extract a radical expression from a factor over Q(α₁)(α₂).
extractRoot2 :: ExtField Rational
             -> ExtField (ExtElem Rational)
             -> ExtElem (ExtElem Rational)  -- α₁ lifted
             -> ExtElem (ExtElem Rational)  -- α₂
             -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
             -> Poly (ExtElem (ExtElem Rational))
             -> Maybe (RadExpr Rational)
extractRoot2 field1 _field2 alpha1 alpha2 rad1 rad2 f
  | degree f == 1 =
      case unPoly f of
        [c, _] ->
          let negC = negate c
          in Just (extElemToRadExpr2 field1 alpha1 alpha2 rad1 rad2 negC)
        _ -> Nothing
  | degree f == 2 =
      case unPoly f of
        [c, b, _] ->
          let disc = b * b - 4 * c
              sqrtDisc = extElemToRadExpr2 field1 alpha1 alpha2 rad1 rad2 disc
              negB = extElemToRadExpr2 field1 alpha1 alpha2 rad1 rad2 (negate b)
              r1' = Mul (Inv (Lit 2)) (Add negB (Root 2 sqrtDisc))
              r2' = Mul (Inv (Lit 2)) (Add negB (Neg (Root 2 sqrtDisc)))
              v1 = realPart (evalComplex r1')
              v2 = realPart (evalComplex r2')
              target = 0 :: Double  -- pick the root with smaller absolute value
          in if abs (v1 - target) < abs (v2 - target)
             then Just r1'
             else Just r2'
        _ -> Nothing
  | otherwise = Nothing

-- | Convert an ExtElem (ExtElem Rational) back to a RadExpr.
-- elem = Σᵢ Σⱼ cᵢⱼ · α₁ⁱ · α₂ʲ
extElemToRadExpr2 :: ExtField Rational
                  -> ExtElem (ExtElem Rational)  -- α₁ lifted
                  -> ExtElem (ExtElem Rational)  -- α₂
                  -> (Int, RadExpr Rational)      -- radical 1
                  -> (Int, RadExpr Rational)      -- radical 2
                  -> ExtElem (ExtElem Rational)
                  -> RadExpr Rational
extElemToRadExpr2 _field1 _alpha1 _alpha2 (d1, r1) (d2, r2) (ExtElem (Poly outerCs) _) =
  -- outerCs are coefficients of α₂: each is an ExtElem Rational (polynomial in α₁)
  let rad1Expr = Root d1 r1
      rad2Expr = Root d2 r2
      allTerms = concatMap (\(j, ExtElem (Poly innerCs) _) ->
        [(c, i, j) | (i, c) <- zip [0 :: Int ..] innerCs, c /= 0])
        (zip [0 :: Int ..] outerCs)
  in case allTerms of
       [] -> Lit 0
       _  -> foldl1 Add [mkTerm2 c i j rad1Expr rad2Expr | (c, i, j) <- allTerms]
  where
    mkTerm2 c 0 0 _ _ = Lit c
    mkTerm2 c i 0 a _ = mulCoeff c (powExpr a i)
    mkTerm2 c 0 j _ b = mulCoeff c (powExpr b j)
    mkTerm2 c i j a b = mulCoeff c (Mul (powExpr a i) (powExpr b j))

    mulCoeff 1 e = e
    mulCoeff c e = Mul (Lit c) e

    powExpr e 1 = e
    powExpr e k = Pow e k

-- | Evaluate a RadExpr in Q(α₁)(α₂) given two radicals.
evalInK2 :: ExtField Rational
         -> ExtField (ExtElem Rational)
         -> ExtElem (ExtElem Rational)  -- α₁ lifted
         -> ExtElem (ExtElem Rational)  -- α₂
         -> (Int, RadExpr Rational)
         -> (Int, RadExpr Rational)
         -> RadExpr Rational
         -> ExtElem (ExtElem Rational)
evalInK2 field1 field2 alpha1 alpha2 rad1 rad2 = go
  where
    go (Lit r)    = embed field2 (embed field1 r)
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root n a)
      | (n, a) == rad1 = alpha1
      | (n, a) == rad2 = alpha2
      | otherwise     = error "evalInK2: unexpected radical"
    go (Pow a m)
      | m >= 0    = go a ^ m
      | otherwise = recip (go a ^ negate m)

-- | Try to evaluate as a rational.
evalRat :: RadExpr Rational -> Maybe Rational
evalRat (Lit r) = Just r
evalRat (Neg a) = negate <$> evalRat a
evalRat (Add a b) = (+) <$> evalRat a <*> evalRat b
evalRat (Mul a b) = (*) <$> evalRat a <*> evalRat b
evalRat (Inv a) = recip <$> evalRat a
evalRat (Pow a n) = (^^ n) <$> evalRat a
evalRat (Root _ _) = Nothing
