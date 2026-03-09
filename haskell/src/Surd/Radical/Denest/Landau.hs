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
import Surd.Polynomial.TragerFactoring (factorSFOverExtension)
import Surd.Field.Extension
import Surd.Radical.Eval (eval)

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
       _ -> Nothing  -- multi-radical denesting not yet implemented

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
    dedup = foldr (\x acc -> if x `elem` acc then acc else acc ++ [x]) []

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

-- | Try to evaluate as a rational.
evalRat :: RadExpr Rational -> Maybe Rational
evalRat (Lit r) = Just r
evalRat (Neg a) = negate <$> evalRat a
evalRat (Add a b) = (+) <$> evalRat a <*> evalRat b
evalRat (Mul a b) = (*) <$> evalRat a <*> evalRat b
evalRat (Inv a) = recip <$> evalRat a
evalRat (Pow a n) = (^^ n) <$> evalRat a
evalRat (Root _ _) = Nothing
