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
import Surd.Polynomial.TragerFactoring (factorSFOverExtension, factorSFOverExtensionK)
import Surd.Field.Extension
import Surd.Radical.Eval (evalComplex)
import Data.Complex (Complex(..))
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
       [(d1, r1), (d2, r2), (d3, r3)] ->
         tryDenestDepth3 n radicand (d1, r1) (d2, r2) (d3, r3)
       _ -> Nothing  -- 4+ radicals not yet supported

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
extractRoot field alpha rad f = extractRootGeneric toExpr f
  where
    toExpr = extElemToRadExpr field alpha rad

-- | Generic root extraction from a polynomial factor of degree 1-4.
-- The @toExpr@ function converts field elements to radical expressions.
extractRootGeneric :: (Eq k, Fractional k)
                   => (k -> RadExpr Rational)
                   -> Poly k
                   -> Maybe (RadExpr Rational)
extractRootGeneric toExpr f
  | degree f == 1 =
      case unPoly f of
        [c, _] -> Just (toExpr (negate c))
        _ -> Nothing
  | degree f == 2 =
      case unPoly f of
        [c, b, _] ->
          let disc = b * b - 4 * c
              sqrtDisc = toExpr disc
              negB = toExpr (negate b)
              r1 = Mul (Inv (Lit 2)) (Add negB (Root 2 sqrtDisc))
              r2 = Mul (Inv (Lit 2)) (Add negB (Neg (Root 2 sqrtDisc)))
          in Just (pickClosestToReal [r1, r2])
        _ -> Nothing
  | degree f == 3 =
      -- Depressed cubic via Cardano
      case unPoly f of
        [d, c, b, _] ->
          -- x³ + bx² + cx + d = 0 (monic)
          -- Depress: x = t - b/3
          let p = c - b*b / 3
              q = d - b*c/3 + 2*b*b*b/27
              shift = Neg (toExpr (b / 3))
              halfQ = toExpr (q / 2)
              innerDisc = q*q/4 + p*p*p/27
              innerDiscE = toExpr innerDisc
              sqrtD = Root 2 innerDiscE
              u = Root 3 (Add (Neg halfQ) sqrtD)
              v = Root 3 (Add (Neg halfQ) (Neg sqrtD))
              -- Three roots via ω = (-1 + √(-3))/2
              omega = Mul (Inv (Lit 2)) (Add (Lit (-1)) (Root 2 (Lit (-3))))
              omega2 = Mul (Inv (Lit 2)) (Add (Lit (-1)) (Neg (Root 2 (Lit (-3)))))
              root0 = Add (Add u v) shift
              root1 = Add (Add (Mul omega u) (Mul omega2 v)) shift
              root2 = Add (Add (Mul omega2 u) (Mul omega u)) shift
          in Just (pickClosestToReal [root0, root1, root2])
        _ -> Nothing
  | degree f == 4 =
      -- Quartic via Ferrari
      case unPoly f of
        [e, d, c, b, _] ->
          -- x⁴ + bx³ + cx² + dx + e = 0 (monic)
          let p = c - 3*b*b/8
              q = d - b*c/2 + b*b*b/8
              r = e - b*d/4 + b*b*c/16 - 3*b*b*b*b/256
              bE = toExpr b
              shiftBack expr = Add expr (Neg (Mul (Inv (Lit 4)) bE))
          in if q == 0
             then -- Biquadratic
               let disc = p*p - 4*r
                   sqrtDisc = Root 2 (toExpr disc)
                   pE = toExpr p
                   t2_1 = Mul (Inv (Lit 2)) (Add (Neg pE) sqrtDisc)
                   t2_2 = Mul (Inv (Lit 2)) (Add (Neg pE) (Neg sqrtDisc))
                   roots = [ shiftBack (Root 2 t2_1)
                           , shiftBack (Neg (Root 2 t2_1))
                           , shiftBack (Root 2 t2_2)
                           , shiftBack (Neg (Root 2 t2_2))
                           ]
               in Just (pickClosestToReal roots)
             else -- General quartic: resolvent cubic y³ - py² - 4ry + (4pr - q²) = 0
               let rcD = 4*p*r - q*q
                   rcC = -4*r
                   rcB = -p
                   -- Solve resolvent cubic (Poly k)
                   rcPoly = mkPoly [rcD, rcC, rcB, 1]
               in case extractRootGeneric toExpr rcPoly of
                    Just yExpr ->
                      let sSquared = Add yExpr (Neg (toExpr p))
                          s = Root 2 sSquared
                          halfY = Mul (Inv (Lit 2)) yExpr
                          qOver2s = Mul (toExpr (q/2)) (Inv s)
                          c1 = Add halfY (Neg qOver2s)
                          c2 = Add halfY qOver2s
                          disc1 = Add sSquared (Mul (Lit (-4)) c1)
                          disc2 = Add sSquared (Mul (Lit (-4)) c2)
                          sqD1 = Root 2 disc1
                          sqD2 = Root 2 disc2
                          t1 = Mul (Inv (Lit 2)) (Add (Neg s) sqD1)
                          t2 = Mul (Inv (Lit 2)) (Add (Neg s) (Neg sqD1))
                          t3 = Mul (Inv (Lit 2)) (Add s sqD2)
                          t4 = Mul (Inv (Lit 2)) (Add s (Neg sqD2))
                          roots = map shiftBack [t1, t2, t3, t4]
                      in Just (pickClosestToReal roots)
                    Nothing -> Nothing
        _ -> Nothing
  | otherwise = Nothing

-- | Pick the root expression closest to a real value (smallest imaginary part).
pickClosestToReal :: [RadExpr Rational] -> RadExpr Rational
pickClosestToReal [] = error "pickClosestToReal: empty list"
pickClosestToReal [r] = r
pickClosestToReal roots =
  let scored = [(r, abs (imagPart (evalComplex r))) | r <- roots]
      imagPart (_ :+ y) = y
  in fst $ foldl1 (\a b -> if snd a <= snd b then a else b) scored

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

-- | Collect distinct radicals from an expression, topologically sorted
-- so that radicals with rational radicands come first, followed by
-- radicals whose radicands depend only on earlier radicals.
collectRadicalsSimple :: RadExpr Rational -> [(Int, RadExpr Rational)]
collectRadicalsSimple = topoSort . nub . go
  where
    go (Lit _)    = []
    go (Neg a)    = go a
    go (Add a b)  = go a ++ go b
    go (Mul a b)  = go a ++ go b
    go (Inv a)    = go a
    go (Pow a _)  = go a
    go (Root n a) = go a ++ [(n, a)]

    topoSort rads = topoGo [] rads
    topoGo sorted [] = sorted
    topoGo sorted remaining =
      let ready = [r | r <- remaining, allRootsIn sorted (snd r)]
          remaining' = filter (`notElem` ready) remaining
      in if null ready
         then sorted ++ remaining  -- can't resolve more; append as-is
         else topoGo (sorted ++ ready) remaining'

    allRootsIn :: [(Int, RadExpr Rational)] -> RadExpr Rational -> Bool
    allRootsIn _ (Lit _) = True
    allRootsIn resolved (Neg a) = allRootsIn resolved a
    allRootsIn resolved (Add a b) = allRootsIn resolved a && allRootsIn resolved b
    allRootsIn resolved (Mul a b) = allRootsIn resolved a && allRootsIn resolved b
    allRootsIn resolved (Inv a) = allRootsIn resolved a
    allRootsIn resolved (Pow a _) = allRootsIn resolved a
    allRootsIn resolved (Root n a) = (n, a) `elem` resolved

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
extractRoot2 field1 _field2 alpha1 alpha2 rad1 rad2 f =
  extractRootGeneric toExpr f
  where
    toExpr = extElemToRadExpr2 field1 alpha1 alpha2 rad1 rad2

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

-- | Denest ⁿ√(expr) where expr involves three radicals.
-- Builds a three-level extension Q(α₁)(α₂)(α₃) and factors x^n - a over it.
tryDenestDepth3 :: Int -> RadExpr Rational
               -> (Int, RadExpr Rational) -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
               -> Maybe (RadExpr Rational)
tryDenestDepth3 n radicand rad1@(d1, r1) rad2@(d2, r2) rad3@(d3, r3) =
  case evalRat r1 of
    Nothing -> Nothing  -- after topo sort, first radicand should be rational
    Just q1 ->
      let -- Level 1: Q(α₁) where α₁^d1 = q1
          mp1 = mkPoly $ [-q1] ++ replicate (d1 - 1) 0 ++ [1]
          field1 = mkExtField mp1 "α₁"
          alpha1 = generator field1
          -- Level 2: Q(α₁)(α₂) where α₂^d2 = r2 evaluated in Q(α₁)
          r2InF1 = evalInK field1 alpha1 rad1 r2
          mp2 = mkPoly $ [negate r2InF1] ++ replicate (d2 - 1) 0 ++ [embed field1 1]
          field2 = mkExtField mp2 "α₂"
          alpha2 = generator field2
          alpha1InF2 = embed field2 alpha1
          -- Level 3: Q(α₁)(α₂)(α₃) where α₃^d3 = r3 evaluated in Q(α₁)(α₂)
          r3InF2 = evalInK2 field1 field2 alpha1InF2 alpha2 rad1 rad2 r3
          mp3 = mkPoly $ [negate r3InF2] ++ replicate (d3 - 1) 0 ++ [embed field2 (embed field1 1)]
          field3 = mkExtField mp3 "α₃"
          alpha3 = generator field3
          alpha1InF3 = embed field3 (embed field2 alpha1)
          alpha2InF3 = embed field3 alpha2
          -- Evaluate radicand in Q(α₁)(α₂)(α₃)
          aInK = evalInK3 field1 field2 field3 alpha1InF3 alpha2InF3 alpha3 rad1 rad2 rad3 radicand
          one3 = embed field3 (embed field2 (embed field1 1))
          xnMinusA = mkPoly $ [negate aInK] ++ replicate (n - 1) 0 ++ [one3]
          -- Factor over Q(α₁)(α₂)(α₃) using recursive Trager
          factorOverK1 = factorSFOverExtension field1
          factorOverK2 = factorSFOverExtensionK factorOverK1 field2
          factors = factorSFOverExtensionK factorOverK2 field3 xnMinusA
      in case [f | f <- factors, degree f > 0, degree f < n] of
           [] -> Nothing
           (f : _) -> extractRoot3 field1 field2 field3
                        alpha1InF3 alpha2InF3 alpha3 rad1 rad2 rad3 f

-- | Extract a radical expression from a factor over Q(α₁)(α₂)(α₃).
extractRoot3 :: ExtField Rational
             -> ExtField (ExtElem Rational)
             -> ExtField (ExtElem (ExtElem Rational))
             -> ExtElem (ExtElem (ExtElem Rational))  -- α₁ lifted
             -> ExtElem (ExtElem (ExtElem Rational))  -- α₂ lifted
             -> ExtElem (ExtElem (ExtElem Rational))  -- α₃
             -> (Int, RadExpr Rational) -> (Int, RadExpr Rational) -> (Int, RadExpr Rational)
             -> Poly (ExtElem (ExtElem (ExtElem Rational)))
             -> Maybe (RadExpr Rational)
extractRoot3 field1 _field2 _field3 alpha1 alpha2 alpha3 rad1 rad2 rad3 f =
  extractRootGeneric toExpr f
  where
    toExpr = extElemToRadExpr3 field1 alpha1 alpha2 alpha3 rad1 rad2 rad3

-- | Convert an ExtElem (ExtElem (ExtElem Rational)) back to a RadExpr.
-- elem = Σᵢ Σⱼ Σₖ cᵢⱼₖ · α₁ⁱ · α₂ʲ · α₃ᵏ
extElemToRadExpr3 :: ExtField Rational
                  -> ExtElem (ExtElem (ExtElem Rational))  -- α₁ lifted
                  -> ExtElem (ExtElem (ExtElem Rational))  -- α₂ lifted
                  -> ExtElem (ExtElem (ExtElem Rational))  -- α₃
                  -> (Int, RadExpr Rational)
                  -> (Int, RadExpr Rational)
                  -> (Int, RadExpr Rational)
                  -> ExtElem (ExtElem (ExtElem Rational))
                  -> RadExpr Rational
extElemToRadExpr3 _field1 _alpha1 _alpha2 _alpha3 (d1, r1) (d2, r2) (d3, r3) (ExtElem (Poly outerCs) _) =
  let rad1Expr = Root d1 r1
      rad2Expr = Root d2 r2
      rad3Expr = Root d3 r3
      -- outerCs: coefficients of α₃, each is ExtElem (ExtElem Rational)
      -- midCs:   coefficients of α₂, each is ExtElem Rational
      -- innerCs: coefficients of α₁, each is Rational
      allTerms = concatMap (\(k, ExtElem (Poly midCs) _) ->
        concatMap (\(j, ExtElem (Poly innerCs) _) ->
          [(c, i, j, k) | (i, c) <- zip [0 :: Int ..] innerCs, c /= 0])
          (zip [0 :: Int ..] midCs))
        (zip [0 :: Int ..] outerCs)
  in case allTerms of
       [] -> Lit 0
       _  -> foldl1 Add [mkTerm3 c i j k rad1Expr rad2Expr rad3Expr | (c, i, j, k) <- allTerms]
  where
    mkTerm3 c i j k a b d =
      let base = case (i, j, k) of
                   (0, 0, 0) -> Nothing
                   (_, 0, 0) -> Just (powExpr a i)
                   (0, _, 0) -> Just (powExpr b j)
                   (0, 0, _) -> Just (powExpr d k)
                   (_, _, 0) -> Just (Mul (powExpr a i) (powExpr b j))
                   (_, 0, _) -> Just (Mul (powExpr a i) (powExpr d k))
                   (0, _, _) -> Just (Mul (powExpr b j) (powExpr d k))
                   _         -> Just (Mul (Mul (powExpr a i) (powExpr b j)) (powExpr d k))
      in case base of
           Nothing -> Lit c
           Just e  -> mulCoeff c e

    mulCoeff 1 e = e
    mulCoeff c e = Mul (Lit c) e

    powExpr e 1 = e
    powExpr e k = Pow e k

-- | Evaluate a RadExpr in Q(α₁)(α₂)(α₃) given three radicals.
evalInK3 :: ExtField Rational
         -> ExtField (ExtElem Rational)
         -> ExtField (ExtElem (ExtElem Rational))
         -> ExtElem (ExtElem (ExtElem Rational))  -- α₁ lifted
         -> ExtElem (ExtElem (ExtElem Rational))  -- α₂ lifted
         -> ExtElem (ExtElem (ExtElem Rational))  -- α₃
         -> (Int, RadExpr Rational)
         -> (Int, RadExpr Rational)
         -> (Int, RadExpr Rational)
         -> RadExpr Rational
         -> ExtElem (ExtElem (ExtElem Rational))
evalInK3 field1 field2 field3 alpha1 alpha2 alpha3 rad1 rad2 rad3 = go
  where
    go (Lit r)    = embed field3 (embed field2 (embed field1 r))
    go (Neg a)    = negate (go a)
    go (Add a b)  = go a + go b
    go (Mul a b)  = go a * go b
    go (Inv a)    = recip (go a)
    go (Root m a)
      | (m, a) == rad1 = alpha1
      | (m, a) == rad2 = alpha2
      | (m, a) == rad3 = alpha3
      | otherwise     = error "evalInK3: unexpected radical"
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
