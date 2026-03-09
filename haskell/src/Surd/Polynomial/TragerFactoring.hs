-- | Factoring polynomials over algebraic extension fields Q(α)
-- using Trager's algorithm (1976).
--
-- Given f(x) ∈ Q(α)[x] where α is a root of an irreducible m(t) ∈ Q[t],
-- compute the norm N(x) = Res_t(m(t), f(x - s·t)) ∈ Q[x] for suitable s,
-- factor N over Q, and lift factors back via GCD.
module Surd.Polynomial.TragerFactoring
  ( factorOverExtension
  , factorSFOverExtension
  , factorSFOverExtensionK
  , normPoly
  , normPolyK
  ) where

import Surd.Polynomial.Univariate
import Surd.Polynomial.Factoring (factorSquareFree)
import Surd.Polynomial.MinimalPoly (polyResultant)
import Surd.Field.Extension

-- | Factor a polynomial over Q(α) into irreducible factors.
-- Returns a list of (factor, multiplicity) pairs.
factorOverExtension :: ExtField Rational
                    -> Poly (ExtElem Rational)
                    -> [(Poly (ExtElem Rational), Int)]
factorOverExtension field f =
  let sfFactors = squareFreeExt field f
  in concatMap (\(g, m) -> map (\h -> (h, m)) (factorSFOverExtension field g)) sfFactors

-- | Factor a square-free polynomial over Q(α) into irreducible factors.
--
-- Trager's algorithm:
-- 1. Compute norm N(x) = Res_t(m(t), f(x - s·α)) for suitable s
-- 2. Factor N(x) over Q
-- 3. Lift: for each factor g_i of N, compute gcd(f(x), g_i(x + s·α)) in Q(α)[x]
factorSFOverExtension :: ExtField Rational
                      -> Poly (ExtElem Rational)
                      -> [Poly (ExtElem Rational)]
factorSFOverExtension field f
  | degree f <= 0 = []
  | degree f == 1 = [monicPoly f]
  | otherwise = tragerFactor field f 0

tragerFactor :: ExtField Rational
             -> Poly (ExtElem Rational)
             -> Int  -- shift parameter s
             -> [Poly (ExtElem Rational)]
tragerFactor field f s
  | s > 20 = [f]  -- give up, return as-is
  | otherwise =
      let alpha = generator field
          -- f_shifted(x) = f(x + s*α) — shift so norm is square-free
          sAlpha = embed field (fromIntegral s) * alpha
          fShifted = shiftPoly f sAlpha
          -- Compute norm: N(x) = Res_t(m(t), fShifted(x, t))
          n = normPoly field fShifted
      in if not (isSquareFreeQ n)
         then tragerFactor field f (s + 1)
         else
           -- Factor N(x) over Q
           let nFactors = factorSquareFree (monicPoly n)
           in if length nFactors <= 1
              then [monicPoly f]  -- f is irreducible over Q(α)
              else
                -- Lift each factor back
                let lifted = liftFactors field f sAlpha nFactors
                in lifted

-- | Compute the norm of f(x) ∈ Q(α)[x]: N(x) = Res_t(m(t), f_lifted(x, t)).
--
-- f_lifted(x, t) is obtained by replacing each coefficient c ∈ Q(α)
-- (represented as a polynomial in α) with that polynomial in t.
-- Then N(x) = Res_t(m(t), f_lifted(x, t)) ∈ Q[x].
--
-- We compute this via evaluation+interpolation: evaluate at enough
-- rational points x₀, compute Res_t(m(t), f_lifted(x₀, t)) for each,
-- then interpolate. The degree of N is deg(f) * deg(m).
normPoly :: ExtField Rational -> Poly (ExtElem Rational) -> Poly Rational
normPoly field f =
  let df = degree f
      dm = extDegree field
      resultDeg = df * dm
      -- Evaluate at 0, 1, 2, ..., resultDeg
      points = [fromIntegral i | i <- [0..resultDeg]]
      values = [normAtPoint field f x0 | x0 <- points]
  in lagrangeInterp (zip points values)

-- | Evaluate the norm at a specific rational point x₀.
-- Res_t(m(t), f_lifted(x₀, t)) where f_lifted substitutes x₀ for x
-- and represents Q(α) coefficients as polynomials in t.
normAtPoint :: ExtField Rational -> Poly (ExtElem Rational) -> Rational -> Rational
normAtPoint field f x0 =
  let m = genMinPoly field
      -- Evaluate f at x₀: each coefficient is an ExtElem, so f(x₀) gives
      -- a polynomial in α. But we need f_lifted(x₀, t) as a polynomial in t.
      --
      -- f(x) = c_0 + c_1*x + c_2*x^2 + ...
      -- f_lifted(x₀, t) = c_0(t) + c_1(t)*x₀ + c_2(t)*x₀^2 + ...
      --                  = Σ c_i(t) * x₀^i
      -- where c_i(t) is the polynomial representation of the i-th coefficient.
      coeffs = map elemPoly (unPoly f)
      -- f_lifted(x₀, t) = Σ c_i(t) * x₀^i
      fAtX0 = case coeffs of
                [] -> zeroPoly
                _  -> foldl addPoly zeroPoly
                        [scalePoly (x0 ^ i) ci | (i, ci) <- zip [0 :: Int ..] coeffs]
  in polyResultant m fAtX0

-- | Shift a polynomial: f(x + a) where a ∈ Q(α).
shiftPoly :: (Eq k, Fractional k) => Poly k -> k -> Poly k
shiftPoly (Poly []) _ = zeroPoly
shiftPoly (Poly cs) a =
  -- f(x + a) via Horner
  let xPlusA = mkPoly [a, 1]  -- x + a
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly xPlusA acc)) zeroPoly cs

-- | Lift Q-factors back to Q(α)-factors via GCD.
-- For each factor g_i(x) of N(x) over Q, compute gcd(f(x), g_i(x - s·α)) in Q(α)[x].
liftFactors :: ExtField Rational
            -> Poly (ExtElem Rational)  -- f(x)
            -> ExtElem Rational         -- s·α (the shift)
            -> [Poly Rational]          -- factors of N(x) over Q
            -> [Poly (ExtElem Rational)]
liftFactors field f sAlpha qFactors = go f qFactors
  where
    go _ [] = []
    go remaining (g : gs)
      | degree remaining <= 0 = []
      | otherwise =
          -- g_i(x - s·α) as a polynomial in Q(α)[x]
          let gLifted = liftQPoly field g
              gUnshifted = shiftPoly gLifted (negate sAlpha)
              -- gcd(remaining, gUnshifted) in Q(α)[x]
              h = gcdPolyExt field remaining gUnshifted
          in if degree h > 0
             then let (q, _) = divModPolyExt field remaining h
                  in monicPoly h : go q gs
             else go remaining gs

-- | Lift a Q[x] polynomial to Q(α)[x] by embedding coefficients.
liftQPoly :: ExtField Rational -> Poly Rational -> Poly (ExtElem Rational)
liftQPoly field (Poly cs) = Poly (map (embed field) cs)

-- | Check if a rational polynomial is square-free.
isSquareFreeQ :: Poly Rational -> Bool
isSquareFreeQ p =
  let p' = diffPoly p
      g = gcdPoly p p'
  in degree g <= 0

-- | GCD of two polynomials over Q(α).
gcdPolyExt :: ExtField Rational
           -> Poly (ExtElem Rational)
           -> Poly (ExtElem Rational)
           -> Poly (ExtElem Rational)
gcdPolyExt field a b
  | isZeroPolyExt b = monicPoly a
  | otherwise = gcdPolyExt field b (snd $ divModPolyExt field a b)

-- | Division of polynomials over Q(α).
divModPolyExt :: ExtField Rational
              -> Poly (ExtElem Rational)
              -> Poly (ExtElem Rational)
              -> (Poly (ExtElem Rational), Poly (ExtElem Rational))
divModPolyExt _ f g
  -- Use the generic divModPoly, which works because ExtElem has Fractional
  = divModPoly f g

-- | Make a polynomial over Q(α) monic.
monicPolyExt :: ExtField Rational
             -> Poly (ExtElem Rational)
             -> Poly (ExtElem Rational)
monicPolyExt _ = monicPoly

-- | Check if a polynomial over Q(α) is zero.
isZeroPolyExt :: Poly (ExtElem Rational) -> Bool
isZeroPolyExt (Poly cs) = all (== 0) cs

-- | Square-free factorization over Q(α).
squareFreeExt :: ExtField Rational
              -> Poly (ExtElem Rational)
              -> [(Poly (ExtElem Rational), Int)]
squareFreeExt _ = squareFree

-- | Lagrange interpolation.
lagrangeInterp :: [(Rational, Rational)] -> Poly Rational
lagrangeInterp points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (1 / (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Generalized Lagrange interpolation over any field.
lagrangeInterpK :: (Eq k, Fractional k) => [(k, k)] -> Poly k
lagrangeInterpK points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (recip (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Resultant of two polynomials over any field.
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

-- | Compute the norm of f(x) ∈ K(α)[x] over any base field K.
-- N(x) = Res_t(m(t), f_lifted(x, t)) ∈ K[x].
normPolyK :: (Eq k, Fractional k) => ExtField k -> Poly (ExtElem k) -> Poly k
normPolyK field f =
  let df = degree f
      dm = extDegree field
      resultDeg = df * dm
      m = genMinPoly field
      points = [fromInteger i | i <- [0..fromIntegral resultDeg]]
      values = [normAtPointK m (evalPolyExtK f x0) | (x0 :: k) <- points]
  in lagrangeInterpK (zip points values)

-- | Evaluate f(x₀) where f has ExtElem k coefficients and x₀ ∈ k.
evalPolyExtK :: (Eq k, Fractional k) => Poly (ExtElem k) -> k -> Poly k
evalPolyExtK (Poly []) _ = zeroPoly
evalPolyExtK (Poly cs) x0 =
  foldl (\acc (i, ExtElem p _) ->
           addPoly acc (scalePoly (x0 ^ i) p))
        zeroPoly
        (zip [0 :: Int ..] cs)

-- | Compute Res_t(m(t), g(t)) over any field k.
normAtPointK :: (Eq k, Fractional k) => Poly k -> Poly k -> k
normAtPointK = polyResultantK

-- | Factor a square-free polynomial over K(α) given a factoring function for K.
--
-- This is the generalized Trager algorithm. The @factorBaseK@ parameter
-- provides factoring over the base field K (e.g., @factorSquareFree@ for Q,
-- or @factorSFOverExtension field1@ for K = Q(α₁)).
factorSFOverExtensionK :: (Eq k, Fractional k)
                       => (Poly k -> [Poly k])  -- ^ Factor over base field K
                       -> ExtField k
                       -> Poly (ExtElem k)
                       -> [Poly (ExtElem k)]
factorSFOverExtensionK factorBaseK field f
  | degree f <= 0 = []
  | degree f == 1 = [monicPoly f]
  | otherwise = tragerFactorK factorBaseK field f 0

tragerFactorK :: (Eq k, Fractional k)
              => (Poly k -> [Poly k])
              -> ExtField k
              -> Poly (ExtElem k)
              -> Int
              -> [Poly (ExtElem k)]
tragerFactorK factorBaseK field f s
  | s > 20 = [f]
  | otherwise =
      let alpha = generator field
          sAlpha = embed field (fromInteger (fromIntegral s)) * alpha
          fShifted = shiftPoly f sAlpha
          n = normPolyK field fShifted
      in if not (isSquareFreeK n)
         then tragerFactorK factorBaseK field f (s + 1)
         else
           let nFactors = factorBaseK (monicPoly n)
           in if length nFactors <= 1
              then [monicPoly f]
              else liftFactorsK field f sAlpha nFactors

-- | Check if a polynomial over k is square-free.
isSquareFreeK :: (Eq k, Fractional k) => Poly k -> Bool
isSquareFreeK p =
  let p' = diffPoly p
      g = gcdPolyK p p'
  in degree g <= 0

-- | GCD over any field.
gcdPolyK :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
gcdPolyK a b
  | isZeroPoly b = monicPoly a
  | otherwise = gcdPolyK b (snd $ divModPoly a b)
  where
    isZeroPoly (Poly cs) = all (== 0) cs

-- | Lift base-field factors back to K(α)-factors via GCD (generalized).
liftFactorsK :: (Eq k, Fractional k)
             => ExtField k
             -> Poly (ExtElem k)
             -> ExtElem k
             -> [Poly k]
             -> [Poly (ExtElem k)]
liftFactorsK field f sAlpha kFactors = go f kFactors
  where
    go _ [] = []
    go remaining (g : gs)
      | degree remaining <= 0 = []
      | otherwise =
          let gLifted = liftKPoly field g
              gUnshifted = shiftPoly gLifted (negate sAlpha)
              h = gcdPolyGeneric remaining gUnshifted
          in if degree h > 0
             then let (q, _) = divModPoly remaining h
                  in monicPoly h : go q gs
             else go remaining gs

-- | GCD of two polynomials (generic, for ExtElem k).
gcdPolyGeneric :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
gcdPolyGeneric a b
  | all (== 0) (unPoly b) = monicPoly a
  | otherwise = gcdPolyGeneric b (snd $ divModPoly a b)

-- | Lift a K[x] polynomial to K(α)[x].
liftKPoly :: (Eq k, Num k) => ExtField k -> Poly k -> Poly (ExtElem k)
liftKPoly field (Poly cs) = Poly (map (embed field) cs)
