-- |
-- Module      : Math.Polynomial.TragerFactoring
-- Description : Factoring polynomials over algebraic extension fields via Trager's algorithm
-- Stability   : experimental
--
-- Implementation of Trager's algorithm (1976) for factoring polynomials
-- over simple algebraic extension fields \(K(\alpha)\).
--
-- __Algorithm outline:__
--
-- Given \(f(x) \in K(\alpha)[x]\) where \(\alpha\) is a root of an
-- irreducible \(m(t) \in K[t]\):
--
-- 1. Compute the norm \(N(x) = \text{Res}_t(m(t), f(x - s \cdot t)) \in K[x]\)
--    for a suitable shift parameter \(s\) (chosen so that \(N\) is square-free).
-- 2. Factor \(N(x)\) over the base field \(K\).
-- 3. Lift factors back to \(K(\alpha)[x]\) via GCD:
--    for each factor \(g_i\) of \(N\), compute
--    \(\gcd(f(x), g_i(x + s \cdot \alpha))\) in \(K(\alpha)[x]\).
--
-- Two variants are provided:
--
-- * 'factorSFOverExtension': factors over \(\mathbb{Q}(\alpha)\),
--   using 'Math.Polynomial.Factoring.factorSquareFree' for the base case.
-- * 'factorSFOverExtensionK': generalized to arbitrary base field \(K\),
--   taking a base-field factoring function as a parameter (enabling
--   iterated extension towers like \(\mathbb{Q}(\alpha)(\beta)\)).
--
-- Reference: B. M. Trager, \"Algebraic Factoring and Rational Function
-- Integration\", /Proceedings of SYMSAC '76/, pp. 219--226, ACM, 1976.
module Math.Polynomial.TragerFactoring
  ( factorOverExtension
  , factorSFOverExtension
  , factorSFOverExtensionK
  , normPoly
  , normPolyK
  ) where

import Math.Polynomial.Univariate
import Math.Polynomial.Factoring (factorSquareFree)
import Math.Polynomial.Resultant (polyResultant, lagrangeInterpolate, shiftPoly)
import Math.Field.Extension

-- | Factor a polynomial over \(\mathbb{Q}(\alpha)\) into irreducible factors.
--
-- Returns a list of @(factor, multiplicity)@ pairs. Uses square-free
-- factorisation followed by 'factorSFOverExtension' on each component.
factorOverExtension :: ExtField Rational
                    -> Poly (ExtElem Rational)
                    -> [(Poly (ExtElem Rational), Int)]
factorOverExtension field f =
  let sfFactors = squareFreeExt field f
  in concatMap (\(g, m) -> map (\h -> (h, m)) (factorSFOverExtension field g)) sfFactors

-- | Factor a square-free polynomial over \(\mathbb{Q}(\alpha)\) into
-- irreducible factors using Trager's algorithm.
--
-- The shift parameter \(s\) is incremented from 0 until the norm
-- polynomial is square-free. If no factorisation is found after 20
-- shifts, the polynomial is returned as-is (assumed irreducible).
--
-- Returns a list of monic irreducible factors.
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
          -- f_shifted(x) = f(x + s*alpha) -- shift so norm is square-free
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
              then [monicPoly f]  -- f is irreducible over Q(alpha)
              else
                -- Lift each factor back
                let lifted = liftFactors field f sAlpha nFactors
                in lifted

-- | Compute the norm of \(f(x) \in \mathbb{Q}(\alpha)[x]\):
-- \(N(x) = \text{Res}_t(m(t), f_{\text{lifted}}(x, t)) \in \mathbb{Q}[x]\).
--
-- The lifted polynomial \(f_{\text{lifted}}(x, t)\) replaces each
-- coefficient \(c \in \mathbb{Q}(\alpha)\) (represented as a polynomial
-- in \(\alpha\)) with that polynomial in the variable \(t\).
--
-- Computed via evaluation at \(\deg f \cdot \deg m + 1\) rational points
-- followed by Lagrange interpolation.
normPoly :: ExtField Rational -> Poly (ExtElem Rational) -> Poly Rational
normPoly field f =
  let df = degree f
      dm = extDegree field
      resultDeg = df * dm
      -- Evaluate at 0, 1, 2, ..., resultDeg
      points = [fromIntegral i | i <- [0..resultDeg]]
      values = [normAtPoint field f x0 | x0 <- points]
  in lagrangeInterpolate (zip points values)

-- | Evaluate the norm at a specific rational point \(x_0\):
-- \(\text{Res}_t(m(t), f_{\text{lifted}}(x_0, t))\).
normAtPoint :: ExtField Rational -> Poly (ExtElem Rational) -> Rational -> Rational
normAtPoint field f x0 =
  let m = genMinPoly field
      -- f(x) = c_0 + c_1*x + c_2*x^2 + ...
      -- f_lifted(x0, t) = c_0(t) + c_1(t)*x0 + c_2(t)*x0^2 + ...
      -- where c_i(t) is the polynomial representation of the i-th coefficient.
      coeffs = map elemPoly (unPoly f)
      -- f_lifted(x0, t) = sum c_i(t) * x0^i
      fAtX0 = case coeffs of
                [] -> zeroPoly
                _  -> foldl addPoly zeroPoly
                        [scalePoly (x0 ^ i) ci | (i, ci) <- zip [0 :: Int ..] coeffs]
  in polyResultant m fAtX0

-- | Lift Q-factors back to Q(alpha)-factors via GCD.
liftFactors :: ExtField Rational
            -> Poly (ExtElem Rational)  -- f(x)
            -> ExtElem Rational         -- s*alpha (the shift)
            -> [Poly Rational]          -- factors of N(x) over Q
            -> [Poly (ExtElem Rational)]
liftFactors field f sAlpha qFactors = go f qFactors
  where
    go _ [] = []
    go remaining (g : gs)
      | degree remaining <= 0 = []
      | otherwise =
          -- g_i(x - s*alpha) as a polynomial in Q(alpha)[x]
          let gLifted = liftQPoly field g
              gUnshifted = shiftPoly gLifted (negate sAlpha)
              -- gcd(remaining, gUnshifted) in Q(alpha)[x]
              h = gcdPolyExt field remaining gUnshifted
          in if degree h > 0
             then let (q, _) = divModPolyExt field remaining h
                  in monicPoly h : go q gs
             else go remaining gs

-- | Lift a \(\mathbb{Q}[x]\) polynomial to \(\mathbb{Q}(\alpha)[x]\) by
-- embedding coefficients.
liftQPoly :: ExtField Rational -> Poly Rational -> Poly (ExtElem Rational)
liftQPoly field (Poly cs) = Poly (map (embed field) cs)

-- | Check if a rational polynomial is square-free (has no repeated roots).
isSquareFreeQ :: Poly Rational -> Bool
isSquareFreeQ p =
  let p' = diffPoly p
      g = gcdPoly p p'
  in degree g <= 0

-- | GCD of two polynomials over Q(alpha).
gcdPolyExt :: ExtField Rational
           -> Poly (ExtElem Rational)
           -> Poly (ExtElem Rational)
           -> Poly (ExtElem Rational)
gcdPolyExt field a b
  | isZeroPolyExt b = monicPoly a
  | otherwise = gcdPolyExt field b (snd $ divModPolyExt field a b)

-- | Division of polynomials over Q(alpha).
divModPolyExt :: ExtField Rational
              -> Poly (ExtElem Rational)
              -> Poly (ExtElem Rational)
              -> (Poly (ExtElem Rational), Poly (ExtElem Rational))
divModPolyExt _ f g
  -- Use the generic divModPoly, which works because ExtElem has Fractional
  = divModPoly f g

-- | Check if a polynomial over Q(alpha) is zero.
isZeroPolyExt :: Poly (ExtElem Rational) -> Bool
isZeroPolyExt (Poly cs) = all (== 0) cs

-- | Square-free factorization over Q(alpha).
squareFreeExt :: ExtField Rational
              -> Poly (ExtElem Rational)
              -> [(Poly (ExtElem Rational), Int)]
squareFreeExt _ = squareFree

-- | Evaluate \(f(x_0)\) where \(f\) has 'ExtElem' coefficients and
-- \(x_0 \in K\), returning a polynomial in \(K[t]\) (the representation
-- of the result as an element of \(K(\alpha)\)).
evalPolyExtK :: (Eq k, Fractional k) => Poly (ExtElem k) -> k -> Poly k
evalPolyExtK (Poly []) _ = zeroPoly
evalPolyExtK (Poly cs) x0 =
  foldl (\acc (i, ExtElem p _) ->
           addPoly acc (scalePoly (x0 ^ i) p))
        zeroPoly
        (zip [0 :: Int ..] cs)

-- | Compute the norm of \(f(x) \in K(\alpha)[x]\) over any base field \(K\):
-- \(N(x) = \text{Res}_t(m(t), f_{\text{lifted}}(x, t)) \in K[x]\).
--
-- Generalizes 'normPoly' from \(\mathbb{Q}\) to arbitrary coefficient fields.
normPolyK :: (Eq k, Fractional k) => ExtField k -> Poly (ExtElem k) -> Poly k
normPolyK field f =
  let df = degree f
      dm = extDegree field
      resultDeg = df * dm
      m = genMinPoly field
      points = [fromInteger i | i <- [0..fromIntegral resultDeg]]
      values = [polyResultant m (evalPolyExtK f x0) | (x0 :: k) <- points]
  in lagrangeInterpolate (zip points values)

-- | Factor a square-free polynomial over \(K(\alpha)\) given a factoring
-- function for the base field \(K\).
--
-- This is the generalized Trager algorithm. The @factorBaseK@ parameter
-- provides factoring over the base field (e.g., 'factorSquareFree' for
-- \(\mathbb{Q}\), or @factorSFOverExtension field1@ for
-- \(K = \mathbb{Q}(\alpha_1)\)).
--
-- This enables factoring over field towers like
-- \(\mathbb{Q}(\alpha_1)(\alpha_2)\) by nesting.
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

-- | Check if a polynomial over @k@ is square-free.
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

-- | Lift base-field factors back to K(alpha)-factors via GCD (generalized).
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

-- | Lift a @K[x]@ polynomial to @K(alpha)[x]@ by embedding coefficients.
liftKPoly :: (Eq k, Num k) => ExtField k -> Poly k -> Poly (ExtElem k)
liftKPoly field (Poly cs) = Poly (map (embed field) cs)
