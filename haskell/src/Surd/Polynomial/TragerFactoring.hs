-- | Factoring polynomials over algebraic extension fields Q(α)
-- using Trager's algorithm (1976).
--
-- Given f(x) ∈ Q(α)[x] where α is a root of an irreducible m(t) ∈ Q[t],
-- compute the norm N(x) = Res_t(m(t), f(x - s·t)) ∈ Q[x] for suitable s,
-- factor N over Q, and lift factors back via GCD.
module Surd.Polynomial.TragerFactoring
  ( factorOverExtension
  , factorSFOverExtension
  , normPoly
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
  | polyDegExt f <= 0 = []
  | polyDegExt f == 1 = [monicPolyExt field f]
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
              then [monicPolyExt field f]  -- f is irreducible over Q(α)
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
  let df = polyDegExt f
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
      | polyDegExt remaining <= 0 = []
      | otherwise =
          -- g_i(x - s·α) as a polynomial in Q(α)[x]
          let gLifted = liftQPoly field g
              gUnshifted = shiftPoly gLifted (negate sAlpha)
              -- gcd(remaining, gUnshifted) in Q(α)[x]
              h = gcdPolyExt field remaining gUnshifted
          in if polyDegExt h > 0
             then let (q, _) = divModPolyExt field remaining h
                  in monicPolyExt field h : go q gs
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
  | isZeroPolyExt b = monicPolyExt field a
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

-- | Degree of a polynomial over Q(α).
polyDegExt :: Poly (ExtElem Rational) -> Int
polyDegExt = degree

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
