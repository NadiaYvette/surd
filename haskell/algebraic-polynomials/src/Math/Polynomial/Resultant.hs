-- | Resultant computation, Lagrange interpolation, and composed-sum/product
-- polynomials, generalized to work over any field @k@ with
-- @(Eq k, Fractional k)@ constraints.
--
-- These are canonical implementations that replace duplicated versions
-- previously scattered across MinimalPoly, MinimalPolyTower, and
-- TragerFactoring.
module Math.Polynomial.Resultant
  ( polyResultant
  , lagrangeInterpolate
  , composedSum
  , composedProduct
  , negateVar
  , reciprocalPoly
  , substituteXN
  , shiftPoly
  ) where

import Math.Polynomial.Univariate

-- | Resultant of two univariate polynomials via the Euclidean algorithm.
polyResultant :: (Eq k, Fractional k) => Poly k -> Poly k -> k
polyResultant f g
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
         else sign * (lg ^ (df - dr)) * polyResultant g r

-- | Lagrange interpolation: given points (x_i, y_i), compute the unique
-- polynomial of degree <= n passing through all points.
lagrangeInterpolate :: (Eq k, Fractional k) => [(k, k)] -> Poly k
lagrangeInterpolate points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (recip (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Compute the composed sum polynomial.
-- If p has roots alpha_i and q has roots beta_j, the composed sum has
-- roots alpha_i + beta_j.
--
-- Uses evaluation and interpolation: evaluate Res_y(p(y), q(x0 - y))
-- at enough points x0, then interpolate.
composedSum :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
composedSum p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromInteger i | i <- [0..fromIntegral resultDeg]]
      values = [resultantSumAt p q x | x <- points]
  in lagrangeInterpolate (zip points values)

-- | Compute Res_y(p(y), q(x0 - y)) for a specific x0.
resultantSumAt :: (Eq k, Fractional k) => Poly k -> Poly k -> k -> k
resultantSumAt p q x0 =
  let qShifted = substituteLinear q x0 (-1)  -- q(x0 + (-1)*y)
  in polyResultant p qShifted

-- | Substitute (a + b*y) for x in polynomial p.
-- p(a + b*y) = sum_i c_i * (a + b*y)^i
substituteLinear :: (Eq k, Fractional k) => Poly k -> k -> k -> Poly k
substituteLinear (Poly []) _ _ = zeroPoly
substituteLinear (Poly cs) a b =
  let binomial = mkPoly [a, b]  -- a + b*y
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly binomial acc)) zeroPoly cs

-- | Composed product: if p has roots alpha_i and q has roots beta_j,
-- result has roots alpha_i * beta_j.
composedProduct :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
composedProduct p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromInteger i | i <- [1..fromIntegral resultDeg + 1]]  -- avoid 0
      values = [productResultantAt p q x | x <- points]
  in lagrangeInterpolate (zip points values)

-- | Compute Res_y(p(y), y^dq * q(x0/y)) for a specific x0.
productResultantAt :: (Eq k, Fractional k) => Poly k -> Poly k -> k -> k
productResultantAt p q x0 =
  let pRev = scaledReciprocalAt p x0
  in polyResultant pRev q

-- | Compute y^n * p(x0/y) as a polynomial in y.
-- If p = c0 + c1*x + ... + cn*x^n, then
-- y^n * p(x0/y) = c0*y^n + c1*x0*y^(n-1) + ... + cn*x0^n
scaledReciprocalAt :: (Eq k, Num k) => Poly k -> k -> Poly k
scaledReciprocalAt (Poly cs) x0 =
  let n = length cs - 1
      newCs = [c * x0 ^ (n - k) | (k, c) <- zip [0..] (reverse cs)]
  in mkPoly newCs

-- | p(-x): negate odd-degree coefficients.
negateVar :: (Eq k, Num k) => Poly k -> Poly k
negateVar (Poly cs) = mkPoly $ zipWith (\i c -> if odd (i :: Int) then negate c else c) [0..] cs

-- | Reciprocal polynomial: x^n * p(1/x), i.e. reverse the coefficient list.
reciprocalPoly :: (Eq k, Num k) => Poly k -> Poly k
reciprocalPoly (Poly cs) = mkPoly (reverse cs)

-- | Substitute x^n for x in p: p(x^n).
-- p(x^n) where p = c0 + c1*x + ... + cd*x^d
-- = c0 + c1*x^n + c2*x^(2n) + ... + cd*x^(dn)
substituteXN :: (Eq k, Num k) => Int -> Poly k -> Poly k
substituteXN n (Poly cs) =
  let indexed = zip [0 :: Int ..] cs
      maxDeg = (length cs - 1) * n
      result = replicate (maxDeg + 1) 0
      set' xs (i, c) =
        let pos = i * n
        in take pos xs ++ [c] ++ drop (pos + 1) xs
  in mkPoly $ foldl set' result indexed

-- | Shift a polynomial: f(x + a) where a is in the coefficient field.
shiftPoly :: (Eq k, Fractional k) => Poly k -> k -> Poly k
shiftPoly (Poly []) _ = zeroPoly
shiftPoly (Poly cs) a =
  -- f(x + a) via Horner
  let xPlusA = mkPoly [a, 1]  -- x + a
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly xPlusA acc)) zeroPoly cs
