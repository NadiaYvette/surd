-- |
-- Module      : Math.Polynomial.Resultant
-- Description : Resultant computation, Lagrange interpolation, and composed polynomials
-- Stability   : experimental
--
-- Canonical implementations of the polynomial resultant (via the Euclidean
-- algorithm), Lagrange interpolation, and composed-sum\/composed-product
-- polynomials. These are generalized to work over any field @k@ with
-- @('Eq' k, 'Fractional' k)@ constraints.
--
-- The composed-sum and composed-product operations are used for computing
-- minimal polynomials of sums and products of algebraic numbers.
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
--
-- The resultant \(\text{Res}(f, g)\) is a scalar in the coefficient field
-- that is zero if and only if \(f\) and \(g\) share a common root (over
-- the algebraic closure).
--
-- Complexity: \(O(n \cdot m)\) where \(n = \deg f\) and \(m = \deg g\),
-- via the Euclidean pseudo-remainder sequence.
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

-- | Lagrange interpolation: given a list of points \((x_i, y_i)\),
-- compute the unique polynomial of degree \(\leq n\) passing through
-- all \(n + 1\) points.
--
-- __Precondition:__ the \(x_i\) values must be distinct.
--
-- Complexity: \(O(n^2)\) in the number of points.
lagrangeInterpolate :: (Eq k, Fractional k) => [(k, k)] -> Poly k
lagrangeInterpolate points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (recip (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Composed sum polynomial.
--
-- If \(p\) has roots \(\alpha_i\) and \(q\) has roots \(\beta_j\), the
-- composed sum has roots \(\alpha_i + \beta_j\). The result has degree
-- \(\deg p \cdot \deg q\).
--
-- Computed via evaluation and interpolation of
-- \(\text{Res}_y(p(y), q(x - y))\) at sufficiently many points.
composedSum :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
composedSum p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromInteger i | i <- [0..fromIntegral resultDeg]]
      values = [resultantSumAt p q x | x <- points]
  in lagrangeInterpolate (zip points values)

-- | Compute \(\text{Res}_y(p(y), q(x_0 - y))\) for a specific \(x_0\).
resultantSumAt :: (Eq k, Fractional k) => Poly k -> Poly k -> k -> k
resultantSumAt p q x0 =
  let qShifted = substituteLinear q x0 (-1)  -- q(x0 + (-1)*y)
  in polyResultant p qShifted

-- | Substitute \(a + b \cdot y\) for \(x\) in polynomial \(p\): computes
-- \(p(a + b y)\).
substituteLinear :: (Eq k, Fractional k) => Poly k -> k -> k -> Poly k
substituteLinear (Poly []) _ _ = zeroPoly
substituteLinear (Poly cs) a b =
  let binomial = mkPoly [a, b]  -- a + b*y
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly binomial acc)) zeroPoly cs

-- | Composed product polynomial.
--
-- If \(p\) has roots \(\alpha_i\) and \(q\) has roots \(\beta_j\), the
-- result has roots \(\alpha_i \cdot \beta_j\). The result has degree
-- \(\deg p \cdot \deg q\).
--
-- Uses evaluation and interpolation of
-- \(\text{Res}_y(y^{\deg q} \cdot p(x_0/y), q(y))\).
composedProduct :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
composedProduct p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromInteger i | i <- [1..fromIntegral resultDeg + 1]]  -- avoid 0
      values = [productResultantAt p q x | x <- points]
  in lagrangeInterpolate (zip points values)

-- | Compute \(\text{Res}_y(y^n \cdot p(x_0/y), q(y))\) for a specific \(x_0\).
productResultantAt :: (Eq k, Fractional k) => Poly k -> Poly k -> k -> k
productResultantAt p q x0 =
  let pRev = scaledReciprocalAt p x0
  in polyResultant pRev q

-- | Compute \(y^n \cdot p(x_0/y)\) as a polynomial in \(y\).
--
-- If \(p = c_0 + c_1 x + \cdots + c_n x^n\), then
-- \(y^n \cdot p(x_0/y) = c_0 y^n + c_1 x_0 y^{n-1} + \cdots + c_n x_0^n\).
scaledReciprocalAt :: (Eq k, Num k) => Poly k -> k -> Poly k
scaledReciprocalAt (Poly cs) x0 =
  let n = length cs - 1
      newCs = [c * x0 ^ (n - k) | (k, c) <- zip [0..] (reverse cs)]
  in mkPoly newCs

-- | Negate the variable: compute \(p(-x)\) by negating odd-degree coefficients.
--
-- >>> negateVar (mkPoly [1, 2, 3] :: Poly Rational)
-- Poly [1 % 1,(-2) % 1,3 % 1]
negateVar :: (Eq k, Num k) => Poly k -> Poly k
negateVar (Poly cs) = mkPoly $ zipWith (\i c -> if odd (i :: Int) then negate c else c) [0..] cs

-- | Reciprocal polynomial: \(x^n \cdot p(1/x)\), computed by reversing
-- the coefficient list.
--
-- If \(r\) is a root of \(p\), then \(1/r\) is a root of the reciprocal.
reciprocalPoly :: (Eq k, Num k) => Poly k -> Poly k
reciprocalPoly (Poly cs) = mkPoly (reverse cs)

-- | Substitute \(x^n\) for \(x\) in \(p\): compute \(p(x^n)\).
--
-- If \(p = c_0 + c_1 x + c_2 x^2 + \cdots\), then
-- \(p(x^n) = c_0 + c_1 x^n + c_2 x^{2n} + \cdots\).
substituteXN :: (Eq k, Num k) => Int -> Poly k -> Poly k
substituteXN n (Poly cs) =
  let indexed = zip [0 :: Int ..] cs
      maxDeg = (length cs - 1) * n
      result = replicate (maxDeg + 1) 0
      set' xs (i, c) =
        let pos = i * n
        in take pos xs ++ [c] ++ drop (pos + 1) xs
  in mkPoly $ foldl set' result indexed

-- | Shift a polynomial: compute \(f(x + a)\) where \(a\) is in the
-- coefficient field.
--
-- Uses Horner's method for composition with the linear polynomial \(x + a\).
shiftPoly :: (Eq k, Fractional k) => Poly k -> k -> Poly k
shiftPoly (Poly []) _ = zeroPoly
shiftPoly (Poly cs) a =
  -- f(x + a) via Horner
  let xPlusA = mkPoly [a, 1]  -- x + a
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly xPlusA acc)) zeroPoly cs
