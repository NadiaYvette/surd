-- | Compute the minimal polynomial of a radical expression over Q.
--
-- The basic approach: given a radical expression \(\alpha\), build a polynomial
-- that \(\alpha\) satisfies (an /annihilating polynomial/), then factor it
-- over \(\mathbb{Q}\) to find the irreducible factor that \(\alpha\) is
-- actually a root of (verified by numerical evaluation).
--
-- The annihilating polynomial is constructed recursively using resultants:
--
--   * @Lit r@: annihilated by @x - r@
--   * @a + b@: use the composed sum @Res_y(p_a(y), p_b(x - y))@
--   * @a * b@: use the composed product @Res_y(y^d * p_a(x\/y), p_b(y))@
--   * @Root n a@: if @p(x)@ annihilates @a@, then @p(x^n)@ annihilates @a^{1\/n}@
--   * @Inv a@: reciprocal polynomial @x^d * p(1\/x)@
--
-- This per-node approach can cause exponential blowup when radicals are
-- shared. For expressions with many shared radicals, use the tower-based
-- approach in "Surd.Polynomial.MinimalPolyTower" instead.
module Surd.Polynomial.MinimalPoly
  ( minimalPoly,
    annihilatingPoly,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Math.Polynomial.Factoring (factorSquareFree)
import Math.Polynomial.Resultant
  ( composedProduct,
    composedSum,
    lagrangeInterpolate,
    negateVar,
    polyResultant,
    reciprocalPoly,
    substituteXN,
  )
import Math.Polynomial.RootBound (pickClosest)
import Math.Polynomial.Univariate
import Surd.Radical.Eval (eval)
import Surd.Types

-- | Compute the minimal polynomial of a radical expression over Q.
--
-- This is the monic irreducible polynomial in Q[x] of smallest degree
-- that the expression satisfies.
minimalPoly :: RadExpr Rational -> Poly Rational
minimalPoly expr =
  let ann = annihilatingPoly expr
      v = eval expr :: Double
      -- Factor and pick the irreducible factor whose root is closest to v
      factors = factorSquareFree ann
   in case factors of
        [] -> ann
        (f : fs) -> monicPoly $ pickClosest v (f :| fs)

-- | Compute an annihilating polynomial (not necessarily minimal) for
-- a radical expression.
--
-- The expression is guaranteed to be a root of the returned polynomial,
-- but the polynomial may be reducible (i.e., not the minimal polynomial).
-- Use 'minimalPoly' to obtain the irreducible factor.
--
-- Built recursively using resultant-based composition operations:
-- composed sum for addition, composed product for multiplication,
-- @x^n@-substitution for nth roots, and reciprocal polynomial for inversion.
annihilatingPoly :: RadExpr Rational -> Poly Rational
annihilatingPoly (Lit r) =
  -- Minimal poly of r ∈ Q is (x - r)
  mkPoly [-r, 1]
annihilatingPoly (Neg e) =
  -- If p(x) annihilates e, then p(-x) annihilates -e
  let p = annihilatingPoly e
   in negateVar p
annihilatingPoly (Add a b) =
  -- Resultant-based: if p(x) annihilates a and q(x) annihilates b,
  -- then Res_y(p(y), q(x-y)) annihilates a+b
  let pa = annihilatingPoly a
      pb = annihilatingPoly b
   in composedSum pa pb
annihilatingPoly (Mul a b) =
  -- If p(x) annihilates a and q(x) annihilates b,
  -- then Res_y(y^deg(p) * p(x/y), q(y)) annihilates a*b (when b ≠ 0)
  let pa = annihilatingPoly a
      pb = annihilatingPoly b
   in composedProduct pa pb
annihilatingPoly (Inv e) =
  -- If p(x) annihilates e, then x^deg(p) * p(1/x) annihilates 1/e
  let p = annihilatingPoly e
   in reciprocalPoly p
annihilatingPoly (Root n (Lit r)) =
  -- nth root of rational r: annihilated by x^n - r
  -- x^n - r
  mkPoly $ [-r] ++ replicate (n - 1) 0 ++ [1]
annihilatingPoly (Root n e) =
  -- If p(x) annihilates e, then p(x^n) annihilates e^(1/n)
  let p = annihilatingPoly e
   in substituteXN n p
annihilatingPoly (Pow e n)
  | n >= 0 =
      -- If p(x) annihilates e, then Res_y(p(y), y^n - x) annihilates e^n
      let p = annihilatingPoly e
       in annihilatingPolyOfPow p n
  | otherwise =
      -- e^n for negative n = (1/e)^(-n)
      let p = annihilatingPoly e
          pr = reciprocalPoly p
       in annihilatingPolyOfPow pr (-n)

-- | Compute annihilating polynomial for e^n given the annihilating poly for e.
annihilatingPolyOfPow :: Poly Rational -> Int -> Poly Rational
annihilatingPolyOfPow p n
  | n == 0 = mkPoly [-1, 1] -- x - 1
  | n == 1 = p
  | otherwise =
      -- If p(α) = 0, we want a polynomial q such that q(α^n) = 0.
      -- Res_y(p(y), y^n - x) gives this.
      let dp = degree p
          resultDeg = dp -- The result has degree dp
          points = [fromIntegral i | i <- [0 .. resultDeg]]
          values = [powResultantAt p n x | x <- points]
       in lagrangeInterpolate (zip points values)

-- | Res_y(p(y), y^n - x₀)
powResultantAt :: Poly Rational -> Int -> Rational -> Rational
powResultantAt p n x0 =
  let ynMinusX0 = mkPoly $ [-x0] ++ replicate (n - 1) 0 ++ [1]
   in polyResultant p ynMinusX0
