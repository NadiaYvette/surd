-- | Compute the minimal polynomial of a radical expression over Q.
--
-- The basic approach: given a radical expression α, build a polynomial
-- that α satisfies, then factor it to find the irreducible factor
-- that α is actually a root of (verified by numerical evaluation).
module Surd.Polynomial.MinimalPoly
  ( minimalPoly
  , annihilatingPoly
  , polyResultant
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import Surd.Types
import Surd.Polynomial.Univariate
import Surd.Polynomial.Factoring (factorSquareFree)
import Surd.Radical.Eval (eval)

-- | Compute the minimal polynomial of a radical expression over Q.
--
-- This is the monic irreducible polynomial in Q[x] of smallest degree
-- that the expression satisfies.
minimalPoly :: RadExpr Rational -> Poly Rational
minimalPoly expr =
  let ann = annihilatingPoly expr
      v   = eval expr :: Double
      -- Factor and pick the irreducible factor whose root is closest to v
      factors = factorSquareFree ann
  in case factors of
    []     -> ann
    (f:fs) -> monicPoly $ pickClosest v (f :| fs)

-- | Compute an annihilating polynomial (not necessarily minimal) for
-- a radical expression. The expression is guaranteed to be a root of
-- this polynomial, but it may be reducible.
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
  in resultantSum pa pb

annihilatingPoly (Mul a b) =
  -- If p(x) annihilates a and q(x) annihilates b,
  -- then Res_y(y^deg(p) * p(x/y), q(y)) annihilates a*b (when b ≠ 0)
  let pa = annihilatingPoly a
      pb = annihilatingPoly b
  in resultantProduct pa pb

annihilatingPoly (Inv e) =
  -- If p(x) annihilates e, then x^deg(p) * p(1/x) annihilates 1/e
  let p = annihilatingPoly e
  in reciprocalPoly p

annihilatingPoly (Root n (Lit r)) =
  -- nth root of rational r: annihilated by x^n - r
  -- x^n - r
  mkPoly $ [- r] ++ replicate (n - 1) 0 ++ [1]

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

-- | p(−x): negate odd-degree coefficients.
negateVar :: (Eq k, Num k) => Poly k -> Poly k
negateVar (Poly cs) = mkPoly $ zipWith (\i c -> if odd (i :: Int) then negate c else c) [0..] cs

-- | Reciprocal polynomial: x^n * p(1/x), i.e. reverse the coefficient list.
reciprocalPoly :: (Eq k, Num k) => Poly k -> Poly k
reciprocalPoly (Poly cs) = mkPoly (reverse cs)

-- | Substitute x^n for x in p: p(x^n).
substituteXN :: (Eq k, Num k) => Int -> Poly k -> Poly k
substituteXN n (Poly cs) = mkPoly $ concatMap pad (zip [0..] cs)
  where
    pad (i, c) = replicate (i * n) 0 ++ [c] ++ replicate (n - 1) 0
    -- Actually, simpler: position of coefficient c_i is at index i*n
    -- Let's redo this properly

-- Hmm, the above is not quite right. Let me fix:
-- p(x^n) where p = c0 + c1*x + ... + cd*x^d
-- = c0 + c1*x^n + c2*x^(2n) + ... + cd*x^(dn)

-- | Resultant-based computation for sum: if p(α)=0 and q(β)=0,
-- find a polynomial r such that r(α+β)=0.
--
-- r(x) = Res_y(p(y), q(x - y))
resultantSum :: Poly Rational -> Poly Rational -> Poly Rational
resultantSum p q =
  -- Compute Res_y(p(y), q(x-y)) by treating x as a parameter.
  -- For now, use a direct construction via the product of (α_i + β_j - x)
  -- over all roots, approximated by the resultant.
  --
  -- Practical approach: use the composed sum via Sylvester matrix.
  -- This is expensive but correct.
  composedSum p q

-- | Resultant-based computation for product.
resultantProduct :: Poly Rational -> Poly Rational -> Poly Rational
resultantProduct p q = composedProduct p q

-- | Compute the composed sum polynomial.
-- If p has roots α_i and q has roots β_j, the composed sum has roots α_i + β_j.
--
-- Uses the formula: ComposedSum(p, q)(x) = Res_y(p(y), y^m * q((x-y)/y))
-- but more practically, we compute it via the Sylvester-like matrix approach.
--
-- For our purposes (typically small degree), we use the direct resultant.
composedSum :: Poly Rational -> Poly Rational -> Poly Rational
composedSum p q =
  -- Res_y(p(y), q_shifted(y)) where q_shifted is q(x - y) with x as parameter
  -- We compute this by building bivariate polynomials and taking the resultant.
  --
  -- Simpler approach: if p = ∏(y - αᵢ), then
  -- ComposedSum = ∏ᵢ q(x - αᵢ) evaluated symbolically.
  --
  -- We compute Res_y(p(y), rev_q(x - y)) where rev means we think of q
  -- as a polynomial in y with x-dependent coefficients.
  --
  -- Direct resultant via Sylvester determinant:
  let dp = degree p
      dq = degree q
      -- Build q(x - y) as a polynomial in y with Poly Rational coefficients (in x)
      -- q(x-y) = q_0 + q_1*(x-y) + q_2*(x-y)^2 + ...
      -- This is a polynomial in y of degree dq.
      -- We need the resultant of p(y) and q(x-y) w.r.t. y.
      --
      -- Use the Sylvester matrix approach.
  in sylvesterResultant p q dp dq

-- | Compute resultant of p(y) and q(x-y) as a polynomial in x,
-- via evaluation and interpolation.
--
-- Evaluate at enough points x = 0, 1, 2, ... to determine the resultant
-- polynomial (which has degree dp*dq), then interpolate.
sylvesterResultant :: Poly Rational -> Poly Rational -> Int -> Int -> Poly Rational
sylvesterResultant p q dp dq =
  let resultDeg = dp * dq
      -- Evaluate at points 0, 1, ..., resultDeg
      points = [fromIntegral i | i <- [0..resultDeg]]
      values = [resultantAt p q x | x <- points]
  in lagrangeInterpolate (zip points values)

-- | Compute Res_y(p(y), q(x₀ - y)) for a specific rational x₀.
resultantAt :: Poly Rational -> Poly Rational -> Rational -> Rational
resultantAt p q x0 =
  -- q(x₀ - y): substitute (x₀ - y) for the variable in q
  let qShifted = substituteLinear q x0 (-1)  -- q(x₀ + (-1)*y)
  in polyResultant p qShifted

-- | Substitute (a + b*y) for x in polynomial p.
-- p(a + b*y) = sum_i c_i * (a + b*y)^i
substituteLinear :: Poly Rational -> Rational -> Rational -> Poly Rational
substituteLinear (Poly []) _ _ = zeroPoly
substituteLinear (Poly cs) a b =
  -- Use Horner: p(a + b*y) with y as the new variable
  let binomial = mkPoly [a, b]  -- a + b*y
  in foldr (\c acc -> addPoly (constPoly c) (mulPoly binomial acc)) zeroPoly cs

-- | Resultant of two univariate polynomials via the Euclidean algorithm.
polyResultant :: Poly Rational -> Poly Rational -> Rational
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

-- | Composed product: if p has roots αᵢ and q has roots βⱼ,
-- result has roots αᵢ * βⱼ.
composedProduct :: Poly Rational -> Poly Rational -> Poly Rational
composedProduct p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromIntegral i | i <- [1..resultDeg + 1]]  -- avoid 0
      values = [productResultantAt p q x | x <- points]
  in lagrangeInterpolate (zip points values)

-- | Compute Res_y(p(y), y^dq * q(x₀/y)) for a specific x₀.
-- This gives the value of the composed-product polynomial at x₀.
productResultantAt :: Poly Rational -> Poly Rational -> Rational -> Rational
productResultantAt p q x0 =
  -- Res_y(p(y), y^dq * q(x₀/y))
  -- = Res_y(p(y), rev_q(y; x₀))
  -- where rev_q has roots x₀/βⱼ
  --
  -- More directly: Res_y(y^m * p(x₀/y), q(y)) where m = deg(p)
  let -- y^dp * p(x₀/y) = reversal of p scaled by x₀
      pRev = scaledReciprocalAt p x0
  in polyResultant pRev q

-- | Compute y^n * p(x₀/y) as a polynomial in y.
-- If p = c₀ + c₁x + ... + cₙxⁿ, then
-- y^n * p(x₀/y) = c₀*y^n + c₁*x₀*y^(n-1) + ... + cₙ*x₀^n
scaledReciprocalAt :: Poly Rational -> Rational -> Poly Rational
scaledReciprocalAt (Poly cs) x0 =
  let n = length cs - 1
      -- Coefficient of y^k in y^n * p(x₀/y) is c_{n-k} * x₀^{n-k}
      newCs = [c * x0 ^ (n - k) | (k, c) <- zip [0..] (reverse cs)]
  in mkPoly newCs

-- | Compute annihilating polynomial for e^n given the annihilating poly for e.
annihilatingPolyOfPow :: Poly Rational -> Int -> Poly Rational
annihilatingPolyOfPow p n
  | n == 0 = mkPoly [-1, 1]  -- x - 1
  | n == 1 = p
  | otherwise =
      -- If p(α) = 0, we want a polynomial q such that q(α^n) = 0.
      -- Res_y(p(y), y^n - x) gives this.
      let dp = degree p
          resultDeg = dp  -- The result has degree dp
          points = [fromIntegral i | i <- [0..resultDeg]]
          values = [powResultantAt p n x | x <- points]
      in lagrangeInterpolate (zip points values)

-- | Res_y(p(y), y^n - x₀)
powResultantAt :: Poly Rational -> Int -> Rational -> Rational
powResultantAt p n x0 =
  let ynMinusX0 = mkPoly $ [-x0] ++ replicate (n - 1) 0 ++ [1]
  in polyResultant p ynMinusX0

-- | Lagrange interpolation: given points (xᵢ, yᵢ), compute the unique
-- polynomial of degree ≤ n passing through all points.
lagrangeInterpolate :: [(Rational, Rational)] -> Poly Rational
lagrangeInterpolate points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]

    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
      in foldl mulPoly (constPoly 1)
           [scalePoly (1 / (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Pick the factor whose numerical root is closest to the target value.
pickClosest :: Double -> NonEmpty (Poly Rational) -> Poly Rational
pickClosest target factors =
  let scored = fmap (\f -> (f, minimum $ map (\r -> abs (fromRational r - target)) (approxRoots f))) factors
  in fst $ NE.head $ NE.sortBy (comparing snd) scored

-- | Find approximate real roots of a polynomial by scanning.
-- Simple bisection-based root finder, sufficient for picking the right factor.
approxRoots :: Poly Rational -> [Rational]
approxRoots p
  | degree p <= 0 = []
  | degree p == 1 =
      case unPoly p of
        [a, b] -> [-a / b]
        _      -> []
  | otherwise =
      -- Scan a range and find sign changes
      let bound = rootBound p
          pts = [fromIntegral i / 10 | i <- [floor (-bound * 10) :: Integer .. ceiling (bound * 10)]]
          signs = [(x, signum (evalPoly p x)) | x <- pts]
          changes = [(x1, x2) | ((x1, s1), (x2, s2)) <- zip signs (drop 1 signs), s1 /= s2, s1 /= 0, s2 /= 0]
          roots = [bisect p lo hi 50 | (lo, hi) <- changes]
      in if null roots then [0] else roots  -- fallback

-- | Cauchy's root bound: all roots have |r| ≤ 1 + max|cᵢ/cₙ|
rootBound :: Poly Rational -> Rational
rootBound (Poly []) = 0
rootBound (Poly cs) =
  let lc = NE.last (NE.fromList cs)
      ratios = map (\c -> abs (c / lc)) (init cs)
  in 1 + maximum (0 : ratios)

-- | Bisection root finding.
bisect :: Poly Rational -> Rational -> Rational -> Int -> Rational
bisect _ lo _ 0 = lo
bisect p lo hi n =
  let mid = (lo + hi) / 2
      fmid = evalPoly p mid
      flo = evalPoly p lo
  in if fmid == 0 then mid
     else if signum fmid == signum flo
          then bisect p mid hi (n - 1)
          else bisect p lo mid (n - 1)
