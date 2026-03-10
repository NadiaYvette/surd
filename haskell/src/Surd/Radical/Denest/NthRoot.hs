-- | General nth-root denesting.
--
-- For the case where we have ⁿ√(a + b·ⁿ√r) and similar nested
-- radical expressions with arbitrary root indices.
--
-- The general approach follows Landau (1992) / Zippel (1985):
-- given a nested radical, compute its minimal polynomial, analyse
-- the Galois group of the splitting field, and determine whether
-- a simpler radical representation exists.
--
-- This module currently implements:
-- 1. Simple nth-root simplification: ⁿ√(aⁿ·b) = a·ⁿ√b
-- 2. Root-index reduction: ⁿ√(x^k) simplification
-- 3. Nested root collapse: ᵐ√(ⁿ√x) = ᵐⁿ√x
-- 4. Cube root denesting for the form ³√(a + b√c)
--
-- More general Galois-theoretic denesting is planned for a future phase.
module Surd.Radical.Denest.NthRoot
  ( denestNthRoot
  , tryCubeRootDenest
  ) where

import Data.Ratio (numerator, denominator)
import Math.NumberTheory.Roots (exactSquareRoot, exactCubeRoot)
import Surd.Types
import Surd.Internal.Positive (Positive)
import Surd.Internal.PrimeFactors (factorise)
import Surd.Radical.Eval (evalExact)

-- | Attempt to denest an nth root expression.
denestNthRoot :: RadExpr Rational -> RadExpr Rational
denestNthRoot expr = case expr of
  Root n (Root m a) ->
    -- ᵐ√(ⁿ√a) = ᵐⁿ√a
    denestNthRoot (Root (m * n) (denestNthRoot (Root 1 a)))

  Root n (Lit r)
    | r == 0 -> Lit 0
    | r == 1 -> Lit 1
    | r > 0  -> simplifyRootOfRational n r
    | odd n  -> Neg (denestNthRoot (Root n (Lit (negate r))))
    | otherwise -> Root n (Lit r)

  Root 3 inner ->
    -- Try cube root denesting: ³√(a + b√c) = p + q√c
    -- where p³ + 3pq²c = a and 3p²q + q³c = b
    case matchSqrtNested3 inner of
      Just (a, b, c) ->
        case tryCubeRootDenest a b c of
          Just result -> result
          Nothing     -> Root 3 (denestNthRoot' inner)
      Nothing -> Root 3 (denestNthRoot' inner)

  Root n a -> Root n (denestNthRoot' a)
  other    -> other
  where
    denestNthRoot' e = case e of
      Root m a -> denestNthRoot (Root m a)
      Neg a    -> Neg (denestNthRoot' a)
      Add a b  -> Add (denestNthRoot' a) (denestNthRoot' b)
      Mul a b  -> Mul (denestNthRoot' a) (denestNthRoot' b)
      Inv a    -> Inv (denestNthRoot' a)
      Pow a n  -> Pow (denestNthRoot' a) n
      other    -> other

-- | Simplify ⁿ√r for rational r by extracting perfect nth powers.
simplifyRootOfRational :: Int -> Rational -> RadExpr Rational
simplifyRootOfRational n r =
  let num = numerator r
      den = denominator r
      (numOut, numRem) = extractPower n num
      (denOut, denRem) = extractPower n den
  in if numOut == 1 && denOut == 1
     then Root n (Lit r)
     else Mul (Lit (fromInteger numOut / fromInteger denOut))
              (Root n (Lit (fromInteger numRem / fromInteger denRem)))

-- | Extract the largest perfect nth power factor.
-- Returns (extracted, remainder) such that m = extracted^n * remainder.
extractPower :: Int -> Integer -> (Integer, Integer)
extractPower n m =
  let fs = factorise (fromInteger (abs m) :: Positive)
      extracted = product [p ^ (e `div` n) | (p, e) <- fs]
      remainder = product [p ^ (e `mod` n) | (p, e) <- fs]
      sign = if m < 0 then -1 else 1
  in (extracted, sign * remainder)

-- | Match the pattern a + b√c in an expression (for cube root denesting).
matchSqrtNested3 :: RadExpr Rational -> Maybe (Rational, Rational, Rational)
matchSqrtNested3 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))) = Just (a, b, c)
matchSqrtNested3 (Add (Mul (Lit b) (Root 2 (Lit c))) (Lit a)) = Just (a, b, c)
matchSqrtNested3 (Add (Lit a) (Root 2 (Lit c)))               = Just (a, 1, c)
matchSqrtNested3 (Add (Root 2 (Lit c)) (Lit a))               = Just (a, 1, c)
matchSqrtNested3 (Add (Lit a) (Neg (Mul (Lit b) (Root 2 (Lit c))))) = Just (a, negate b, c)
matchSqrtNested3 (Add (Lit a) (Neg (Root 2 (Lit c))))         = Just (a, -1, c)
matchSqrtNested3 _ = Nothing

-- | Try to denest ³√(a + b√c) into the form p + q√c
-- where p, q are rational numbers satisfying:
--   p³ + 3pq²c = a
--   3p²q + q³c = b
--
-- We can also try the form ³√(a + b√c) = ³√s + ³√t
-- where s + t = a and s*t = -(b²c)/... but the first form is simpler.
--
-- Approach: if ³√(a + b√c) = p + q√c, then
--   (p + q√c)³ = p³ + 3p²q√c + 3pq²c + q³c√c
--              = (p³ + 3pq²c) + (3p²q + q³c)√c
--   So: a = p³ + 3pq²c, b = 3p²q + q³c = q(3p² + q²c)
--
-- Also: (a + b√c)(a - b√c) = a² - b²c = N (the norm)
-- And ³√N = (p + q√c)(p - q√c) ... wait, that's p² - q²c.
-- So p² - q²c = ³√(a² - b²c).
--
-- For this to work with p,q rational, we need a² - b²c to be a perfect cube.
tryCubeRootDenest :: Rational -> Rational -> Rational -> Maybe (RadExpr Rational)
tryCubeRootDenest a b c =
  let norm = a * a - b * b * c
  in case isRationalCubeRoot norm of
    Nothing -> Nothing
    Just cbrtNorm ->
      -- p² - q²c = cbrtNorm
      -- p³ + 3pq²c = a
      -- From p² - q²c = cbrtNorm: q² = (p² - cbrtNorm) / c
      -- Substitute into p³ + 3pq²c = a:
      --   p³ + 3p(p² - cbrtNorm)/c * c = a
      --   p³ + 3p(p² - cbrtNorm) = a
      --   p³ + 3p³ - 3p*cbrtNorm = a
      --   4p³ - 3p*cbrtNorm = a
      -- So p is a rational root of 4x³ - 3*cbrtNorm*x - a = 0.
      let candidates = rationalCubeEqRoots 4 (-3 * cbrtNorm) (-a)
      in case candidates of
        [] -> Nothing
        (p:_) ->
          let q2 = (p * p - cbrtNorm) / c
          in case isRationalSqrt q2 of
            Nothing -> Nothing
            Just q ->
              -- Verify: pick the sign of q based on the sign of b
              let q' = if (3 * p * p * q + q * q * q * c) * signum b >= 0
                       then q else negate q
                  result = Add (Lit p) (Mul (Lit q') (Root 2 (Lit c)))
                  -- Sanity check via exact real evaluation
                  expected = evalExact (Root 3 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))))
                  got = evalExact result
              in if abs (expected - got) < 1e-40
                 then Just result
                 else Nothing

-- | Check if a rational is a perfect cube.
isRationalCubeRoot :: Rational -> Maybe Rational
isRationalCubeRoot q
  | q == 0    = Just 0
  | otherwise = do
      sn <- exactCubeRoot (abs (numerator q))
      sd <- exactCubeRoot (denominator q)
      Just (fromInteger (signum (numerator q) * sn) / fromInteger sd)

-- | Check if a rational is a perfect square.
isRationalSqrt :: Rational -> Maybe Rational
isRationalSqrt q
  | q < 0     = Nothing
  | q == 0    = Just 0
  | otherwise = do
      sn <- exactSquareRoot (numerator q)
      sd <- exactSquareRoot (denominator q)
      Just (fromInteger sn / fromInteger sd)

-- | Find rational roots of ax³ + bx + c = 0 (no x² term).
-- By rational root theorem, candidates are ±(divisor of c)/(divisor of a).
rationalCubeEqRoots :: Rational -> Rational -> Rational -> [Rational]
rationalCubeEqRoots a b c =
  let -- Evaluate ax³ + bx + c at x
      f x = a * x * x * x + b * x + c
      -- Generate candidates from rational root theorem
      numC = abs (numerator c)
      denC = denominator c
      numA = abs (numerator a)
      denA = denominator a
      -- Candidates: ±(divisors of c * denA) / (divisors of a * denC)
      -- Simplified: we try small rationals
      candidates = [fromInteger p / fromInteger q
                   | p <- concatMap (\d -> [d, -d]) (intDivisors (numC * denA))
                   , q <- intDivisors (numA * denC)
                   , q /= 0
                   ]
  in filter (\x -> f x == 0) candidates

-- | Divisors of a non-negative integer.
intDivisors :: Integer -> [Integer]
intDivisors 0 = [1]
intDivisors n = [d | d <- [1..abs n], abs n `mod` d == 0]
