-- |
-- Module      : Surd.Radical.Denest.NthRoot
-- Description : General nth-root denesting and cube root denesting
-- Stability   : experimental
--
-- Denesting of general nth-root expressions. Currently implements:
--
-- 1. __Perfect power extraction__: @nth-root(a^n * b) = a * nth-root(b)@
-- 2. __Root-index reduction__: @nth-root(x^k)@ simplification
-- 3. __Nested root collapse__: @mth-root(nth-root x) = (m*n)th-root x@
-- 4. __Cube root denesting__: @cbrt(a + b*sqrt c) = p + q*sqrt c@
--
-- === Cube root denesting criterion
--
-- For @cbrt(a + b*sqrt c)@ to denest into @p + q*sqrt c@ with @p, q@
-- rational, we need:
--
-- * The norm @a^2 - b^2*c@ must be a perfect cube.
-- * The depressed cubic @4p^3 - 3*(cbrt(a^2 - b^2*c))*p - a = 0@
--   must have a rational root.
-- * @(p^2 - cbrt(a^2 - b^2*c)) / c@ must be a perfect square (giving @q^2@).
--
-- More general Galois-theoretic denesting is provided by
-- "Surd.Radical.Denest.Landau".
module Surd.Radical.Denest.NthRoot
  ( -- * Top-level denesting
    denestNthRoot,

    -- * Cube root algorithm
    tryCubeRootDenest,
  )
where

import Data.Ratio (denominator, numerator)
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Math.NumberTheory.Roots (exactCubeRoot, exactSquareRoot)
import Surd.Radical.Eval (evalExact)
import Surd.Types

-- | Attempt to denest an nth root expression.
--
-- Tries, in order:
--
-- 1. Nested root collapse: @mth-root(nth-root a)@ -> @(m*n)th-root a@
-- 2. Perfect power extraction for literal radicands
-- 3. Cube root denesting for @cbrt(a + b*sqrt c)@
-- 4. Recursive denesting of sub-expressions
denestNthRoot :: RadExpr Rational -> RadExpr Rational
denestNthRoot expr = case expr of
  Root n (Root m a) ->
    -- mth-root(nth-root a) = (m*n)th-root a
    denestNthRoot (Root (m * n) (denestNthRoot (Root 1 a)))
  Root n (Lit r)
    | r == 0 -> Lit 0
    | r == 1 -> Lit 1
    | r > 0 -> simplifyRootOfRational n r
    | odd n -> Neg (denestNthRoot (Root n (Lit (negate r))))
    | otherwise -> Root n (Lit r)
  Root 3 inner ->
    -- Try cube root denesting: cbrt(a + b*sqrt c) = p + q*sqrt c
    -- where p^3 + 3*p*q^2*c = a and 3*p^2*q + q^3*c = b
    case matchSqrtNested3 inner of
      Just (a, b, c) ->
        case tryCubeRootDenest a b c of
          Just result -> result
          Nothing -> Root 3 (denestNthRoot' inner)
      Nothing -> Root 3 (denestNthRoot' inner)
  Root n a -> Root n (denestNthRoot' a)
  other -> other
  where
    denestNthRoot' e = case e of
      Root m a -> denestNthRoot (Root m a)
      Neg a -> Neg (denestNthRoot' a)
      Add a b -> Add (denestNthRoot' a) (denestNthRoot' b)
      Mul a b -> Mul (denestNthRoot' a) (denestNthRoot' b)
      Inv a -> Inv (denestNthRoot' a)
      Pow a n -> Pow (denestNthRoot' a) n
      other -> other

-- | Simplify @nth-root(r)@ for rational @r@ by extracting perfect nth powers.
--
-- Factors @r = (numOut/denOut)^n * (numRem/denRem)@ and returns
-- @(numOut/denOut) * nth-root(numRem/denRem)@.
simplifyRootOfRational :: Int -> Rational -> RadExpr Rational
simplifyRootOfRational n r =
  let num = numerator r
      den = denominator r
      (numOut, numRem) = extractPower n num
      (denOut, denRem) = extractPower n den
   in if numOut == 1 && denOut == 1
        then Root n (Lit r)
        else
          Mul
            (Lit (fromInteger numOut / fromInteger denOut))
            (Root n (Lit (fromInteger numRem / fromInteger denRem)))

-- | Extract the largest perfect nth power factor from an integer.
--
-- Returns @(extracted, remainder)@ such that @m = extracted^n * remainder@.
extractPower :: Int -> Integer -> (Integer, Integer)
extractPower n m =
  let fs = factorise (fromInteger (abs m) :: Positive)
      extracted = product [p ^ (e `div` n) | (p, e) <- fs]
      remainder = product [p ^ (e `mod` n) | (p, e) <- fs]
      sign = if m < 0 then -1 else 1
   in (extracted, sign * remainder)

-- | Match the pattern @a + b*sqrt c@ in an expression (for cube root denesting).
-- Handles several syntactic variants.
matchSqrtNested3 :: RadExpr Rational -> Maybe (Rational, Rational, Rational)
matchSqrtNested3 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))) = Just (a, b, c)
matchSqrtNested3 (Add (Mul (Lit b) (Root 2 (Lit c))) (Lit a)) = Just (a, b, c)
matchSqrtNested3 (Add (Lit a) (Root 2 (Lit c))) = Just (a, 1, c)
matchSqrtNested3 (Add (Root 2 (Lit c)) (Lit a)) = Just (a, 1, c)
matchSqrtNested3 (Add (Lit a) (Neg (Mul (Lit b) (Root 2 (Lit c))))) = Just (a, negate b, c)
matchSqrtNested3 (Add (Lit a) (Neg (Root 2 (Lit c)))) = Just (a, -1, c)
matchSqrtNested3 _ = Nothing

-- | Try to denest @cbrt(a + b*sqrt c)@ into the form @p + q*sqrt c@
-- where @p, q@ are rational numbers satisfying:
--
-- @
-- p^3 + 3*p*q^2*c = a
-- 3*p^2*q + q^3*c = b
-- @
--
-- The algorithm:
--
-- 1. Compute the norm @N = a^2 - b^2*c@. If @N@ is not a perfect cube, fail.
-- 2. From @p^2 - q^2*c = cbrt N@, substitute into the first equation to get
--    the depressed cubic @4*p^3 - 3*cbrt(N)*p - a = 0@.
-- 3. Find rational roots of this cubic (rational root theorem).
-- 4. Compute @q^2 = (p^2 - cbrt N) / c@ and check it is a perfect square.
-- 5. Verify the result via exact real evaluation.
tryCubeRootDenest :: Rational -> Rational -> Rational -> Maybe (RadExpr Rational)
tryCubeRootDenest a b c =
  let norm = a * a - b * b * c
   in case isRationalCubeRoot norm of
        Nothing -> Nothing
        Just cbrtNorm ->
          -- p^2 - q^2*c = cbrtNorm
          -- p^3 + 3*p*q^2*c = a
          -- From p^2 - q^2*c = cbrtNorm: q^2 = (p^2 - cbrtNorm) / c
          -- Substitute into p^3 + 3*p*q^2*c = a:
          --   p^3 + 3*p*(p^2 - cbrtNorm)/c * c = a
          --   p^3 + 3*p*(p^2 - cbrtNorm) = a
          --   p^3 + 3*p^3 - 3*p*cbrtNorm = a
          --   4*p^3 - 3*p*cbrtNorm = a
          -- So p is a rational root of 4*x^3 - 3*cbrtNorm*x - a = 0.
          let candidates = rationalCubeEqRoots 4 (-3 * cbrtNorm) (-a)
           in case candidates of
                [] -> Nothing
                (p : _) ->
                  let q2 = (p * p - cbrtNorm) / c
                   in case isRationalSqrt q2 of
                        Nothing -> Nothing
                        Just q ->
                          -- Verify: pick the sign of q based on the sign of b
                          let q' =
                                if (3 * p * p * q + q * q * q * c) * signum b >= 0
                                  then q
                                  else negate q
                              result = Add (Lit p) (Mul (Lit q') (Root 2 (Lit c)))
                              -- Sanity check via exact real evaluation
                              expected = evalExact (Root 3 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))))
                              got = evalExact result
                           in if abs (expected - got) < 1e-40
                                then Just result
                                else Nothing

-- | Check if a rational is a perfect cube.
-- Returns @Just (cbrt q)@ if @q@ is a perfect cube, @Nothing@ otherwise.
isRationalCubeRoot :: Rational -> Maybe Rational
isRationalCubeRoot q
  | q == 0 = Just 0
  | otherwise = do
      sn <- exactCubeRoot (abs (numerator q))
      sd <- exactCubeRoot (denominator q)
      Just (fromInteger (signum (numerator q) * sn) / fromInteger sd)

-- | Check if a rational is a perfect square.
-- Returns @Just (sqrt q)@ if @q >= 0@ is a perfect square, @Nothing@ otherwise.
isRationalSqrt :: Rational -> Maybe Rational
isRationalSqrt q
  | q < 0 = Nothing
  | q == 0 = Just 0
  | otherwise = do
      sn <- exactSquareRoot (numerator q)
      sd <- exactSquareRoot (denominator q)
      Just (fromInteger sn / fromInteger sd)

-- | Find rational roots of @a*x^3 + b*x + c = 0@ (no @x^2@ term).
--
-- Uses the rational root theorem: candidates are @+/-(divisor of c) / (divisor of a)@.
rationalCubeEqRoots :: Rational -> Rational -> Rational -> [Rational]
rationalCubeEqRoots a b c =
  let -- Evaluate a*x^3 + b*x + c at x
      f x = a * x * x * x + b * x + c
      -- Generate candidates from rational root theorem
      numC = abs (numerator c)
      denC = denominator c
      numA = abs (numerator a)
      denA = denominator a
      -- Candidates: +/-(divisors of c * denA) / (divisors of a * denC)
      -- Simplified: we try small rationals
      candidates =
        [ fromInteger p / fromInteger q
          | p <- concatMap (\d -> [d, -d]) (intDivisors (numC * denA)),
            q <- intDivisors (numA * denC),
            q /= 0
        ]
   in filter (\x -> f x == 0) candidates

-- | Divisors of a non-negative integer.
intDivisors :: Integer -> [Integer]
intDivisors 0 = [1]
intDivisors n = [d | d <- [1 .. abs n], abs n `mod` d == 0]
