-- | Express roots of unity as radical expressions.
--
-- A primitive nth root of unity ζₙ = e^(2πi/n) can ALWAYS be expressed
-- in radicals, since cyclotomic extensions have abelian Galois groups
-- (hence solvable by the Kronecker-Weber theorem).
--
-- The compass-and-straightedge constructible case (n = 2^a · distinct
-- Fermat primes) is the special case where only square roots are needed.
-- For general n, the radical expression may involve roots of higher
-- degree (cube roots for factors of 3 in φ(n), fifth roots for factors
-- of 5, etc.) and may pass through complex intermediates due to the
-- casus irreducibilis.
module Surd.Trig.RootOfUnity
  ( cosOfUnity
  , sinOfUnity
  , isConstructible
  , fermatPrimes
  ) where

import Data.Bits qualified as Bits
import Data.Ratio ((%))
import Surd.Types
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise, isPrime)
import Surd.Trig.Galois (cosOfUnityViaGauss)

-- | Known Fermat primes (these are the only known ones, and it's
-- conjectured there are no others).
fermatPrimes :: [Integer]
fermatPrimes = [3, 5, 17, 257, 65537]

-- | Check if cos(2π/n) is expressible using only nested square roots
-- (i.e., compass-and-straightedge constructible).
--
-- This is true iff n = 2^a · product of distinct Fermat primes.
--
-- Note: cos(2π/n) is expressible in radicals (possibly involving
-- higher-degree roots) for ALL n, not just constructible ones.
isConstructible :: Int -> Bool
isConstructible n =
  let fs = factorise (fromIntegral n :: Positive)
      oddFactors = [(p, e) | (p, e) <- fs, p /= 2]
  in all (\(p, e) -> e == 1 && p `elem` fermatPrimes) oddFactors

-- | Compute cos(2π/n) as a radical expression.
--
-- Always succeeds for n ≥ 1. For small n and constructible cases,
-- returns optimised closed-form expressions. For general n, uses
-- the Gauss period descent which may involve complex intermediates.
cosOfUnity :: Int -> Maybe (RadExpr Rational)
cosOfUnity n
  | n <= 0    = Nothing
  -- Small exact cases (hand-optimised)
  | n == 1    = Just (Lit 1)
  | n == 2    = Just (Lit (-1))
  | n == 3    = Just (Lit (-1 % 2))
  | n == 4    = Just (Lit 0)
  | n == 5    = Just cos2piOver5
  | n == 6    = Just (Lit (1 % 2))
  | n == 8    = Just (Mul (Inv (Lit 2)) (Root 2 (Lit 2)))
  | n == 10   = Just cos2piOver10
  | n == 12   = Just (Mul (Inv (Lit 2)) (Root 2 (Lit 3)))
  | n == 15   = Just cos2piOver15
  | n == 16   = Just cos2piOver16
  | n == 17   = Just cos2piOver17
  | n == 20   = Just cos2piOver20
  | n == 24   = Just cos2piOver24
  -- Powers of 2: half-angle recurrence (only square roots)
  | isPowerOf2 n = Just (cosOfPow2 n)
  -- Odd prime or odd prime power: Gauss period descent
  -- (Z/p^k Z)* is cyclic for odd primes, so this always works
  | isPrime (fromIntegral n) = cosOfUnityViaGauss n
  | Just (p, k) <- primePowerDecomp n, p > 2, k > 1 =
      cosOfUnityViaGauss n
  -- General composite: decompose via CRT / product formulas
  | otherwise = cosOfUnityComposite n

-- | Compute sin(2π/n) as a radical expression.
sinOfUnity :: Int -> Maybe (RadExpr Rational)
sinOfUnity n
  | n <= 0    = Nothing
  | n == 1    = Just (Lit 0)
  | n == 2    = Just (Lit 0)
  | n == 3    = Just (Mul (Lit (1 % 2)) (Root 2 (Lit 3)))
  | n == 4    = Just (Lit 1)
  | n == 6    = Just (Mul (Lit (1 % 2)) (Root 2 (Lit 3)))
  | n == 8    = Just (Mul (Inv (Lit 2)) (Root 2 (Lit 2)))
  | n == 12   = Just (Lit (1 % 2))
  | otherwise =
      -- sin(2π/n) = √(1 - cos²(2π/n))
      case cosOfUnity n of
        Nothing -> Nothing
        Just c  ->
          let sin2 = Add (Lit 1) (Neg (Mul c c))
          in Just (Root 2 sin2)

-- cos(2π/5) = (√5 - 1) / 4
cos2piOver5 :: RadExpr Rational
cos2piOver5 = Mul (Inv (Lit 4)) (Add (Root 2 (Lit 5)) (Lit (-1)))

-- cos(2π/10) = cos(π/5) = (1 + √5) / 4
cos2piOver10 :: RadExpr Rational
cos2piOver10 = Mul (Inv (Lit 4)) (Add (Lit 1) (Root 2 (Lit 5)))

-- cos(2π/15) = (1 + √5 + √(30 - 6√5)) / 8
cos2piOver15 :: RadExpr Rational
cos2piOver15 =
  Mul (Inv (Lit 8))
    (Add (Add (Lit 1) (Root 2 (Lit 5)))
         (Root 2 (Add (Lit 30) (Neg (Mul (Lit 6) (Root 2 (Lit 5)))))))

-- cos(2π/16) = cos(π/8) = √(2 + √2) / 2
cos2piOver16 :: RadExpr Rational
cos2piOver16 =
  Mul (Inv (Lit 2)) (Root 2 (Add (Lit 2) (Root 2 (Lit 2))))

-- cos(2π/17): the famous 17-gon (Gauss)
-- 16·cos(2π/17) = -1 + √17 + √(34-2√17) + 2·√(17+3√17-√(34-2√17)-2·√(34+2√17))
cos2piOver17 :: RadExpr Rational
cos2piOver17 =
  let s17 = Root 2 (Lit 17)
      a = Root 2 (Add (Lit 34) (Neg (Mul (Lit 2) s17)))
      b = Root 2 (Add (Lit 34) (Mul (Lit 2) s17))
  in Mul (Inv (Lit 16))
       (Add (Add (Add (Lit (-1)) s17) a)
            (Mul (Lit 2) (Root 2
              (Add (Add (Lit 17) (Mul (Lit 3) s17))
                   (Add (Neg a) (Neg (Mul (Lit 2) b)))))))

-- cos(2π/20) = cos(π/10) = √(10 + 2√5) / 4
cos2piOver20 :: RadExpr Rational
cos2piOver20 =
  Mul (Inv (Lit 4)) (Root 2 (Add (Lit 10) (Mul (Lit 2) (Root 2 (Lit 5)))))

-- cos(2π/24) = cos(π/12) = (√6 + √2) / 4
cos2piOver24 :: RadExpr Rational
cos2piOver24 =
  Mul (Inv (Lit 4)) (Add (Root 2 (Lit 6)) (Root 2 (Lit 2)))

-- | cos(2π/2^k) via the half-angle recurrence:
-- cos(2π/2^(k+1)) = √((1 + cos(2π/2^k)) / 2)
cosOfPow2 :: Int -> RadExpr Rational
cosOfPow2 n = go n
  where
    go 1 = Lit 1
    go 2 = Lit (-1)
    go 4 = Lit 0
    go k =
      let half = go (k `div` 2)
      in Root 2 (Mul (Inv (Lit 2)) (Add (Lit 1) half))

-- | Decompose n into (p, k) if n = p^k for prime p, k ≥ 1.
primePowerDecomp :: Int -> Maybe (Int, Int)
primePowerDecomp n =
  let fs = factorise (fromIntegral n :: Positive)
  in case fs of
    [(p, k)] -> Just (fromIntegral p, k)
    _        -> Nothing

-- | cos(2π/p^k) for prime p and k ≥ 2.
--
-- Uses the identity: cos(2π/p^k) is a root of the p-th Chebyshev-like
-- polynomial applied to cos(2π/p^{k-1}), specifically:
-- The minimal polynomial of 2cos(2π/p^k) over Q(2cos(2π/p^{k-1}))
-- has degree p and can be solved in radicals (the Galois group is cyclic).
--
-- For p = 2, this reduces to the half-angle formula (square root).
-- For odd p, this involves p-th roots.
-- | cos(2π/n) for composite n.
--
-- Strategy: factor n = n₁·n₂ where gcd(n₁, n₂) = 1, and use
-- the identity cos(2π/n) = cos(2π/(n₁·n₂)).
-- By CRT, there exist a, b with a·n₂ + b·n₁ = 1, so
-- 2π/n = a·(2π/n₁) + b·(2π/n₂), and we can use the
-- addition formula for cosine.
cosOfUnityComposite :: Int -> Maybe (RadExpr Rational)
cosOfUnityComposite n =
  let fs = factorise (fromIntegral n :: Positive)
  in case fs of
    []  -> Nothing
    [_] -> Nothing  -- prime power, handled elsewhere
    _   ->
      -- Split into two coprime factors n = n₁·n₂ with gcd(n₁,n₂) = 1.
      -- By CRT: a·n₁ + b·n₂ = 1 for integers a, b.
      -- Then 1/(n₁·n₂) = a/n₂ + b/n₁, so:
      --   2π/n = 2πa/n₂ + 2πb/n₁
      --   cos(2π/n) = cos(2πa/n₂ + 2πb/n₁)
      --             = cos(2πa/n₂)·cos(2πb/n₁) - sin(2πa/n₂)·sin(2πb/n₁)
      --
      -- Chebyshev T_k(cos θ) = cos(kθ) gives cos(2πk/m) from cos(2π/m).
      -- sinFromCos gives |sin|; the sign is determined from the quadrant.
      let ((p1, e1):_) = fs
          n1 = fromIntegral p1 ^ e1
          n2 = n `div` n1
          (a, b) = extGcdInt n1 n2  -- a*n1 + b*n2 = 1
          -- Reduce to canonical range
          a' = a `mod` n2
          b' = b `mod` n1
          -- Determine signs of sin(2πa'/n₂) and sin(2πb'/n₁)
          sA = sinSignFromAngle a' n2
          sB = sinSignFromAngle b' n1
          signFactor = sA * sB
      in do
        cosBase1 <- cosOfUnity n2  -- cos(2π/n₂)
        cosBase2 <- cosOfUnity n1  -- cos(2π/n₁)
        let cosAE = chebyshevSimple a' cosBase1  -- cos(2πa'/n₂)
            cosBE = chebyshevSimple b' cosBase2  -- cos(2πb'/n₁)
        if signFactor == 0
          then -- One of the sines is zero; cos(α+β) = cos(α)·cos(β)
            Just (Mul cosAE cosBE)
          else do
            let sinAbsA = sinFromCos cosAE  -- |sin(2πa'/n₂)|
                sinAbsB = sinFromCos cosBE  -- |sin(2πb'/n₁)|
                -- cos(α+β) = cos(α)cos(β) - signFactor · |sin(α)|·|sin(β)|
                sinProduct = Mul sinAbsA sinAbsB
                result = if signFactor == 1
                         then Add (Mul cosAE cosBE) (Neg sinProduct)
                         else Add (Mul cosAE cosBE) sinProduct
            Just result

-- | Sign of sin(2πk/m) for k ∈ [0, m-1]:
--   0 < k/m < 1/2 → positive (+1)
--   k/m = 0 or 1/2 → zero (0)
--   1/2 < k/m < 1 → negative (-1)
sinSignFromAngle :: Int -> Int -> Int
sinSignFromAngle k m
  | k' == 0       = 0
  | 2 * k' < m    = 1
  | 2 * k' == m   = 0
  | otherwise      = -1
  where k' = k `mod` m

-- | Extended GCD for integers: returns (a, b) such that a*x + b*y = gcd(x,y).
extGcdInt :: Int -> Int -> (Int, Int)
extGcdInt 0 _ = (0, 1)
extGcdInt x y =
  let (q, r) = y `divMod` x
      (a, b) = extGcdInt r x
  in (b - q * a, a)

-- | Simple Chebyshev: T_k(cos θ) = cos(kθ).
-- Computes T_k(x) symbolically using the recurrence.
chebyshevSimple :: Int -> RadExpr Rational -> RadExpr Rational
chebyshevSimple 0 _ = Lit 1
chebyshevSimple 1 x = x
chebyshevSimple k x = go 2 (Lit 1) x
  where
    go n t0 t1
      | n > k     = t1
      | otherwise  =
          let t2 = Add (Mul (Mul (Lit 2) x) t1) (Neg t0)
          in go (n + 1) t1 t2

-- | sin from cos: sin(θ) = √(1 - cos²(θ)) (positive in first quadrant).
sinFromCos :: RadExpr Rational -> RadExpr Rational
sinFromCos c = Root 2 (Add (Lit 1) (Neg (Mul c c)))

isPowerOf2 :: Int -> Bool
isPowerOf2 n = n > 0 && (n Bits..&. (n - 1)) == 0
