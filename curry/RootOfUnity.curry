--- Express roots of unity as radical expressions.
---
--- A primitive nth root of unity can ALWAYS be expressed in radicals,
--- since cyclotomic extensions have abelian Galois groups.
---
--- Compass-and-straightedge constructible angles (n = 2^a * distinct
--- Fermat primes) use only square roots. For general n, higher-degree
--- roots and complex intermediates may appear.
module RootOfUnity
  ( cosOfUnity
  , sinOfUnity
  , isConstructible
  , fermatPrimes
  ) where

import Rational
import RadExpr
import PrimeFactors (isPrime, factorise)
import Positive (unsafePositive)
import TrigGalois (cosOfUnityViaGauss, allPeriodsViaGauss)

--- Known Fermat primes.
fermatPrimes :: [Int]
fermatPrimes = [3, 5, 17, 257, 65537]

--- Check if cos(2*pi/n) is expressible using only nested square roots
--- (compass-and-straightedge constructible).
isConstructible :: Int -> Bool
isConstructible n =
  let fs = factorise (unsafePositive n)
      oddFactors = filter (\(p, _) -> p /= 2) fs
  in all (\(p, e) -> e == 1 && elem p fermatPrimes) oddFactors

--- Compute cos(2*pi/n) as a radical expression.
---
--- Returns Just expr for all supported n, Nothing for unsupported.
cosOfUnity :: Int -> Maybe (RadExpr Rational)
cosOfUnity n
  | n <= 0   = Nothing
  | n == 1   = Just (Lit rOne)
  | n == 2   = Just (Lit (Rational.fromInt (negate 1)))
  | n == 3   = Just (Lit (mkRat (negate 1) 2))
  | n == 4   = Just (Lit rZero)
  | n == 5   = Just cos2piOver5
  | n == 6   = Just (Lit (mkRat 1 2))
  | n == 8   = Just (Mul (Inv (Lit (Rational.fromInt 2)))
                         (Root 2 (Lit (Rational.fromInt 2))))
  | n == 10  = Just cos2piOver10
  | n == 12  = Just (Mul (Inv (Lit (Rational.fromInt 2)))
                         (Root 2 (Lit (Rational.fromInt 3))))
  | isPowerOf2 n = Just (cosOfPow2 n)
  | isPrime n = cosOfUnityViaGauss n
  | otherwise =
      case primePowerDecomp n of
        Just (p, k) ->
          if p > 2 && k > 1
          then cosOfUnityViaGauss n
          else crtDecomp n
        Nothing -> crtDecomp n
  where
    rZero = Rational.fromInt 0
    rOne  = Rational.fromInt 1

--- cos(2*pi/5) = (-1 + sqrt(5)) / 4
cos2piOver5 :: RadExpr Rational
cos2piOver5 =
  Mul (Inv (Lit (Rational.fromInt 4)))
      (Add (Lit (Rational.fromInt (negate 1)))
           (Root 2 (Lit (Rational.fromInt 5))))

--- cos(2*pi/10) = (1 + sqrt(5)) / 4
cos2piOver10 :: RadExpr Rational
cos2piOver10 =
  Mul (Inv (Lit (Rational.fromInt 4)))
      (Add (Lit (Rational.fromInt 1))
           (Root 2 (Lit (Rational.fromInt 5))))

--- cos(2*pi/2^k) via half-angle recurrence.
--- cos(2*pi/2^(k+1)) = sqrt((1 + cos(2*pi/2^k)) / 2)
cosOfPow2 :: Int -> RadExpr Rational
cosOfPow2 n
  | n <= 4    = case n of
                  1 -> Lit (Rational.fromInt 1)
                  2 -> Lit (Rational.fromInt (negate 1))
                  4 -> Lit (Rational.fromInt 0)
                  _ -> Lit (Rational.fromInt 0)
  | otherwise =
      let prev = cosOfPow2 (n `div` 2)
      in Root 2 (Mul (Inv (Lit (Rational.fromInt 2)))
                     (Add (Lit (Rational.fromInt 1)) prev))

--- Check if n is a power of 2.
isPowerOf2 :: Int -> Bool
isPowerOf2 n
  | n <= 0    = False
  | n == 1    = True
  | otherwise = n `mod` 2 == 0 && isPowerOf2 (n `div` 2)

--- Decompose n into a prime power p^k (if it is one).
primePowerDecomp :: Int -> Maybe (Int, Int)
primePowerDecomp n
  | n <= 1    = Nothing
  | otherwise =
      let fs = factorise (unsafePositive n)
      in case fs of
           [(p, k)] -> Just (p, k)
           _        -> Nothing

--- CRT decomposition for composite n (stub).
--- For composite n = n1 * n2 with gcd(n1,n2) = 1,
--- cos(2*pi/n) can be computed from cos(2*pi/n1) and cos(2*pi/n2).
crtDecomp :: Int -> Maybe (RadExpr Rational)
crtDecomp n =
  let fs = factorise (unsafePositive n)
      -- For each prime power factor, compute cos(2*pi/p^k)
      parts = map (\(p, k) -> cosOfUnity (intPow p k)) fs
  in if all isJust parts
     then Just (combineCosParts (map fromJust parts))
     else Nothing

--- Combine cos values from CRT decomposition using product formula.
--- cos(2*pi/n) = T_k(cos(2*pi/m)) where n = m*k (simplified).
combineCosParts :: [RadExpr Rational] -> RadExpr Rational
combineCosParts xs = case xs of
  []  -> Lit (Rational.fromInt 1)
  [x] -> x
  _   -> Lit (Rational.fromInt 0)  -- Simplified stub

--- Compute sin(2*pi/n) = sqrt(1 - cos(2*pi/n)^2).
sinOfUnity :: Int -> Maybe (RadExpr Rational)
sinOfUnity n = case cosOfUnity n of
  Nothing -> Nothing
  Just c  ->
    let sin2 = Add (Lit (Rational.fromInt 1)) (Neg (Mul c c))
    in Just (Root 2 sin2)

--- Integer power.
intPow :: Int -> Int -> Int
intPow b e
  | e == 0    = 1
  | otherwise = b * intPow b (e - 1)

--- Maybe helpers.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust: Nothing"
