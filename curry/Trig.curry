--- Exact symbolic evaluation of trigonometric functions at
--- rational multiples of pi.
---
--- Every trig value at a rational multiple of pi can be expressed in
--- radicals, since cyclotomic extensions have abelian (hence solvable)
--- Galois groups.
---
--- The primary entry points are cosExact and sinExact, which compute
--- cos(p*pi/q) and sin(p*pi/q) as radical expressions.
module Trig
  ( cosExact
  , sinExact
  , tanExact
  , TrigResult(..)
  , chebyshev
  ) where

import Rational
import Poly (Poly)
import RadExpr
import RootOfUnity (cosOfUnity)
import Cyclotomic (cyclotomic)
import Normalize (normalize)
import Eval (evalDouble)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Result of exact trig evaluation.
data TrigResult
  = Radical (RadExpr Rational)
  | MinPoly Poly

--- Compute cos(p*pi/q) exactly.
---
--- Reduces to cos(2*pi/n) via standard identities, then uses the
--- root-of-unity machinery (Gauss period descent).
cosExact :: Int -> Int -> TrigResult
cosExact p q
  | q <= 0    = error "cosExact: non-positive denominator"
  | otherwise =
      let g = gcdInt (absInt p) q
          p' = p `div` g
          q' = q `div` g
      in cosReduced p' q'

--- Compute sin(p*pi/q) exactly.
sinExact :: Int -> Int -> TrigResult
sinExact p q
  | q <= 0    = error "sinExact: non-positive denominator"
  | otherwise =
      let g = gcdInt (absInt p) q
          p' = p `div` g
          q' = q `div` g
      in sinReduced p' q'

--- Compute tan(p*pi/q) exactly, as sin/cos.
tanExact :: Int -> Int -> Maybe TrigResult
tanExact p q = case (sinExact p q, cosExact p q) of
  (Radical s, Radical c) -> Just (Radical (Mul s (Inv c)))
  _                      -> Nothing

--- Internal: cos with reduced fraction.
cosReduced :: Int -> Int -> TrigResult
cosReduced p q =
  let p' = p `mod` (2 * q)
      p'' = if p' < 0 then p' + 2 * q else p'
  in cosInRange p'' q

--- Internal: sin with reduced fraction.
sinReduced :: Int -> Int -> TrigResult
sinReduced p q =
  let p' = p `mod` (2 * q)
      p'' = if p' < 0 then p' + 2 * q else p'
      positive = p'' >= 0 && p'' <= q
  in if p'' == 0 || p'' == q
     then Radical (Lit rZero)
     else case cosExact p q of
            Radical c ->
              let sin2 = Add (Lit rOne) (Neg (Mul c c))
                  sinExpr = Root 2 sin2
                  signed = if positive then sinExpr else Neg sinExpr
              in Radical (normalize signed)
            mp -> mp

--- cos(p*pi/q) where 0 <= p <= 2q (angle in [0, 2*pi]).
cosInRange :: Int -> Int -> TrigResult
cosInRange p q
  | p == 0             = Radical (Lit rOne)
  | 2 * p == q         = Radical (Lit rZero)
  | p == q             = Radical (Lit (Rational.fromInt (negate 1)))
  | 2 * p == 3 * q     = Radical (Lit rZero)
  | 2 * p > q && p < q =
      case cosInRange (q - p) q of
        Radical e -> Radical (Neg e)
        other     -> other
  | p > q && 2 * p < 3 * q =
      case cosInRange (p - q) q of
        Radical e -> Radical (Neg e)
        other     -> other
  | 2 * p >= 3 * q     = cosInRange (2 * q - p) q
  | otherwise           = cosFirstQuadrant p q

--- cos in first quadrant: compute via cos(2*pi/n) and Chebyshev.
cosFirstQuadrant :: Int -> Int -> TrigResult
cosFirstQuadrant p q =
  let g = gcdInt p (2 * q)
      n = (2 * q) `div` g
      k = p `div` g
  in if k == 1
     then case cosOfUnity n of
            Just e  -> Radical (normalize e)
            Nothing -> MinPoly (cyclotomic n)
     else case cosOfUnity n of
            Just base ->
              let cheb = chebyshev k base
              in Radical (normalize cheb)
            Nothing -> MinPoly (cyclotomic n)

--- Chebyshev polynomial evaluation: T_k(x) computed symbolically.
--- T_0(x) = 1, T_1(x) = x, T_{n+1}(x) = 2x*T_n(x) - T_{n-1}(x)
chebyshev :: Int -> RadExpr Rational -> RadExpr Rational
chebyshev k x
  | k == 0    = Lit rOne
  | k == 1    = x
  | otherwise = go 2 (Lit rOne) x
  where
    go n t0 t1
      | n > k     = t1
      | otherwise  =
          let t2 = Add (Mul (Mul (Lit (Rational.fromInt 2)) x) t1) (Neg t0)
          in go (n + 1) t1 t2

--- GCD.
gcdInt :: Int -> Int -> Int
gcdInt a b
  | b == 0    = a
  | otherwise = gcdInt b (a `mod` b)

--- Absolute value.
absInt :: Int -> Int
absInt x = if x < 0 then negate x else x

instance Show TrigResult where
  show tr = case tr of
    Radical e -> "Radical (" ++ show e ++ ")"
    MinPoly p -> "MinPoly (" ++ show p ++ ")"
