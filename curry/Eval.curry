--- Numerical evaluation of radical expressions.
---
--- evalDouble evaluates to Float (Curry's floating-point type).
--- evalComplex evaluates to (Float, Float) representing (re, im).
module Eval
  ( evalDouble
  , evalComplex
  , complexNthRoot
  ) where

import Rational ( Rational, numerator, denominator )
import RadExpr

--- Convert a Rational to Float.
ratToFloat :: Rational -> Float
ratToFloat r = fromInt (numerator r) / fromInt (denominator r)

--- Evaluate a radical expression to a Float (Curry's floating-point type).
---
--- Expressions involving even roots of negative numbers will produce NaN.
--- Use evalComplex for expressions with complex intermediates.
evalDouble :: RadExpr Rational -> Float
evalDouble expr = case expr of
  Lit r     -> ratToFloat r
  Neg a     -> negate (evalDouble a)
  Add a b   -> evalDouble a + evalDouble b
  Mul a b   -> evalDouble a * evalDouble b
  Inv a     -> 1.0 / evalDouble a
  Root n a  -> evalDouble a ** (1.0 / fromInt n)
  Pow a n   -> if n >= 0
               then evalDouble a ** fromInt n
               else 1.0 / (evalDouble a ** fromInt (negate n))

--- Evaluate a radical expression to a complex (Float, Float) pair.
---
--- Handles expressions with complex intermediates, such as those
--- arising from the casus irreducibilis in Gauss period descent.
evalComplex :: RadExpr Rational -> (Float, Float)
evalComplex expr = case expr of
  Lit r     -> (ratToFloat r, 0.0)
  Neg a     -> let (re, im) = evalComplex a
               in (negate re, negate im)
  Add a b   -> let (ar, ai) = evalComplex a
                   (br, bi) = evalComplex b
               in (ar + br, ai + bi)
  Mul a b   -> let (ar, ai) = evalComplex a
                   (br, bi) = evalComplex b
               in (ar * br - ai * bi, ar * bi + ai * br)
  Inv a     -> let (re, im) = evalComplex a
                   d = re * re + im * im
               in (re / d, negate im / d)
  Root n a  -> complexNthRoot n (evalComplex a)
  Pow a n   -> if n >= 0
               then complexPow (evalComplex a) n
               else let z = complexPow (evalComplex a) (negate n)
                        (zr, zi) = z
                        d = zr * zr + zi * zi
                    in (zr / d, negate zi / d)

--- Complex power by repeated multiplication.
complexPow :: (Float, Float) -> Int -> (Float, Float)
complexPow z n
  | n == 0    = (1.0, 0.0)
  | n == 1    = z
  | even n    = let half = complexPow z (n `div` 2)
                    (hr, hi) = half
                in (hr * hr - hi * hi, 2.0 * hr * hi)
  | otherwise = let (zr, zi) = z
                    (rr, ri) = complexPow z (n - 1)
                in (zr * rr - zi * ri, zr * ri + zi * rr)

--- atan2 implemented manually since Curry's Prelude lacks it.
floatAtan2 :: Float -> Float -> Float
floatAtan2 y x
  | x > 0.0           = atan (y / x)
  | x < 0.0 && y >= 0.0 = atan (y / x) + pi
  | x < 0.0 && y < 0.0  = atan (y / x) - pi
  | x == 0.0 && y > 0.0 = pi / 2.0
  | x == 0.0 && y < 0.0 = negate (pi / 2.0)
  | otherwise            = 0.0

--- Principal nth root of a complex number.
--- Uses polar form: z = r * e^(i*theta) -> z^(1/n) = r^(1/n) * e^(i*theta/n)
complexNthRoot :: Int -> (Float, Float) -> (Float, Float)
complexNthRoot n (re, im) =
  let r = sqrt (re * re + im * im)
      theta = floatAtan2 im re
      rn = r ** (1.0 / fromInt n)
      an = theta / fromInt n
  in (rn * cos an, rn * sin an)
