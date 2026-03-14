module Surd.Eval

import Surd.Rational
import Surd.Types

%default covering

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

||| Convert a Rational to Double.
ratToDouble : Rational -> Double
ratToDouble r = cast (numer r) / cast (denom r)

||| Raise a Double to an integer power.
powDouble : Double -> Int -> Double
powDouble x n =
  if n == 0 then 1.0
  else if n > 0 then go 1.0 x (cast n)
  else 1.0 / go 1.0 x (cast (negate n))
  where
    go : Double -> Double -> Nat -> Double
    go acc _ Z = acc
    go acc b (S k) = go (acc * b) b k

------------------------------------------------------------------------
-- Real evaluation (Double)
------------------------------------------------------------------------

||| Evaluate a radical expression to a Double.
||| Uses floating-point arithmetic -- fast but inexact.
||| Note: expressions involving even roots of negative numbers (e.g., sqrt(-3))
||| will produce NaN. Use evalComplex for expressions with complex intermediates.
export
eval : RadExpr Rational -> Double
eval (Lit r)    = ratToDouble r
eval (Neg a)    = negate (eval a)
eval (Add a b)  = eval a + eval b
eval (Mul a b)  = eval a * eval b
eval (Inv a)    = 1.0 / eval a
eval (Root n a) = pow (eval a) (1.0 / cast (cast {to = Integer} n))
eval (Pow a n)  = powDouble (eval a) n

------------------------------------------------------------------------
-- Complex evaluation (pairs of Doubles)
------------------------------------------------------------------------

||| A complex number as a pair (real, imaginary).
public export
Complex : Type
Complex = (Double, Double)

||| Complex addition.
cadd : Complex -> Complex -> Complex
cadd (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

||| Complex subtraction.
csub : Complex -> Complex -> Complex
csub (r1, i1) (r2, i2) = (r1 - r2, i1 - i2)

||| Complex multiplication.
cmul : Complex -> Complex -> Complex
cmul (r1, i1) (r2, i2) = (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)

||| Complex negation.
cneg : Complex -> Complex
cneg (r, i) = (negate r, negate i)

||| Complex inverse: 1/(a + bi) = (a - bi)/(a^2 + b^2)
cinv : Complex -> Complex
cinv (r, i) =
  let d = r * r + i * i
  in (r / d, negate i / d)

||| Complex magnitude.
cmag : Complex -> Double
cmag (r, i) = sqrt (r * r + i * i)

||| Complex from real.
cfromReal : Double -> Complex
cfromReal x = (x, 0.0)

||| Complex power to a natural number.
cpowNat : Complex -> Nat -> Complex
cpowNat _ Z = (1.0, 0.0)
cpowNat z (S k) = cmul z (cpowNat z k)

||| atan2 implemented via atan, handling quadrant selection.
atan2' : Double -> Double -> Double
atan2' y x =
  if x > 0.0 then atan (y / x)
  else if x < 0.0 && y >= 0.0 then atan (y / x) + pi
  else if x < 0.0 && y < 0.0 then atan (y / x) - pi
  else if x == 0.0 && y > 0.0 then pi / 2.0
  else if x == 0.0 && y < 0.0 then negate (pi / 2.0)
  else 0.0  -- x == 0, y == 0

||| Principal nth root of a complex number.
||| Uses polar form: z = r*e^(i*theta) -> z^(1/n) = r^(1/n)*e^(i*theta/n)
export
complexNthRoot : Int -> Complex -> Complex
complexNthRoot n (r, i) =
  let mag = sqrt (r * r + i * i)
      theta = atan2' i r
      rn = pow mag (1.0 / cast n)
      an = theta / cast n
  in (rn * cos an, rn * sin an)

||| Evaluate a radical expression to a complex number (pair of Doubles).
|||
||| Handles expressions that pass through complex intermediates,
||| such as those arising from the casus irreducibilis in the
||| Gauss period descent for roots of unity.
export
evalComplex : RadExpr Rational -> Complex
evalComplex (Lit r)    = cfromReal (ratToDouble r)
evalComplex (Neg a)    = cneg (evalComplex a)
evalComplex (Add a b)  = cadd (evalComplex a) (evalComplex b)
evalComplex (Mul a b)  = cmul (evalComplex a) (evalComplex b)
evalComplex (Inv a)    = cinv (evalComplex a)
evalComplex (Root n a) = complexNthRoot (cast n) (evalComplex a)
evalComplex (Pow a n)  =
  if n >= 0 then cpowNat (evalComplex a) (cast n)
  else cinv (cpowNat (evalComplex a) (cast (negate n)))

||| Extract the real part of a complex evaluation.
export
evalReal : RadExpr Rational -> Double
evalReal e = fst (evalComplex e)

||| Extract the imaginary part of a complex evaluation.
export
evalImag : RadExpr Rational -> Double
evalImag e = snd (evalComplex e)
