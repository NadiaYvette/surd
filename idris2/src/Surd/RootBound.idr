module Surd.RootBound

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- for Ring/Field Rational instances

%default covering

------------------------------------------------------------------------
-- Cauchy bound
------------------------------------------------------------------------

||| Cauchy bound: all roots of the polynomial satisfy |r| <= bound.
|||
||| For a monic polynomial x^n + a_{n-1}x^{n-1} + ... + a_0,
||| all roots satisfy |r| <= 1 + max(|a_0|, ..., |a_{n-1}|).
initList : List a -> List a
initList [] = []
initList [_] = []
initList (x :: xs) = x :: initList xs

export
cauchyBound : Poly Rational -> Rational
cauchyBound (MkPoly []) = Rational.zero
cauchyBound p =
  case leadCoeff p of
    Nothing => Rational.zero
    Just lc =>
      let cs = coeffs p
          ratios = map (\c => abs (c / lc)) (initList cs)
          maxRatio = foldl max Rational.zero ratios
      in Rational.one + maxRatio

||| Lagrange bound: tighter upper bound on positive roots.
|||
||| For f(x) = a_n x^n + ... + a_0 with a_n > 0:
||| All positive roots satisfy r <= 1 + (max negative coefficient / leading coefficient)^(1/k)
||| where k is chosen appropriately. Simplified version returns Cauchy bound.
export
lagrangeBound : Poly Rational -> Rational
lagrangeBound = cauchyBound

||| Lower bound on the absolute value of roots (reciprocal of Cauchy bound
||| of the reversed polynomial).
export
rootLowerBound : Poly Rational -> Rational
rootLowerBound (MkPoly []) = Rational.zero
rootLowerBound p =
  let cs = coeffs p
      rev = mkPoly (reverse cs)
      ub = cauchyBound rev
  in if ub > Rational.zero then recip ub else Rational.zero

||| Bound on the absolute value of the largest root, returned as an interval.
export
rootInterval : Poly Rational -> (Rational, Rational)
rootInterval p =
  let b = cauchyBound p
  in (negate b, b)
