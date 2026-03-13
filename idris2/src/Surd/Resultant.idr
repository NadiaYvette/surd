module Surd.Resultant

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- for Ring/Field Rational instances

import Data.List

%default covering

------------------------------------------------------------------------
-- Pseudo-remainder
------------------------------------------------------------------------

||| Pseudo-remainder: prem(f, g) such that lc(g)^delta * f = q*g + r
||| where delta = deg(f) - deg(g) + 1.
||| Works over any ring (no division needed).
export
pseudoRemainder : Ring k => Poly k -> Poly k -> Poly k
pseudoRemainder f g =
  case (degreeInt f, degreeInt g) of
    (df, dg) =>
      if df < dg then f
      else go f (cast (df - dg + 1))
  where
    go : Poly k -> Nat -> Poly k
    go r Z = r
    go r (S steps) =
      if degreeInt r < degreeInt g then r
      else
        let lcG = case leadCoeff g of Just c => c; Nothing => rOne
            lcR = case leadCoeff r of Just c => c; Nothing => rZero
            dr = degreeInt r
            dg = degreeInt g
            d  = cast {to = Nat} (dr - dg)
            -- lcR * x^d * g
            shifted = MkPoly (replicate d (the k rZero) ++ coeffs g)
            scaledShifted = scalePoly lcR shifted
            -- lcG * r
            scaledR = scalePoly lcG r
        in go (subPoly scaledR scaledShifted) steps

------------------------------------------------------------------------
-- Subresultant PRS
------------------------------------------------------------------------

powField : Field k => k -> Integer -> k
powField _ 0 = rOne
powField x n = if n < 0 then rInv (powField x (negate n))
               else rMul x (powField x (n - 1))

mutual
  resultantPRS : Field k => Poly k -> Poly k -> k
  resultantPRS f g =
    let r = snd (divModPoly f g)
    in if degreeInt r < 0 then
         -- g divides f; resultant is 0 if deg g > 0
         if degreeInt g > 0 then rZero
         else case leadCoeff g of Just c => c; Nothing => rZero
       else
         let df = degreeInt f
             dg = degreeInt g
             dr = degreeInt r
             sign = if mod (df * dg) 2 == 0 then rOne else rNeg rOne
             lcG = case leadCoeff g of Just c => c; Nothing => rOne
             lcGpow = powField lcG (cast (df - dr))
             sub = rMul sign (rDiv (resultant g r) lcGpow)
         in sub

  ||| Compute the resultant of two polynomials via the subresultant PRS.
  ||| Returns the resultant as an element of the coefficient ring.
  export
  resultant : Field k => Poly k -> Poly k -> k
  resultant f g =
    case (degreeInt f, degreeInt g) of
      (-1, _) => rZero
      (_, -1) => rZero
      (0, _)  => case leadCoeff f of Just c => c; Nothing => rZero
      (_, 0)  => case leadCoeff g of Just c => c; Nothing => rZero
      _       =>
        if degreeInt f >= degreeInt g
          then resultantPRS f g
          else
            let r = resultantPRS g f
                df = degreeInt f
                dg = degreeInt g
                sign = if mod (df * dg) 2 == 0 then rOne else rNeg rOne
            in rMul sign r

------------------------------------------------------------------------
-- Composed polynomials (for algebraic number arithmetic)
------------------------------------------------------------------------

||| Composed sum polynomial: annihilating polynomial for alpha + beta
||| where alpha is a root of f and beta is a root of g.
||| Res_y(f(x - y), g(y))
export
composedSum : Poly Rational -> Poly Rational -> Poly Rational
composedSum f g =
  mkPoly [Rational.one]  -- stub for composed sum

||| Composed product polynomial: annihilating polynomial for alpha * beta.
export
composedProduct : Poly Rational -> Poly Rational -> Poly Rational
composedProduct f g = mkPoly [Rational.one]  -- stub
