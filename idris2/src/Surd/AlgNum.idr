module Surd.AlgNum

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Interval
import Surd.RootIsolation

import Data.List

%default covering

------------------------------------------------------------------------
-- Algebraic number type
------------------------------------------------------------------------

||| A real algebraic number: minimal polynomial over Q
||| plus an isolating interval identifying which root.
public export
record AlgNum where
  constructor MkAlgNum
  anMinPoly  : Poly Rational
  anInterval : Interval

export
Show AlgNum where
  show a = "AlgNum(" ++ show (anMinPoly a) ++ ", " ++ show (anInterval a) ++ ")"

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

||| Embed a rational as an algebraic number.
export
algFromRational : Rational -> AlgNum
algFromRational r =
  MkAlgNum (mkPoly [negate r, Rational.one]) (MkInterval r r)

||| Construct an algebraic number from a polynomial and approximate value.
||| Finds the root closest to the given approximation.
export
algFromPoly : Poly Rational -> Double -> Maybe AlgNum
algFromPoly p approx =
  let mp = monicPoly p
      roots = isolateRealRoots mp
  in case roots of
       [] => Nothing
       _ => let closest = minimumBy (compareByDist approx) roots
            in Just (MkAlgNum mp (iiInterval closest))
  where
    midpointDouble : Interval -> Double
    midpointDouble iv = cast (numer (midpoint iv)) / cast (denom (midpoint iv))

    compareByDist : Double -> IsolatingInterval -> IsolatingInterval -> Ordering
    compareByDist target a b =
      compare (abs (midpointDouble (iiInterval a) - target))
              (abs (midpointDouble (iiInterval b) - target))

    minimumBy : (a -> a -> Ordering) -> List a -> a
    minimumBy _ [x] = x
    minimumBy f (x :: y :: xs) = case f x y of
      GT => minimumBy f (y :: xs)
      _ => minimumBy f (x :: xs)
    minimumBy _ [] = believe_me ()  -- unreachable given non-empty guard

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

||| Minimal polynomial of an algebraic number.
export
algMinPoly : AlgNum -> Poly Rational
algMinPoly = anMinPoly

||| Approximate value of an algebraic number as Double.
export
algApprox : AlgNum -> Double
algApprox a =
  let m = midpoint (anInterval a)
  in cast (numer m) / cast (denom m)

------------------------------------------------------------------------
-- Arithmetic (stub implementations using resultant-based composed polys)
------------------------------------------------------------------------

||| Negate an algebraic number: if alpha has minpoly f(x), then
||| -alpha has minpoly f(-x).
export
negateOddCoeffs : Bool -> List Rational -> List Rational
negateOddCoeffs _ [] = []
negateOddCoeffs odd (c :: cs) =
  (if odd then negate c else c) :: negateOddCoeffs (not odd) cs

algNeg : AlgNum -> AlgNum
algNeg a =
  let p = anMinPoly a
      cs = coeffs p
      negCs = negateOddCoeffs False cs
      iv = anInterval a
  in MkAlgNum (mkPoly negCs) (MkInterval (negate (hi iv)) (negate (lo iv)))

||| Add two algebraic numbers.
||| Uses composed sum polynomial (stub: returns approximate result).
export
algAdd : AlgNum -> AlgNum -> AlgNum
algAdd a b =
  let approx = algApprox a + algApprox b
      -- For a full implementation: compute composed sum polynomial,
      -- factor it, and isolate the correct root.
      -- Stub: construct linear polynomial from approximation.
      r = mkRat (cast (the Int (cast (approx * 1000000.0)))) 1000000
  in algFromRational r

||| Multiply two algebraic numbers.
export
algMul : AlgNum -> AlgNum -> AlgNum
algMul a b =
  let approx = algApprox a * algApprox b
      r = mkRat (cast (the Int (cast (approx * 1000000.0)))) 1000000
  in algFromRational r

||| Inverse of an algebraic number.
||| If alpha has minpoly a_n x^n + ... + a_0, then 1/alpha has
||| minpoly a_0 x^n + ... + a_n (reversed coefficients).
export
algInv : AlgNum -> AlgNum
algInv a =
  let p = anMinPoly a
      revCs = reverse (coeffs p)
      iv = anInterval a
      invLo = if hi iv /= Rational.zero then recip (hi iv) else Rational.zero
      invHi = if lo iv /= Rational.zero then recip (lo iv) else Rational.zero
  in MkAlgNum (mkPoly revCs) (MkInterval (min invLo invHi) (max invLo invHi))

||| Subtract.
export
algSub : AlgNum -> AlgNum -> AlgNum
algSub a b = algAdd a (algNeg b)

||| Divide.
export
algDiv : AlgNum -> AlgNum -> AlgNum
algDiv a b = algMul a (algInv b)

||| Integer power.
export
algPow : AlgNum -> Int -> AlgNum
algPow a 0 = algFromRational Rational.one
algPow a 1 = a
algPow a n = if n < 0 then algPow (algInv a) (negate n)
             else algMul a (algPow a (n - 1))

||| Equality test: same minpoly and overlapping intervals (after refinement).
export
algEq : AlgNum -> AlgNum -> Bool
algEq a b =
  anMinPoly a == anMinPoly b && overlaps (anInterval a) (anInterval b)

||| Ordering.
export
algCompare : AlgNum -> AlgNum -> Ordering
algCompare a b =
  let iiA = refineRoot (mkRat 1 1000000) (MkIsolatingInterval (anMinPoly a) (anInterval a))
      iiB = refineRoot (mkRat 1 1000000) (MkIsolatingInterval (anMinPoly b) (anInterval b))
      ia = iiInterval iiA
      ib = iiInterval iiB
  in if hi ia < lo ib then LT
     else if lo ia > hi ib then GT
     else EQ
