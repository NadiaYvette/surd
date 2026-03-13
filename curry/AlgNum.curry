--- Canonical representation of real algebraic numbers.
---
--- An algebraic number is represented as a pair (minimal polynomial,
--- isolating interval).
module AlgNum
  ( AlgNum(..)
  , algFromRational
  , algFromPoly
  , algMinPoly
  , algApprox
  , algAdd
  , algNeg
  , algSub
  , algMul
  , algInv
  , algEq
  , algCompare
  , showAlgNum
  ) where

import Rational
import Poly
import Interval (Interval(..))
import RootIsolation (IsolatingInterval(..), isolateRealRoots, refineRoot,
                      rootInInterval)
import Resultant (composedSum, composedProduct, negateVar, reciprocalPoly)
import Factoring (factorSquareFree)
import RootBound (pickClosest)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- A real algebraic number: its minimal polynomial over Q and an
--- isolating interval pinpointing which root it is.
data AlgNum = AlgNum Poly Interval

--- Embed a rational number as an algebraic number.
algFromRational :: Rational -> AlgNum
algFromRational r =
  AlgNum (mkPoly [ratNeg r, rOne]) (IV r r)

--- Construct an algebraic number from a polynomial and an approximate
--- real value (used to pick the right root).
algFromPoly :: Poly -> Float -> Maybe AlgNum
algFromPoly p approx =
  let mp = monicPoly p
      roots = isolateRealRoots mp
  in case roots of
    [] -> Nothing
    _  -> Just (pickRoot roots approx)

--- Pick the root whose interval is closest to the approximate value.
pickRoot :: [IsolatingInterval] -> Float -> AlgNum
pickRoot iis approx =
  let scored = map (\ii -> (ii, distFromApprox ii approx)) iis
      best = fst (foldl1 (\(i1, d1) (i2, d2) ->
               if d1 <= d2 then (i1, d1) else (i2, d2)) scored)
      (IsolatingInterval p iv) = best
  in AlgNum p iv

distFromApprox :: IsolatingInterval -> Float -> Float
distFromApprox (IsolatingInterval _ (IV l h)) approx =
  let mid = ratToFloat (ratDiv (ratAdd l h) (Rational.fromInt 2))
  in abs (mid - approx)

ratToFloat :: Rational -> Float
ratToFloat r = Prelude.fromInt (numerator r) / Prelude.fromInt (denominator r)

--- Extract the minimal polynomial.
algMinPoly :: AlgNum -> Poly
algMinPoly (AlgNum p _) = p

--- Approximate value as a Float.
algApprox :: AlgNum -> Float
algApprox (AlgNum _ (IV l h)) =
  ratToFloat (ratDiv (ratAdd l h) (Rational.fromInt 2))

--- Addition of algebraic numbers.
algAdd :: AlgNum -> AlgNum -> AlgNum
algAdd (AlgNum pa iva) (AlgNum pb ivb) =
  let cs = composedSum pa pb
      factors = factorSquareFree cs
      approxA = algApprox (AlgNum pa iva)
      approxB = algApprox (AlgNum pb ivb)
      approxSum = approxA + approxB
      mp = pickClosest approxSum factors
  in case algFromPoly mp approxSum of
       Just a  -> a
       Nothing -> AlgNum mp (IV (Rational.fromInt (negate 1000))
                                (Rational.fromInt 1000))

--- Negation.
algNeg :: AlgNum -> AlgNum
algNeg (AlgNum p (IV l h)) =
  AlgNum (negateVar p) (IV (ratNeg h) (ratNeg l))

--- Subtraction.
algSub :: AlgNum -> AlgNum -> AlgNum
algSub a b = algAdd a (algNeg b)

--- Multiplication.
algMul :: AlgNum -> AlgNum -> AlgNum
algMul (AlgNum pa iva) (AlgNum pb ivb) =
  let cp = composedProduct pa pb
      factors = factorSquareFree cp
      approxA = algApprox (AlgNum pa iva)
      approxB = algApprox (AlgNum pb ivb)
      approxProd = approxA * approxB
      mp = pickClosest approxProd factors
  in case algFromPoly mp approxProd of
       Just a  -> a
       Nothing -> AlgNum mp (IV (Rational.fromInt (negate 1000))
                                (Rational.fromInt 1000))

--- Multiplicative inverse.
algInv :: AlgNum -> AlgNum
algInv (AlgNum p iv) =
  let rp = reciprocalPoly p
      approx = 1.0 / algApprox (AlgNum p iv)
  in case algFromPoly (monicPoly rp) approx of
       Just a  -> a
       Nothing -> AlgNum (monicPoly rp) (IV (Rational.fromInt (negate 1000))
                                            (Rational.fromInt 1000))

--- Equality: same minimal polynomial and overlapping intervals.
algEq :: AlgNum -> AlgNum -> Bool
algEq a b =
  let pa = algMinPoly a
      pb = algMinPoly b
  in pa == pb && intervalsOverlap (algInterval a) (algInterval b)

algInterval :: AlgNum -> Interval
algInterval (AlgNum _ iv) = iv

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (IV l1 h1) (IV l2 h2) =
  ratLe l1 h2 && ratLe l2 h1

--- Comparison (by approximate value, with refinement if needed).
algCompare :: AlgNum -> AlgNum -> Ordering
algCompare a b =
  if algEq a b then EQ
  else compare (algApprox a) (algApprox b)

--- Show.
showAlgNum :: AlgNum -> String
showAlgNum (AlgNum p iv) =
  "AlgNum (" ++ show p ++ ") " ++ show iv

instance Eq AlgNum where
  (==) = algEq

instance Show AlgNum where
  show = showAlgNum
