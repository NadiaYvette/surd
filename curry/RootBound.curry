--- Cauchy root bound for polynomials with rational coefficients.
---
--- Provides an upper bound on the absolute value of all real and
--- complex roots of a polynomial.
module RootBound
  ( rootBound
  , pickClosest
  ) where

import Rational
import Poly
import Eval (evalDouble)
import RadExpr (RadExpr)

--- Local alias.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Cauchy bound: all roots r of p satisfy |r| <= rootBound p.
---
--- For p(x) = a_n x^n + ... + a_0,
--- bound = 1 + max(|a_0/a_n|, ..., |a_{n-1}/a_n|)
rootBound :: Poly -> Rational
rootBound (Poly cs) = case cs of
  [] -> rZero
  _  ->
    let lc = lastElem cs
        ratios = map (\c -> ratAbs (ratDiv c lc)) (initList cs)
        m = foldl ratMax rZero ratios
    in ratAdd rOne m

--- Last element of a non-empty list.
lastElem :: [a] -> a
lastElem xs = case xs of
  [x]    -> x
  (_:ys) -> lastElem ys
  []     -> error "lastElem: empty"

--- All but the last element.
initList :: [a] -> [a]
initList xs = case xs of
  []     -> []
  [_]    -> []
  (y:ys) -> y : initList ys

--- Pick the polynomial factor whose root is closest to a given
--- approximate value.
pickClosest :: Float -> [Poly] -> Poly
pickClosest approx ps = case ps of
  []  -> error "pickClosest: empty list"
  [p] -> p
  _   ->
    let scored = map (\p -> (p, closestRootDist p approx)) ps
    in fst (foldl1 (\(p1, d1) (p2, d2) ->
              if d1 <= d2 then (p1, d1) else (p2, d2)) scored)

--- Approximate distance from the closest root of p to the given value.
--- Evaluates |p(approx)| / |p'(approx)| as a crude proximity measure.
closestRootDist :: Poly -> Float -> Float
closestRootDist p v =
  let pv  = evalPolyFloat p v
      p'  = diffPoly p
      dpv = evalPolyFloat p' v
  in if abs dpv < 1.0e-15
     then abs pv
     else abs (pv / dpv)

--- Evaluate polynomial at a Float point.
evalPolyFloat :: Poly -> Float -> Float
evalPolyFloat (Poly cs) x = case cs of
  [] -> 0.0
  _  -> foldr (\c acc -> ratToFloat c + x * acc) 0.0 cs

--- Convert Rational to Float.
ratToFloat :: Rational -> Float
ratToFloat r = Prelude.fromInt (numerator r) / Prelude.fromInt (denominator r)
