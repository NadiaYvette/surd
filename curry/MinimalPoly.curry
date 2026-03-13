--- Compute the minimal polynomial of a radical expression over Q.
---
--- The basic approach: given a radical expression alpha, build a
--- polynomial that alpha satisfies (an annihilating polynomial),
--- then factor it to find the irreducible factor that alpha is
--- actually a root of (verified by numerical evaluation).
module MinimalPoly
  ( minimalPoly
  , annihilatingPoly
  ) where

import Rational
import Poly
import RadExpr
import Eval (evalDouble)
import Factoring (factorSquareFree)
import Resultant (composedSum, composedProduct, negateVar, reciprocalPoly,
                  shiftPoly, lagrangeInterpolate)
import RootBound (pickClosest)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Compute the minimal polynomial of a radical expression over Q.
---
--- This is the monic irreducible polynomial in Q[x] of smallest degree
--- that the expression satisfies.
minimalPoly :: RadExpr Rational -> Poly
minimalPoly expr =
  let ann = annihilatingPoly expr
      v   = evalDouble expr
      factors = factorSquareFree ann
  in case factors of
       []    -> ann
       [f]   -> monicPoly f
       _     -> monicPoly (pickClosest v factors)

--- Compute an annihilating polynomial (not necessarily minimal) for
--- a radical expression. The expression is guaranteed to be a root
--- of this polynomial, but it may be reducible.
annihilatingPoly :: RadExpr Rational -> Poly
annihilatingPoly expr = case expr of
  Lit r ->
    mkPoly [ratNeg r, rOne]
  Neg e ->
    negateVar (annihilatingPoly e)
  Add a b ->
    let pa = annihilatingPoly a
        pb = annihilatingPoly b
    in composedSum pa pb
  Mul a b ->
    let pa = annihilatingPoly a
        pb = annihilatingPoly b
    in composedProduct pa pb
  Inv e ->
    let p = annihilatingPoly e
    in monicPoly (reciprocalPoly p)
  Root n e ->
    -- If p(x) annihilates e, then p(x^n) annihilates e^(1/n)
    let p = annihilatingPoly e
    in substituteXN n p
  Pow e n ->
    if n >= 0
    then annihilatingPoly (powToMuls e n)
    else annihilatingPoly (Inv (powToMuls e (negate n)))

--- Substitute x -> x^n in a polynomial.
substituteXN :: Int -> Poly -> Poly
substituteXN n (Poly cs) =
  mkPoly (concatMap (\c -> c : replicate (n - 1) rZero) cs)

--- Convert Pow to repeated Mul (for annihilating polynomial computation).
powToMuls :: RadExpr Rational -> Int -> RadExpr Rational
powToMuls e n
  | n == 0    = Lit rOne
  | n == 1    = e
  | otherwise = Mul e (powToMuls e (n - 1))
