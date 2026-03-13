--- Conversion between radical expressions and algebraic numbers.
---
--- Forward direction (RadExpr -> AlgNum): compute the minimal polynomial
--- of a radical expression and isolate the correct root.
---
--- Backward direction (AlgNum -> RadExpr): given a simplified algebraic
--- number (low-degree minimal polynomial), express it as a radical.
module Convert
  ( radExprToAlgNum
  , algNumToRadExpr
  , simplifyViaCanonical
  ) where

import Rational
import Poly
import RadExpr
import Interval (Interval(..))
import Eval (evalDouble)
import AlgNum (AlgNum(..), algFromRational, algFromPoly, algMinPoly, algApprox)
import MinimalPoly (minimalPoly)
import Factoring (rationalRoots)
import Normalize (normalize)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Convert a radical expression to its canonical algebraic number form.
radExprToAlgNum :: RadExpr Rational -> AlgNum
radExprToAlgNum expr =
  let mp = minimalPoly expr
      approx = evalDouble expr
  in case algFromPoly mp approx of
       Just a  -> a
       Nothing -> AlgNum mp (Interval.IV (mkRat (negate 1000) 1)
                                         (mkRat 1000 1))

--- Convert an algebraic number back to a radical expression,
--- if the degree is small enough for closed-form solutions.
algNumToRadExpr :: AlgNum -> Maybe (RadExpr Rational)
algNumToRadExpr a =
  let p = algMinPoly a
      d = degree p
  in if d == 1 then algNumDeg1 p
     else if d == 2 then algNumDeg2 p (algApprox a)
     else if d == 3 then algNumDeg3 p (algApprox a)
     else if d == 4 then algNumDeg4 p (algApprox a)
     else Nothing

--- Degree 1: linear polynomial x - r, root is r.
algNumDeg1 :: Poly -> Maybe (RadExpr Rational)
algNumDeg1 (Poly cs) = case cs of
  [a, b] -> Just (Lit (ratNeg (ratDiv a b)))
  _      -> Nothing

--- Degree 2: quadratic formula.
algNumDeg2 :: Poly -> Float -> Maybe (RadExpr Rational)
algNumDeg2 (Poly cs) approx = case cs of
  [c, b, a] ->
    let disc = ratSub (ratMul b b) (ratMul (Rational.fromInt 4)
                                           (ratMul a c))
    in if ratLt disc rZero
       then Nothing
       else let sqrtDisc = Root 2 (Lit disc)
                twoA = Mul (Lit (Rational.fromInt 2)) (Lit a)
                r1 = divE (Add (Neg (Lit b)) sqrtDisc) twoA
                r2 = divE (subE (Neg (Lit b)) sqrtDisc) twoA
                nr1 = normalize r1
                nr2 = normalize r2
            in if abs (evalDouble nr1 - approx) <
                  abs (evalDouble nr2 - approx)
               then Just nr1
               else Just nr2
  _ -> Nothing

--- Degree 3: Cardano's formula (simplified stub).
algNumDeg3 :: Poly -> Float -> Maybe (RadExpr Rational)
algNumDeg3 (Poly cs) approx = case cs of
  [d, c, b, a] ->
    -- Depressed cubic: t^3 + pt + q where x = t - b/(3a)
    let shift = ratDiv b (ratMul (Rational.fromInt 3) a)
        p = ratSub (ratDiv c a)
                   (ratDiv (ratMul b b)
                           (ratMul (Rational.fromInt 3) (ratMul a a)))
        q = ratAdd (ratDiv d a)
                   (ratSub (ratDiv (ratMul (ratMul b b) b)
                                   (ratMul (Rational.fromInt 27)
                                           (ratMul (ratMul a a) a)))
                           (ratDiv (ratMul b c)
                                   (ratMul (Rational.fromInt 6)
                                           (ratMul a a))))
        -- Cardano: disc = -4p^3 - 27q^2
        disc = ratSub (ratNeg (ratMul (Rational.fromInt 4)
                                      (ratMul (ratMul p p) p)))
                      (ratMul (Rational.fromInt 27) (ratMul q q))
    in if ratGe disc rZero
       then -- One real root (or triple root)
         let halfQ = ratDiv q (Rational.fromInt 2)
             sqrtTerm = Root 2 (Lit (ratDiv (ratNeg disc)
                                            (Rational.fromInt 108)))
             u = Root 3 (Add (Lit (ratNeg halfQ)) sqrtTerm)
             -- t = u - p/(3u)
             t = Add u (Neg (divE (Lit p) (Mul (Lit (Rational.fromInt 3)) u)))
             x = Add t (Neg (Lit shift))
         in Just (normalize x)
       else -- Casus irreducibilis: all three roots real but need complex cube roots
         Nothing
  _ -> Nothing

--- Degree 4: Ferrari's method (stub - returns Nothing for now).
algNumDeg4 :: Poly -> Float -> Maybe (RadExpr Rational)
algNumDeg4 _ _ = Nothing

--- Simplify a radical expression via its algebraic number form.
--- Computes the minimal polynomial, converts back to radicals if
--- the degree is small enough.
simplifyViaCanonical :: RadExpr Rational -> RadExpr Rational
simplifyViaCanonical expr =
  let a = radExprToAlgNum expr
  in case algNumToRadExpr a of
       Just simplified -> simplified
       Nothing         -> expr
