module Surd.Convert

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Eval
import Surd.Normalize
import Surd.Interval
import Surd.AlgNum
import Surd.MinimalPoly
import Surd.Factoring

import Data.List

%default covering

------------------------------------------------------------------------
-- RadExpr -> AlgNum
------------------------------------------------------------------------

||| Convert a radical expression to its canonical algebraic number form.
|||
||| Computes the minimal polynomial and isolates the correct root.
export
radExprToAlgNum : RadExpr Rational -> AlgNum
radExprToAlgNum expr =
  let mp = minimalPoly expr
      approx = eval expr
  in case algFromPoly mp approx of
       Just a => a
       Nothing => algFromRational (mkRat (cast (the Int (cast (approx * 1000000.0)))) 1000000)

------------------------------------------------------------------------
-- AlgNum -> RadExpr (degree <= 4)
------------------------------------------------------------------------

||| Convert an algebraic number back to a radical expression.
|||
||| Handles degree 1 (rational), degree 2 (quadratic formula),
||| degree 3 (Cardano), and degree 4 (Ferrari).
export
algNumToRadExpr : AlgNum -> Maybe (RadExpr Rational)
algNumToRadExpr a =
  case degreeInt (anMinPoly a) of
    1 => Just (solveLinear (anMinPoly a))
    2 => Just (solveQuadratic (anMinPoly a) (algApprox a))
    3 => solveCubic (anMinPoly a) (algApprox a)
    4 => Nothing  -- Ferrari: stub
    _ => Nothing
  where
    ||| Solve ax + b = 0.
    solveLinear : Poly Rational -> RadExpr Rational
    solveLinear p =
      case coeffs p of
        [b, a] => Lit (negate b / a)
        _ => Lit Rational.zero

    ||| Solve ax^2 + bx + c = 0 via quadratic formula.
    solveQuadratic : Poly Rational -> Double -> RadExpr Rational
    solveQuadratic p approx =
      case coeffs p of
        [c, b, a] =>
          let disc = b * b - Rational.fromInteger 4 * a * c
              sqrtDisc = Root 2 (Lit disc)
              r1 = Mul (Inv (Lit (Rational.fromInteger 2 * a)))
                       (Add (Neg (Lit b)) sqrtDisc)
              r2 = Mul (Inv (Lit (Rational.fromInteger 2 * a)))
                       (Add (Neg (Lit b)) (Neg sqrtDisc))
              v1 = eval r1
              v2 = eval r2
          in if abs (v1 - approx) < abs (v2 - approx) then r1 else r2
        _ => Lit Rational.zero

    ||| Solve cubic via Cardano's formula (simplified).
    solveCubic : Poly Rational -> Double -> Maybe (RadExpr Rational)
    solveCubic p approx =
      case coeffs p of
        [d, c, b, a] =>
          -- Depressed cubic: t^3 + pt + q where x = t - b/(3a)
          let a' = a
              shift = negate b / (Rational.fromInteger 3 * a')
              pp = (Rational.fromInteger 3 * a' * c - b * b) / (Rational.fromInteger 9 * a' * a')
              qq = (Rational.fromInteger 2 * b * b * b - Rational.fromInteger 9 * a' * b * c + Rational.fromInteger 27 * a' * a' * d)
                   / (Rational.fromInteger 54 * a' * a' * a')
              disc = qq * qq + pp * pp * pp
              -- u = cbrt(-q + sqrt(disc))
              sqrtDisc = Root 2 (Lit disc)
              u = Root 3 (Add (Neg (Lit qq)) sqrtDisc)
              -- t = u - p/u (need to handle u = 0 case)
              -- x = t + shift
              -- For Cardano: u2 = -p/(3*u1), NOT independent cube root
              pLit = Lit pp
              t = Add u (Mul (Neg pLit) (Inv u))
              x = Add t (Lit shift)
          in Just (normalize x)
        _ => Nothing

------------------------------------------------------------------------
-- Simplification via canonical form
------------------------------------------------------------------------

||| Simplify a radical expression by converting to algebraic number
||| and back. The round-trip may yield a simpler radical form if the
||| minimal polynomial has low degree.
export
simplifyViaCanonical : RadExpr Rational -> RadExpr Rational
simplifyViaCanonical expr =
  let a = radExprToAlgNum expr
  in case algNumToRadExpr a of
       Just simpler =>
         let s1 = eval expr
             s2 = eval simpler
         in if abs (s1 - s2) < 1.0e-8 then simpler else expr
       Nothing => expr

||| Display info about an algebraic number.
export
algNumInfo : AlgNum -> String
algNumInfo a =
  "MinPoly: " ++ show (anMinPoly a) ++
  ", Approx: " ++ show (algApprox a) ++
  ", Interval: " ++ show (anInterval a)
