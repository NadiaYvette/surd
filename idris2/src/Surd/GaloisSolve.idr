module Surd.GaloisSolve

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Eval
import Surd.AlgNum
import Surd.Identify
import Surd.RadicalTower

import Data.List

%default covering

------------------------------------------------------------------------
-- Top-level solving interface
------------------------------------------------------------------------

||| Solve a polynomial by radicals, if possible.
|||
||| Pipeline:
||| 1. Check degree (degree <= 4 returns Nothing for caller to use
|||    Cardano/Ferrari)
||| 2. Identify the Galois group
||| 3. If solvable, construct radical expressions via tower
||| 4. Return the list of radical roots
export
solveByRadicals : Poly Rational -> SolveResult
solveByRadicals p =
  case degreeInt p of
    d =>
      if d <= 4 then Unsupported "Degree <= 4: use direct formulas"
      else if d == 5 then
        case identifyGaloisGroup5 p of
          Identified5 tg => solveViaTower p tg
          Unidentified msg => Unsupported msg
          _ => Unsupported "Unexpected identification result"
      else Unsupported ("Degree " ++ show d ++ " not supported")

||| Solve for a specific algebraic number.
|||
||| Given an algebraic number (minpoly + isolating interval),
||| find the radical expression for the root closest to the
||| target numerical value.
export
solveAlgNum : AlgNum -> Maybe (RadExpr Rational)
solveAlgNum a =
  case degreeInt (anMinPoly a) of
    d =>
      if d <= 4 then Nothing  -- caller should use Cardano/Ferrari
      else if d == 5 then
        case solveByRadicals (anMinPoly a) of
          Solved roots =>
            let target = algApprox a
            in pickClosest roots target
          _ => Nothing
      else Nothing
  where
    pickClosest : List (RadExpr Rational) -> Double -> Maybe (RadExpr Rational)
    pickClosest [] _ = Nothing
    pickClosest roots target =
      let withDist = map (\r => (abs (eval r - target), r)) roots
          sorted = sortBy (\a, b => compare (fst a) (fst b)) withDist
      in case sorted of
           ((_, r) :: _) => Just r
           [] => Nothing
