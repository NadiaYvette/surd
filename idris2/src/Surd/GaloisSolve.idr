||| Top-level polynomial solving interface via Galois group computation.
|||
||| Pipeline:
|||   1. Check degree (degree <= 4 returns Unsupported for caller to use
|||      Cardano/Ferrari)
|||   2. Identify the Galois group
|||   3. If solvable, construct radical expressions via tower
|||   4. Return the list of radical roots
|||
||| Supports all prime degrees >= 5 via the generalized Lagrange
||| resolvent tower.
module Surd.GaloisSolve

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Eval
import Surd.AlgNum
import Surd.Identify
import Surd.RadicalTower
import Surd.TransitiveGroup
import Surd.PrimeFactors

import Data.List

%default covering

------------------------------------------------------------------------
-- Top-level solving interface
------------------------------------------------------------------------

||| Degree 5 fast path: uses the Fin-indexed TransGroup 5.
solveDeg5 : Poly Rational -> SolveResult
solveDeg5 p =
  case identifyGaloisGroup5 p of
    Identified5 tg => solveViaTower p tg
    Unidentified msg => Unsupported msg
    _ => Unsupported "Unexpected identification result for degree 5"

||| General prime degree: uses runtime TransGroupRT.
solveGeneralPrime : Poly Rational -> SolveResult
solveGeneralPrime p =
  case identifyGaloisGroup p of
    IdentifiedRT tg =>
      if tgSolvable tg
        then solveViaTowerRT p tg
        else NotSolvable
    Unidentified msg => Unsupported msg
    _ => Unsupported "Unexpected identification result"

||| Solve a polynomial by radicals, if possible.
|||
||| For degree <= 4, returns Unsupported (caller should use direct formulas).
||| For prime degree >= 5, identifies the Galois group and solves if solvable.
||| For composite degree > 4 (non-prime), returns Unsupported.
export
solveByRadicals : Poly Rational -> SolveResult
solveByRadicals p =
  let d = degreeInt p
  in if d <= 4
       then Unsupported "Degree <= 4: use direct formulas"
       else if d == 5
       then solveDeg5 p
       else if d > 0 && isPrime d
       then solveGeneralPrime p
       else Unsupported ("Degree " ++ show d ++ " not supported (composite or non-positive)")

------------------------------------------------------------------------
-- AlgNum interface
------------------------------------------------------------------------

||| Solve for a specific algebraic number.
|||
||| Given an algebraic number (minpoly + isolating interval),
||| find the radical expression for the root closest to the
||| target numerical value.
export
solveAlgNum : AlgNum -> Maybe (RadExpr Rational)
solveAlgNum a =
  let d = degreeInt (anMinPoly a)
  in if d <= 4 then Nothing  -- caller should use Cardano/Ferrari
     else
       case solveByRadicals (anMinPoly a) of
         Solved roots =>
           let target = algApprox a
           in pickClosest roots target
         _ => Nothing
  where
    pickClosest : List (RadExpr Rational) -> Double -> Maybe (RadExpr Rational)
    pickClosest [] _ = Nothing
    pickClosest roots target =
      let withDist = map (\r => (abs (eval r - target), r)) roots
          sorted = sortBy (\a, b => compare (fst a) (fst b)) withDist
      in case sorted of
           ((_, r) :: _) => Just r
           [] => Nothing

------------------------------------------------------------------------
-- Identify-and-solve convenience function
------------------------------------------------------------------------

||| Identify the Galois group and solve if solvable.
||| Returns (groupName, roots) on success.
export
identifyAndSolve : Poly Rational -> Maybe (String, List (RadExpr Rational))
identifyAndSolve p =
  let d = degreeInt p
  in if d <= 4 then Nothing
     else
       let galoisResult = identifyGaloisGroup p
       in if isSolvableResult galoisResult
          then
            case solveByRadicals p of
              Solved roots => Just (resultGroupName galoisResult, roots)
              _ => Nothing
          else Nothing
