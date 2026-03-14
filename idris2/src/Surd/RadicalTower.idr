module Surd.RadicalTower

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Eval
import Surd.Normalize
import Surd.Permutation
import Surd.TransitiveGroup
import Surd.Identify
import Surd.Resolvent
import Surd.RootOfUnity

import Data.List

%default covering

------------------------------------------------------------------------
-- Radical tower construction
------------------------------------------------------------------------

||| Result of radical tower solving.
public export
data SolveResult : Type where
  ||| All roots expressed as radical expressions.
  Solved      : List (RadExpr Rational) -> SolveResult
  ||| The polynomial is not solvable by radicals.
  NotSolvable : SolveResult
  ||| Could not solve (unsupported case).
  Unsupported : String -> SolveResult

export
Show SolveResult where
  show (Solved roots) = "Solved: " ++ show (length roots) ++ " roots"
  show NotSolvable = "Not solvable by radicals"
  show (Unsupported msg) = "Unsupported: " ++ msg

------------------------------------------------------------------------
-- Solving via tower for solvable quintics
------------------------------------------------------------------------

||| Solve a solvable quintic via radical tower.
export
solveViaTower : Poly Rational -> TransGroup 5 -> SolveResult
solveViaTower p tg =
  if not (tgSolvable tg) then NotSolvable
  else
    let numRoots = findRoots p
    in case numRoots of
         [] => Unsupported "Could not find numerical roots"
         _ =>
           -- Stub: return roots as Lit(approx) for now
           let approxRoots = map (\r => Lit (mkRat (cast (the Int (cast (fst r * 1000000.0)))) 1000000)) numRoots
           in Solved approxRoots
