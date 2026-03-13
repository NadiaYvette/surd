/-
  Surd.Galois.Solve — Top-level solver composing Galois group identification
  and radical tower solving for degree-5 polynomials.
-/
import Surd.Poly.Univariate
import Surd.Radical.Expr
import Surd.Radical.DAG
import Surd.Algebraic.Number
import Surd.Galois.Identify
import Surd.Galois.RadicalTower
import Surd.Galois.TransitiveGroup
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

private def evalDist (e : RadExpr Rat) (target : Float) : Float :=
  let v := dagEvalComplex (RadDAG.toDAG e)
  if Float.abs v.im < 1e-6
  then Float.abs (v.re - target)
  else Float.sqrt ((v.re - target) * (v.re - target) + v.im * v.im)

private def pickClosestReal (exprs : List (RadExpr Rat)) (target : Float)
    : Option (RadExpr Rat) :=
  let scored := exprs.map fun e => (e, evalDist e target)
  let valid := scored.filter fun (_, d) => d < 1e-4
  match valid with
  | [] =>
    match scored.mergeSort (fun a b => a.2 ≤ b.2) with
    | (best, d) :: _ => if d < 1.0 then some best else none
    | _ => none
  | _ =>
    match valid.mergeSort (fun a b => a.2 ≤ b.2) with
    | (best, _) :: _ => some best
    | _ => none

-- ---------------------------------------------------------------------------
-- Public API (ordered so that callees precede callers)
-- ---------------------------------------------------------------------------

/-- Identify the Galois group and solve if solvable. -/
partial def identifyAndSolve (f : Poly Rat) : Option (String × List (RadExpr Rat)) := do
  if f.coeffs.size - 1 != 5 then failure
  let gr ← identifyGaloisGroup5 f
  let tg := gr.grGroup
  if !tg.tgSolvable then failure
  let roots ← solveViaTower gr f
  pure (tg.tgName, roots)

/-- Solve a polynomial returning radical expressions for all roots. -/
partial def solvePoly (f : Poly Rat) : Option (List (RadExpr Rat)) := do
  let (_, roots) ← identifyAndSolve f
  pure roots

/-- Try to express an algebraic number as a radical expression (degree > 4). -/
partial def solveAlgNum (a : AlgNum) : Option (RadExpr Rat) := do
  let p := a.anMinPoly
  let d := p.coeffs.size - 1
  if d ≤ 4 then failure
  let allRoots ← solvePoly p
  let toRat (n : Int) : Rat := n
  let eps : Rat := toRat 1 / toRat (10 ^ 15)
  let approxVal := ratToFloat (algApprox eps a)
  pickClosestReal allRoots approxVal

end Surd
