/-
  Surd.Radical.Denest — Top-level radical denesting dispatcher.

  Applies the appropriate denesting algorithm based on the
  root index and expression structure.
-/
import Surd.Radical.Denest.Sqrt
import Surd.Radical.Denest.NthRoot
import Surd.Radical.Denest.Landau
import Surd.Radical.Eval
import Surd.Radical.Normalize
import Surd.Radical.Expr
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Full denesting pass without pre-normalization. -/
partial def denestFull : RadExpr Rat → RadExpr Rat
  | e@(.root 2 inner) =>
    let specialized := denestSqrt e
    if changed specialized e then specialized
    else tryLandau 2 inner e
  | e@(.root n inner) =>
    let specialized := denestNthRoot e
    if changed specialized e then specialized
    else tryLandau n inner e
  | .neg a => .neg (denestFull a)
  | .add a b => .add (denestFull a) (denestFull b)
  | .mul a b => .mul (denestFull a) (denestFull b)
  | .inv a => .inv (denestFull a)
  | .pow a n => .pow (denestFull a) n
  | e => e
where
  -- Try Landau denesting as a fallback
  tryLandau (n : Int) (inner : RadExpr Rat) (original : RadExpr Rat) : RadExpr Rat :=
    let inner' := denestFull inner
    match denestRadical n inner' with
    | some denested =>
      let dv := evalFloat denested
      let ov := evalFloat original
      if Float.abs (dv - ov) < 1.0e-10
      then denestFull denested  -- recurse on result
      else original
    | none => .root n (denestFull inner)
  -- Check if denesting changed anything (structural equality)
  changed (new old : RadExpr Rat) : Bool := new != old

/-- Denest a radical expression: normalize first, then apply all
    available denesting algorithms recursively. -/
partial def denest (expr : RadExpr Rat) : RadExpr Rat :=
  denestFull (normalize expr)

end Surd
