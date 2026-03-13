/-
  Surd.Poly.RootBound — Root bounding and approximate root finding.

  Cauchy's root bound and bisection-based root approximation,
  sufficient for picking the right factor during algebraic number
  operations.
-/
import Surd.Poly.Univariate
import Surd.Radical.Eval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Cauchy's root bound: all roots have |r| ≤ 1 + max|cᵢ/cₙ|. -/
def rootBound (p : Poly Rat) : Rat :=
  if p.coeffs.size ≤ 1 then 0
  else
    let lc := p.coeffs[p.coeffs.size - 1]!
    if lc == 0 then 0
    else
      let maxRatio := (List.range (p.coeffs.size - 1)).foldl (fun acc i =>
        let r := Rat'.abs (p.coeffs[i]! / lc)
        if r > acc then r else acc
      ) (0 : Rat)
      1 + maxRatio

/-- Bisection root finding. -/
partial def bisectRoot (p : Poly Rat) (lo hi : Rat) (n : Nat) : Rat :=
  match n with
  | 0 => lo
  | n' + 1 =>
    let mid := (lo + hi) / 2
    let fmid := Poly.eval p mid
    let flo := Poly.eval p lo
    if fmid == 0 then mid
    else if Rat'.sign fmid == Rat'.sign flo
      then bisectRoot p mid hi n'
      else bisectRoot p lo mid n'

/-- Find approximate real roots of a polynomial by scanning and bisection. -/
partial def approxRoots (p : Poly Rat) : List Rat :=
  if p.coeffs.size ≤ 1 then []
  else if p.coeffs.size == 2 then
    match p.coeffs.toList with
    | [a, b] => [-a / b]
    | _ => []
  else
    let bound := rootBound p
    let nPts := (bound * 20).num.toNat + 1
    let nPts' := Nat.min nPts 2000
    let pts : List Rat := (List.range (2 * nPts' + 1)).map fun i =>
      -bound + (Int.ofNat i : Rat) * bound / (Int.ofNat nPts' : Rat)
    let signs : List (Rat × Int) := pts.map fun x =>
      (x, Rat'.sign (Poly.eval p x))
    let changes : List (Rat × Rat) := go signs
    let roots := changes.map fun (lo, hi) => bisectRoot p lo hi 50
    if roots.isEmpty then [0] else roots
where
  go : List (Rat × Int) → List (Rat × Rat)
    | [] => []
    | [_] => []
    | (x1, s1) :: (x2, s2) :: rest =>
      if s1 != s2 && s1 != 0 && s2 != 0
      then (x1, x2) :: go ((x2, s2) :: rest)
      else go ((x2, s2) :: rest)

/-- Pick the factor whose numerical root is closest to the target value. -/
def pickClosest (target : Float) (factors : List (Poly Rat)) : Poly Rat :=
  match factors with
  | [] => Poly.mkPoly #[]
  | f :: fs =>
    let score (g : Poly Rat) : Float :=
      let roots := approxRoots g
      match roots with
      | [] => 1e20
      | _ => roots.foldl (fun best r =>
          let d := Float.abs (ratToFloat r - target)
          if d < best then d else best
        ) 1e20
    fs.foldl (fun (best : Poly Rat × Float) g =>
      let s := score g
      if s < best.2 then (g, s) else best
    ) (f, score f) |>.1

end Surd
