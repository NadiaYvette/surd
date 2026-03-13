/-
  Surd.Algebraic.Convert — Conversion between radical expressions and algebraic numbers.

  Forward direction (RadExpr → AlgNum): compute the minimal polynomial
  of a radical expression and isolate the correct root.

  Backward direction (AlgNum → RadExpr): given a simplified algebraic
  number (low-degree minimal polynomial), express it as a radical.

  The round-trip RadExpr → AlgNum → RadExpr is the simplification
  pipeline: complex nested radical expressions may simplify to lower-degree
  algebraic numbers with simpler radical representations.
-/
import Surd.Poly.Univariate
import Surd.Poly.Resultant
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Radical.Normalize
import Surd.Radical.LaTeX
import Surd.Algebraic.Number
import Surd.Polynomial.MinimalPoly
import Surd.Interval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal
open Surd.Poly

namespace Surd

/-- Stub: rational roots of a polynomial.
    TODO: implement rational root theorem search. -/
private def rationalRoots (p : Poly Rat) : List Rat :=
  if p.isZero then []
  else
    -- Try ±(factor of constant term)/(factor of leading coeff)
    -- Simplified: just try small integers and simple fractions
    let candidates : List Rat := [0, 1, -1, 2, -2, 3, -3,
      1/2, -1/2, 1/3, -1/3, 2/3, -2/3, 1/4, -1/4, 3/2, -3/2]
    candidates.filter fun r => Poly.eval p r == 0

/-- Convert a radical expression to its canonical algebraic number form.
    Computes the minimal polynomial and isolates the correct root using
    numerical evaluation to pick the right factor. -/
partial def radExprToAlgNum (expr : RadExpr Rat) : AlgNum :=
  let mp := minimalPoly expr
  let approx := evalFloat expr
  match algFromPoly mp approx with
  | some a => a
  | none =>
    let ann := annihilatingPoly expr
    match algFromPoly ann approx with
    | some a => a
    | none =>
      -- Fallback: use a wide interval
      ⟨mp, ⟨-1000, 1000⟩⟩

/-- Solve ax² + bx + c = 0 (monic: a=1), picking root closest to approx. -/
private def solveQuadratic (cs : Array Rat) (approx : Float) : Option (RadExpr Rat) :=
  if cs.size < 3 then none
  else
    let c := cs[0]!
    let b := cs[1]!
    -- x² + bx + c = 0, discriminant = b² - 4c
    let disc := b * b - 4 * c
    if disc < 0 then none  -- complex roots
    else
      let sqrtDisc : RadExpr Rat := .root 2 (.lit disc)
      let r1 : RadExpr Rat := .mul (.inv (.lit 2)) (.add (.neg (.lit b)) sqrtDisc)
      let r2 : RadExpr Rat := .mul (.inv (.lit 2)) (.add (.neg (.lit b)) (.neg sqrtDisc))
      let v1 := evalFloat r1
      let v2 := evalFloat r2
      if Float.abs (v1 - approx) < Float.abs (v2 - approx) then some r1 else some r2

/-- Solve depressed cubic x³ + bx² + cx + d = 0 (monic) via Cardano. -/
private def solveCubic (cs : Array Rat) (approx : Float) : Option (RadExpr Rat) :=
  if cs.size < 4 then none
  else
    let d := cs[0]!
    let c := cs[1]!
    let b := cs[2]!
    -- Depress: substitute x = t - b/3
    let p := c - b * b / 3
    let q := d - b * c / 3 + 2 * b * b * b / 27
    let disc := -(4 * p * p * p + 27 * q * q)
    let shift : RadExpr Rat := .neg (.lit (b / 3))
    if disc >= 0 then
      -- Three real roots. Try rational roots first.
      let poly := Poly.mkPoly cs
      let rats := rationalRoots poly
      match rats with
      | r :: _ => some (.lit r)
      | [] =>
        -- Casus irreducibilis: Cardano with complex intermediates
        let halfQ := q / 2
        let innerDisc := halfQ * halfQ + p * p * p / 27
        let sqrtD : RadExpr Rat := .root 2 (.lit innerDisc)
        let u1 : RadExpr Rat := .root 3 (.add (.neg (.lit halfQ)) sqrtD)
        let u2 : RadExpr Rat := .root 3 (.add (.neg (.lit halfQ)) (.neg sqrtD))
        let omega : RadExpr Rat :=
          .mul (.inv (.lit 2)) (.add (.lit (-1)) (.root 2 (.lit (-3))))
        let omega2 : RadExpr Rat :=
          .mul (.inv (.lit 2)) (.add (.lit (-1)) (.neg (.root 2 (.lit (-3)))))
        let root0 : RadExpr Rat := .add (.add u1 u2) shift
        let root1 : RadExpr Rat := .add (.add (.mul omega u1) (.mul omega2 u2)) shift
        let root2 : RadExpr Rat := .add (.add (.mul omega2 u1) (.mul omega u2)) shift
        let roots := [root0, root1, root2]
        let scored := roots.map fun r => (r, Float.abs (evalFloat r - approx))
        match scored with
        | [] => none
        | s :: ss =>
          let (best, _) := ss.foldl (fun (bestAcc : RadExpr Rat × Float) cur =>
            if cur.2 < bestAcc.2 then cur else bestAcc) s
          some best
    else
      -- One real root. Cardano works directly.
      let halfQ := q / 2
      let innerDisc := halfQ * halfQ + p * p * p / 27
      let sqrtD : RadExpr Rat := .root 2 (.lit innerDisc)
      let u : RadExpr Rat := .root 3 (.add (.neg (.lit halfQ)) sqrtD)
      let v : RadExpr Rat := .root 3 (.add (.neg (.lit halfQ)) (.neg sqrtD))
      some (.add (.add u v) shift)

/-- Solve quartic via Ferrari's method. -/
private def solveQuartic (cs : Array Rat) (approx : Float) : Option (RadExpr Rat) :=
  if cs.size < 5 then none
  else
    let e := cs[0]!
    let d := cs[1]!
    let c := cs[2]!
    let b := cs[3]!
    -- Depress: x = t - b/4
    let p := c - 3 * b * b / 8
    let q := d - b * c / 2 + b * b * b / 8
    let r := e - b * d / 4 + b * b * c / 16 - 3 * b * b * b * b / 256
    let shiftBack (expr : RadExpr Rat) : RadExpr Rat :=
      .add expr (.neg (.lit (b / 4)))
    if q == 0 then
      -- Biquadratic: t⁴ + pt² + r = 0
      let disc := p * p - 4 * r
      if disc < 0 then none
      else
        let sqrtDisc : RadExpr Rat := .root 2 (.lit disc)
        let t2_1 : RadExpr Rat := .mul (.inv (.lit 2)) (.add (.neg (.lit p)) sqrtDisc)
        let t2_2 : RadExpr Rat := .mul (.inv (.lit 2)) (.add (.neg (.lit p)) (.neg sqrtDisc))
        let roots := [
          shiftBack (.root 2 t2_1),
          shiftBack (.neg (.root 2 t2_1)),
          shiftBack (.root 2 t2_2),
          shiftBack (.neg (.root 2 t2_2))
        ]
        let scored := roots.map fun root => (root, Float.abs (evalFloat root - approx))
        match scored with
        | [] => none
        | s :: ss =>
          let (best, _) := ss.foldl (fun acc cur =>
            if cur.2 < acc.2 then cur else acc) s
          some best
    else
      -- General case: resolvent cubic y³ - py² - 4ry + (4pr - q²) = 0
      let rcCs : Array Rat := #[4 * p * r - q * q, -4 * r, -p, 1]
      match solveCubic rcCs 0.0 with
      | none => none
      | some yExpr =>
        -- Factor depressed quartic using resolvent root y
        let sSquared : RadExpr Rat := .add yExpr (.neg (.lit p))
        let s : RadExpr Rat := .root 2 sSquared
        let halfY : RadExpr Rat := .mul (.inv (.lit 2)) yExpr
        let qOver2s : RadExpr Rat := .mul (.lit (q / 2)) (.inv s)
        let c1 : RadExpr Rat := .add halfY (.neg qOver2s)
        let c2 : RadExpr Rat := .add halfY qOver2s
        let disc1 : RadExpr Rat := .add sSquared (.mul (.lit (-4)) c1)
        let disc2 : RadExpr Rat := .add sSquared (.mul (.lit (-4)) c2)
        let sqD1 : RadExpr Rat := .root 2 disc1
        let sqD2 : RadExpr Rat := .root 2 disc2
        let t1 : RadExpr Rat := .mul (.inv (.lit 2)) (.add (.neg s) sqD1)
        let t2 : RadExpr Rat := .mul (.inv (.lit 2)) (.add (.neg s) (.neg sqD1))
        let t3 : RadExpr Rat := .mul (.inv (.lit 2)) (.add s sqD2)
        let t4 : RadExpr Rat := .mul (.inv (.lit 2)) (.add s (.neg sqD2))
        let roots := [t1, t2, t3, t4].map shiftBack
        let scored := roots.map fun root => (root, Float.abs (evalFloat root - approx))
        match scored with
        | [] => none
        | s :: ss =>
          let (best, _) := ss.foldl (fun acc cur =>
            if cur.2 < acc.2 then cur else acc) s
          some best

/-- Convert an algebraic number back to a radical expression,
    if the degree is small enough for closed-form solutions.
    Handles degrees 1-4. Returns none if degree is too high. -/
partial def algNumToRadExpr (a : AlgNum) : Option (RadExpr Rat) :=
  let p := a.anMinPoly
  let d := match p.degree with | some d => d | none => 0
  let approx := algApproxFloat a
  match d with
  | 0 => none
  | 1 => if p.coeffs.size ≥ 2 then some (.lit (-p.coeffs[0]!)) else none
  | 2 => solveQuadratic p.coeffs approx
  | 3 => solveCubic p.coeffs approx
  | 4 => solveQuartic p.coeffs approx
  | _ => none  -- TODO: degree 5 via Galois.Solve when ported

/-- Simplify a radical expression by converting to canonical form
    and back. If the algebraic number has a simpler radical representation,
    return it; otherwise return the original. -/
partial def simplifyViaCanonical (expr : RadExpr Rat) : RadExpr Rat :=
  let algNum := radExprToAlgNum expr
  match algNumToRadExpr algNum with
  | some e => normalize e
  | none => expr

/-- Get a human-readable summary of the algebraic number form
    of a radical expression. -/
partial def algNumInfo (expr : RadExpr Rat) : String :=
  let algNum := radExprToAlgNum expr
  let mp := algNum.anMinPoly
  let d := match mp.degree with | some d => d | none => 0
  let approx := algApproxFloat algNum
  let polyStr := repr mp
  let radStr := match algNumToRadExpr algNum with
    | some e => s!"Radical form: {repr (normalize e)}\nLaTeX: {latex (normalize e)}"
    | none => "Radical form: (degree too high)"
  s!"Minimal polynomial: {polyStr}\nDegree: {d}\nApproximate value: {approx}\n{radStr}"

end Surd
