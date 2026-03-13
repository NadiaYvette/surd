/-
  Surd.Galois.RadicalTower — Radical tower construction for solvable
  quintics via Lagrange resolvents.

  Pipeline: depress → findCyclicOrdering → DFT → matchDs →
  reconstruct R_j^5 → selectBranch5 → inverse DFT → un-depress.
-/
import Surd.Poly.Univariate
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Radical.DAG
import Surd.Galois.Identify
import Surd.Galois.Resolvent
import Surd.Galois.TransitiveGroup
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Complex helpers (using CFloat from Resolvent, now public)
-- ---------------------------------------------------------------------------

private def cfA (a b : CFloat) : CFloat := ⟨a.re + b.re, a.im + b.im⟩
private def cfS (a b : CFloat) : CFloat := ⟨a.re - b.re, a.im - b.im⟩
private def cfM (a b : CFloat) : CFloat :=
  ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩
private def cfD (a b : CFloat) : CFloat :=
  let d := b.re * b.re + b.im * b.im
  if d == 0 then ⟨0, 0⟩
  else ⟨(a.re * b.re + a.im * b.im) / d, (a.im * b.re - a.re * b.im) / d⟩
private def cfMag' (a : CFloat) : Float := Float.sqrt (a.re * a.re + a.im * a.im)
private def cfR (r : Float) : CFloat := ⟨r, 0⟩
private def cfPow' (a : CFloat) (n : Nat) : CFloat :=
  (List.range n).foldl (fun acc _ => cfM acc a) ⟨1, 0⟩
private def cfPolar (mag angle : Float) : CFloat :=
  ⟨mag * Float.cos angle, mag * Float.sin angle⟩
private def cfPhase (a : CFloat) : Float := Float.atan2 a.im a.re

-- ---------------------------------------------------------------------------
-- ω₅ helpers
-- ---------------------------------------------------------------------------

private def piF : Float := 3.141592653589793

/-- ω₅^k as CFloat. -/
private def omega5C (k : Nat) : CFloat :=
  cfPolar 1 (2 * piF * k.toFloat / 5)

/-- ω₅^k as CFloat, for signed index. -/
private def omega5CI (k : Int) : CFloat :=
  let k' := ((k % 5) + 5) % 5
  omega5C k'.toNat

/-- ω₅ = e^{2πi/5} as RadExpr Rat. -/
private def omega5Expr : RadExpr Rat :=
  let cos5 := RadExpr.mul (RadExpr.inv (RadExpr.lit 4))
               (RadExpr.add (RadExpr.root 2 (RadExpr.lit 5)) (RadExpr.neg (RadExpr.lit 1)))
  let sin5 := RadExpr.mul (RadExpr.inv (RadExpr.lit 4))
               (RadExpr.root 2 (RadExpr.add (RadExpr.lit 10)
                 (RadExpr.mul (RadExpr.lit 2) (RadExpr.root 2 (RadExpr.lit 5)))))
  let i := RadExpr.root 2 (RadExpr.lit (-1))
  RadExpr.add cos5 (RadExpr.mul i sin5)

/-- ω₅^k as RadExpr, reduced modulo 5. -/
private def omegaPow5 (k : Nat) : RadExpr Rat :=
  let k' := k % 5
  if k' == 0 then RadExpr.lit 1
  else if k' == 1 then omega5Expr
  else RadExpr.pow omega5Expr (Int.ofNat k')

-- ---------------------------------------------------------------------------
-- Rational approximation
-- ---------------------------------------------------------------------------

private def approxRatRT (x : Float) : Rat :=
  let sign : Float := if x < 0 then -1 else 1
  let ax := Float.abs x
  let (_, bestN, bestD) := (List.range 10000).foldl (fun (bestErr, bestN, bestD) d' =>
    let d := d' + 1
    let n := Float.round (ax * d.toFloat)
    let approx := n / d.toFloat
    let err := Float.abs (approx - ax)
    if err < bestErr then (err, n, d) else (bestErr, bestN, bestD)
  ) (1e20, 0.0, 1)
  let nAbs : Int := (Float.abs (sign * bestN)).toUInt64.toNat |> Int.ofNat
  let nFinal : Int := if x < 0 then -nAbs else nAbs
  (nFinal : Rat) / (Int.ofNat bestD : Rat)

private def ratAbs (r : Rat) : Rat :=
  if r < 0 then -r else r

-- ---------------------------------------------------------------------------
-- Score rationality
-- ---------------------------------------------------------------------------

private def scoreRational (c : CFloat) : Float :=
  Float.abs c.im + Float.abs (c.re - Float.round c.re)

-- ---------------------------------------------------------------------------
-- DAG evaluation helper
-- ---------------------------------------------------------------------------

private def dagEvalC (e : RadExpr Rat) : CFloat :=
  let c := dagEvalComplex (RadDAG.toDAG e)
  ⟨c.re, c.im⟩

-- ---------------------------------------------------------------------------
-- Find cyclic ordering
-- ---------------------------------------------------------------------------

private partial def permsNat : List Nat → List (List Nat)
  | [] => [[]]
  | xs => xs.enum.flatMap fun (i, x) =>
      let rest := xs.take i ++ xs.drop (i + 1)
      (permsNat rest).map (x :: ·)

private def scoreOrdering (roots : List CFloat) (ordering : List Nat)
    (groupName : String) : Float :=
  let ordered := ordering.map fun i => roots.get! i
  let rj (j : Nat) : CFloat := (List.range 5).foldl (fun acc k =>
    cfA acc (cfM (omega5C ((j * k) % 5)) (ordered.get! k))) ⟨0, 0⟩
  let rjPows := (List.range 5).map fun j => cfPow' (rj j) 5
  let dVals := (List.range 5).map fun s =>
    cfM (cfR 0.2) ((List.range 5).foldl (fun acc j =>
      cfA acc (cfM (omega5C ((5 - (j * s) % 5) % 5)) (rjPows.get! j))) ⟨0, 0⟩)
  let d0 := dVals.get! 0; let d1 := dVals.get! 1; let d2 := dVals.get! 2
  let d3 := dVals.get! 3; let d4 := dVals.get! 4
  match groupName with
  | "C5" => dVals.foldl (fun acc d => acc + scoreRational d) (0 : Float)
  | "D5" =>
    scoreRational d0
      + scoreRational (cfA d1 d4) + scoreRational (cfM d1 d4)
      + scoreRational (cfA d2 d3) + scoreRational (cfM d2 d3)
  | "F20" =>
    let e1 := [d1, d2, d3, d4].foldl cfA ⟨0, 0⟩
    let e2 := cfA (cfA (cfA (cfM d1 d2) (cfM d1 d3)) (cfA (cfM d1 d4) (cfM d2 d3)))
               (cfA (cfM d2 d4) (cfM d3 d4))
    let e3 := cfA (cfA (cfM (cfM d1 d2) d3) (cfM (cfM d1 d2) d4))
               (cfA (cfM (cfM d1 d3) d4) (cfM (cfM d2 d3) d4))
    let e4 := cfM (cfM d1 d2) (cfM d3 d4)
    scoreRational d0 + scoreRational e1 + scoreRational e2
      + scoreRational e3 + scoreRational e4
  | _ => 1e10

/-- Find cyclic ordering of 5 roots. -/
def findCyclicOrdering (roots : List CFloat) (n : Nat) (groupName : String)
    : Option (List Nat) :=
  if n != 5 then none
  else
    let orderings := (permsNat [1, 2, 3, 4]).map (0 :: ·)
    let scored := orderings.map fun o => (o, scoreOrdering roots o groupName)
    let sorted := scored.mergeSort (fun a b => a.2 ≤ b.2)
    match sorted with
    | (bestO, bestScore) :: (_, secondScore) :: _ =>
      if bestScore < 10 && bestScore < 0.5 * secondScore then some bestO
      else if bestScore < 5 then some bestO
      else none
    | [(bestO, bestScore)] => if bestScore < 5 then some bestO else none
    | _ => none

-- ---------------------------------------------------------------------------
-- DFT coefficient matching
-- ---------------------------------------------------------------------------

private def matchRatC (c : CFloat) : Option (RadExpr Rat) :=
  if Float.abs c.im > 0.1 then none
  else some (RadExpr.lit (approxRatRT c.re))

private def matchDsToQ (dVals : List CFloat) : Option (List (RadExpr Rat)) :=
  dVals.mapM fun d =>
    if Float.abs d.im > 0.01 then none
    else some (RadExpr.lit (approxRatRT d.re))

private def matchDsD5 (dVals : List CFloat) : Option (List (RadExpr Rat)) := do
  let d0 := dVals.get! 0; let d1 := dVals.get! 1; let d2 := dVals.get! 2
  let d3 := dVals.get! 3; let d4 := dVals.get! 4

  let d0Expr ← matchRatC d0

  let s1C := cfA d1 d4; let p1C := cfM d1 d4
  let s1 ← matchRatC s1C; let p1 ← matchRatC p1C
  let s1R := approxRatRT s1C.re; let p1R := approxRatRT p1C.re
  let disc1R := s1R * s1R - 4 * p1R
  let disc1Expr := RadExpr.add (RadExpr.mul s1 s1)
                    (RadExpr.neg (RadExpr.mul (RadExpr.lit 4) p1))
  let sqrtDisc1 := RadExpr.root 2 disc1Expr
  let d1Plus := RadExpr.mul (RadExpr.inv (RadExpr.lit 2))
                 (RadExpr.add s1 sqrtDisc1)
  let d1Minus := RadExpr.mul (RadExpr.inv (RadExpr.lit 2))
                  (RadExpr.add s1 (RadExpr.neg sqrtDisc1))
  let sqrtDisc1Val : CFloat :=
    if disc1R ≥ 0 then ⟨Float.sqrt (ratToFloat disc1R), 0⟩
    else ⟨0, Float.sqrt (ratToFloat (ratAbs disc1R))⟩
  let d1PlusVal := cfD (cfA s1C sqrtDisc1Val) (cfR 2)
  let (d1Expr, d4Expr) :=
    if cfMag' (cfS d1PlusVal d1) < cfMag' (cfS d1PlusVal d4)
    then (d1Plus, d1Minus) else (d1Minus, d1Plus)

  let s2C := cfA d2 d3; let p2C := cfM d2 d3
  let s2 ← matchRatC s2C; let p2 ← matchRatC p2C
  let s2R := approxRatRT s2C.re; let p2R := approxRatRT p2C.re
  let disc2R := s2R * s2R - 4 * p2R
  let disc2Expr := RadExpr.add (RadExpr.mul s2 s2)
                    (RadExpr.neg (RadExpr.mul (RadExpr.lit 4) p2))
  let sqrtDisc2 := RadExpr.root 2 disc2Expr
  let d2Plus := RadExpr.mul (RadExpr.inv (RadExpr.lit 2))
                 (RadExpr.add s2 sqrtDisc2)
  let d2Minus := RadExpr.mul (RadExpr.inv (RadExpr.lit 2))
                  (RadExpr.add s2 (RadExpr.neg sqrtDisc2))
  let sqrtDisc2Val : CFloat :=
    if disc2R ≥ 0 then ⟨Float.sqrt (ratToFloat disc2R), 0⟩
    else ⟨0, Float.sqrt (ratToFloat (ratAbs disc2R))⟩
  let d2PlusVal := cfD (cfA s2C sqrtDisc2Val) (cfR 2)
  let (d2Expr, d3Expr) :=
    if cfMag' (cfS d2PlusVal d2) < cfMag' (cfS d2PlusVal d3)
    then (d2Plus, d2Minus) else (d2Minus, d2Plus)

  pure [d0Expr, d1Expr, d2Expr, d3Expr, d4Expr]

-- Q(ω₅) matching

private def matchSingleOmega5 (d : CFloat) : Option (RadExpr Rat) :=
  let candidates := (List.range 5).map fun k =>
    let v := cfM d (omega5C ((5 - k) % 5))  -- d · ω₅^{-k}
    (k, v, scoreRational v)
  let best := candidates.foldl (fun b c => if c.2.2 < b.2.2 then c else b)
    (0, ⟨(0 : Float), 0⟩, 1e10)
  if best.2.2 < 0.01 then
    let r := approxRatRT best.2.1.re
    if best.1 == 0 then some (RadExpr.lit r)
    else some (RadExpr.mul (RadExpr.lit r) (omegaPow5 best.1))
  else none

private def matchTwoTermOmega5 (d : CFloat) : Option (RadExpr Rat) :=
  let tries := (List.range 4).flatMap fun k' =>
    let k := k' + 1
    let bApprox := (cfM d (omega5C ((5 - k) % 5))).re
    let r := approxRatRT bApprox
    let remainder := cfS d (cfM (cfR (ratToFloat r)) (omega5C k))
    [(k, r, remainder, scoreRational remainder)]
  let best := tries.foldl (fun b c => if c.2.2.2 < b.2.2.2 then c else b)
    (0, (0 : Rat), ⟨(0 : Float), 0⟩, 1e10)
  if best.2.2.2 < 0.01 then
    let a := approxRatRT best.2.2.1.re
    some (RadExpr.add (RadExpr.lit a)
           (RadExpr.mul (RadExpr.lit best.2.1) (omegaPow5 best.1)))
  else none

private def buildQOmega5Expr (a0 a1 a2 a3 : Rat) : RadExpr Rat :=
  let terms : List (Rat × Nat) := [(a0, 0), (a1, 1), (a2, 2), (a3, 3)]
  let nonzero := terms.filter fun (c, _) => c != 0
  let mkTerm : Rat × Nat → RadExpr Rat
    | (c, 0) => RadExpr.lit c
    | (c, k) => RadExpr.mul (RadExpr.lit c) (omegaPow5 k)
  match nonzero.map mkTerm with
  | [] => RadExpr.lit 0
  | [t] => t
  | t :: ts => ts.foldl RadExpr.add t

private def matchGeneralQOmega5 (d : CFloat) : Option (RadExpr Rat) :=
  let s1 := Float.sin (2 * piF / 5)
  let s2 := Float.sin (4 * piF / 5)
  let c1 := Float.cos (2 * piF / 5)
  let c2 := Float.cos (4 * piF / 5)
  let candidateRats (x : Float) : List Rat :=
    let r := approxRatRT x
    if Float.abs (ratToFloat r - x) < 0.01 then [r] else []
  let candidates := (candidateRats (d.im / s1)).flatMap fun a1R =>
    let a1 := ratToFloat a1R
    let v := (d.im - a1 * s1) / s2
    (candidateRats v).flatMap fun vR =>
      let uCandidates := candidateRats ((d.re - a1 * c1) / c2) ++ [0]
      uCandidates.flatMap fun uR =>
        let a0D := d.re - a1 * c1 - ratToFloat uR * c2
        (candidateRats a0D).filterMap fun a0R =>
          let a2R := (uR + vR) / 2
          let a3R := (uR - vR) / 2
          let recon := ratToFloat a0R + ratToFloat a1R * c1
                       + (ratToFloat a2R + ratToFloat a3R) * c2
          let reconIm := ratToFloat a1R * s1
                         + (ratToFloat a2R - ratToFloat a3R) * s2
          let err := Float.abs (recon - d.re) + Float.abs (reconIm - d.im)
          some (err, a0R, a1R, a2R, a3R)
  match candidates.mergeSort (fun a b => a.1 ≤ b.1) with
  | (err, a0R, a1R, a2R, a3R) :: _ =>
    if err < 0.01 then some (buildQOmega5Expr a0R a1R a2R a3R) else none
  | _ => none

private def matchQOmega5 (d : CFloat) : Option (RadExpr Rat) :=
  matchSingleOmega5 d <|> matchTwoTermOmega5 d <|> matchGeneralQOmega5 d

private def matchDsF20 (dVals : List CFloat) : Option (List (RadExpr Rat)) := do
  let d0Expr ← matchRatC (dVals.get! 0)
  let d1Expr ← matchQOmega5 (dVals.get! 1)
  let d2Expr ← matchQOmega5 (dVals.get! 2)
  let d3Expr ← matchQOmega5 (dVals.get! 3)
  let d4Expr ← matchQOmega5 (dVals.get! 4)
  pure [d0Expr, d1Expr, d2Expr, d3Expr, d4Expr]

private def matchDs (groupName : String) (dVals : List CFloat)
    : Option (List (RadExpr Rat)) :=
  match groupName with
  | "C5" => matchDsToQ dVals
  | "D5" => matchDsD5 dVals
  | "F20" => matchDsF20 dVals
  | _ => none

-- ---------------------------------------------------------------------------
-- Branch selection
-- ---------------------------------------------------------------------------

/-- Select correct 5th root branch: R_j = ω₅^{b_j} · ⁵√(R_j⁵). -/
private def selectBranch5 (rj5Expr : RadExpr Rat) (targetVal : CFloat) : RadExpr Rat :=
  let rj5Val := dagEvalC rj5Expr
  let mag := cfMag' rj5Val
  let principalVal := cfPolar (Float.pow mag 0.2) (cfPhase rj5Val / 5)
  let principalRoot := RadExpr.root 5 rj5Expr
  let scored := (List.range 5).map fun k =>
    (k, cfMag' (cfS (cfM (omega5C k) principalVal) targetVal))
  let bestK := (scored.foldl (fun best cur =>
    if cur.2 < best.2 then cur else best) (0, 1e30)).1
  if bestK == 0 then principalRoot
  else RadExpr.mul (omegaPow5 bestK) principalRoot

-- ---------------------------------------------------------------------------
-- Match to original root ordering
-- ---------------------------------------------------------------------------

private def matchToOriginal (exprs : List (RadExpr Rat)) (numRoots : List CFloat)
    : List (RadExpr Rat) :=
  let exprVals := exprs.map fun e => (e, dagEvalC e)
  numRoots.map fun t =>
    (exprVals.foldl (fun best (e, v) =>
      let d := cfMag' (cfS v t)
      if d < best.2 then (e, d) else best
    ) (exprs.head!, 1e30)).1

-- ---------------------------------------------------------------------------
-- Main solver
-- ---------------------------------------------------------------------------

/-- Solve a solvable quintic via Lagrange resolvent descent. -/
partial def solveViaTower (gr : GaloisResult) (f : Poly Rat)
    : Option (List (RadExpr Rat)) := do
  if !gr.grGroup.tgSolvable then failure
  if f.coeffs.size - 1 != 5 then failure

  let cs := f.coeffs.toList
  let lc := cs.getLast!
  let monicCs := cs.map (· / lc)
  let a4 := monicCs.get! 4
  let shiftVal : Rat := -(a4 / 5)
  let numRoots := gr.grRoots
  let depRoots := numRoots.map fun r =>
    cfS r (cfR (ratToFloat shiftVal))
  let groupName := gr.grGroup.tgName

  let ordering ← findCyclicOrdering gr.grRoots 5 groupName
  let orderedRoots := ordering.map fun i => depRoots.get! i

  -- Compute R_j and R_j⁵ numerically
  let rjVals := (List.range 5).map fun j =>
    (List.range 5).foldl (fun acc k =>
      cfA acc (cfM (omega5C ((j * k) % 5)) (orderedRoots.get! k))) ⟨0, 0⟩
  let rjPows := rjVals.map fun r => cfPow' r 5

  -- DFT: d_s = (1/5) Σ_j ω₅^{-js} R_j⁵
  let dVals := (List.range 5).map fun s =>
    cfM (cfR 0.2) ((List.range 5).foldl (fun acc j =>
      cfA acc (cfM (omega5C ((5 - (j * s) % 5) % 5)) (rjPows.get! j))) ⟨0, 0⟩)

  let dExprs ← matchDs groupName dVals

  -- R_j⁵ = Σ_s d_s · ω₅^{js}
  let rjPowExprs := (List.range 4).map fun j' =>
    let j := j' + 1
    (List.range 5).foldl (fun acc s =>
      RadExpr.add acc (RadExpr.mul (dExprs.get! s) (omegaPow5 ((j * s) % 5)))
    ) (RadExpr.lit 0)

  -- Select correct branch
  let rjExprs := (List.range 4).map fun j' =>
    let j := j' + 1
    selectBranch5 (rjPowExprs.get! j') (rjVals.get! j)

  let allR := (RadExpr.lit 0 : RadExpr Rat) :: rjExprs

  -- Inverse DFT: α_k = (1/5) Σ_j ω₅^{-jk} R_j
  let rootExprs := (List.range 5).map fun k =>
    RadExpr.mul (RadExpr.inv (RadExpr.lit 5))
      ((List.range 5).foldl (fun acc j =>
        RadExpr.add acc (RadExpr.mul (omegaPow5 ((5 - (j * k) % 5) % 5)) (allR.get! j))
      ) (RadExpr.lit 0))

  -- Un-depress
  let finalExprs := rootExprs.map fun e =>
    RadExpr.add e (RadExpr.lit shiftVal)

  pure (matchToOriginal finalExprs numRoots)

end Surd
