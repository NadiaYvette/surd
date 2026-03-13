/-
  Surd.Trig — Exact symbolic evaluation of trigonometric functions at
  rational multiples of π.

  Every trig value at a rational multiple of π can be expressed in
  radicals, since cyclotomic extensions have abelian (hence solvable)
  Galois groups.

  The primary entry points are cosExact and sinExact, which compute
  cos(pπ/q) and sin(pπ/q) as radical expressions.
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Radical.DAG
import Surd.Radical.Normalize
import Surd.Radical.NormalForm
import Surd.Trig.Galois
import Surd.Trig.RootOfUnity
import Surd.Poly.Cyclotomic
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- TrigResult
-- ---------------------------------------------------------------------------

/-- Result of exact trig evaluation. -/
inductive TrigResult where
  | radical : RadExpr Rat → TrigResult
  | minPoly : Poly Rat → TrigResult
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Chebyshev
-- ---------------------------------------------------------------------------

/-- Chebyshev polynomial evaluation: T_k(x). -/
partial def chebyshev (k : Int) (x : RadExpr Rat) : RadExpr Rat :=
  if k == 0 then .lit 1
  else if k == 1 then x
  else go 2 (.lit 1) x
where
  go (n : Int) (t0 t1 : RadExpr Rat) : RadExpr Rat :=
    if n > k then t1
    else
      let t2 := RadExpr.add (.mul (.mul (.lit 2) x) t1) (.neg t0)
      go (n + 1) t1 t2

-- ---------------------------------------------------------------------------
-- Safe normalization
-- ---------------------------------------------------------------------------

/-- Normalize with size guard. -/
private partial def safeNormalize (e : RadExpr Rat) : RadExpr Rat :=
  let dag := RadDAG.toDAG e
  let d := RadDAG.dagDepth dag
  let s := RadDAG.dagSize dag
  if d > 30 || s > 500 then e
  else normalize e

/-- Try denesting followed by normalization. -/
private partial def safeDenestAndNormalize (e : RadExpr Rat) : RadExpr Rat :=
  let dag := RadDAG.toDAG e
  let d := RadDAG.dagDepth dag
  let s := RadDAG.dagSize dag
  if d > 50 || s > 5000 then e
  else if d ≤ 20 && s ≤ 500 then safeNormalize e
  else e

-- ---------------------------------------------------------------------------
-- Simplification
-- ---------------------------------------------------------------------------

/-- Simplify a trig result for display. -/
def simplifyTrigResult : TrigResult → TrigResult
  | .radical e =>
    let nRads := (RadExpr.collectRadicalIndices e).length
    if nRads > 5 then .radical e
    else .radical (fromNormExpr (toNormExpr e))
  | other => other

/-- Compute simplified sin from simplified cos. -/
partial def simplifiedSin (p q : Int) : TrigResult → TrigResult
  | .radical c =>
    let p' := p % (2 * q)
    let p'' := if p' < 0 then p' + 2 * q else p'
    let positive := p'' ≥ 0 && p'' ≤ q
    if p'' == 0 || p'' == q then .radical (.lit 0)
    else
      let sin2 := fromNormExpr (toNormExpr (.add (.lit 1) (.neg (.mul c c))))
      let sinExpr := RadExpr.root 2 sin2
      let signed := if positive then sinExpr else .neg sinExpr
      .radical (safeNormalize signed)
  | other => other

-- ---------------------------------------------------------------------------
-- Direct period sin
-- ---------------------------------------------------------------------------

/-- Compute sin(2πk/n) directly from Gauss period expressions. -/
private partial def directPeriodSin (n k : Int) : Option (RadExpr Rat) := do
  let periods ← allPeriodsViaGauss n
  let pk ← periods.find? (fun (k', _) => k' == k) |>.map Prod.snd
  let pnk ← periods.find? (fun (k', _) => k' == n - k) |>.map Prod.snd
  let nRadsPk := (RadExpr.collectRadicalIndices pk).length
  let nRadsPnk := (RadExpr.collectRadicalIndices pnk).length
  if nRadsPk > 10 || nRadsPnk > 10 then none
  else
    let pk' := fromNormExpr (toNormExpr pk)
    let pnk' := fromNormExpr (toNormExpr pnk)
    let i := RadExpr.root 2 (.lit (-1 : Rat))
    let sinForm := RadExpr.mul (.mul (.inv (.lit 2)) (.neg i)) (.add pk' (.neg pnk'))
    some (fromNormExpr (toNormExpr sinForm))

-- ---------------------------------------------------------------------------
-- Range reduction
-- ---------------------------------------------------------------------------

mutual

/-- cos(pπ/q) where 0 ≤ p ≤ 2q. -/
partial def cosInRange (p q : Int) : TrigResult :=
  if p == 0 then .radical (.lit 1)
  else if 2 * p == q then .radical (.lit 0)
  else if p == q then .radical (.lit (-1))
  else if 2 * p == 3 * q then .radical (.lit 0)
  else if 2 * p > q && p < q then
    match cosInRange (q - p) q with
    | .radical e => .radical (.neg e)
    | other => other
  else if p > q && 2 * p < 3 * q then
    match cosInRange (p - q) q with
    | .radical e => .radical (.neg e)
    | other => other
  else if 2 * p ≥ 3 * q then
    cosInRange (2 * q - p) q
  else cosFirstQuadrant p q

/-- cos(pπ/q) in the first quadrant (0 < p < q/2). -/
partial def cosFirstQuadrant (p q : Int) : TrigResult :=
  let g := p.gcd (2 * q)
  let n := (2 * q / g).toNat
  let k := p / g
  if k == 1 then
    match cosOfUnity n with
    | some e => .radical (safeDenestAndNormalize e)
    | none => .minPoly (cyclotomic n)
  else
    match cosOfUnity n with
    | some base =>
      let cheb := chebyshev k base
      let nRads := (RadExpr.collectRadicalIndices cheb).length
      if nRads > 3 then .radical (safeDenestAndNormalize cheb)
      else
        let simplified := fromNormExpr (toNormExpr cheb)
        .radical (safeDenestAndNormalize simplified)
    | none => .minPoly (cyclotomic n)

end -- mutual

-- ---------------------------------------------------------------------------
-- Cos/Sin reduced
-- ---------------------------------------------------------------------------

private partial def cosReduced (p q : Int) : TrigResult :=
  let p' := p % (2 * q)
  let p'' := if p' < 0 then p' + 2 * q else p'
  cosInRange p'' q

private partial def sinReduced (p q : Int) : TrigResult :=
  let p' := p % (2 * q)
  let p'' := if p' < 0 then p' + 2 * q else p'
  let positive := p'' ≥ 0 && p'' ≤ q
  if p'' == 0 || p'' == q then .radical (.lit 0)
  else
    let g := p''.gcd (2 * q)
    let n := (2 * q / g).toNat
    let k := p'' / g
    match directPeriodSin (Int.ofNat n) k with
    | some sinExpr => .radical sinExpr
    | none =>
      match cosReduced p q with
      | .radical c =>
        let sin2 := fromNormExpr (toNormExpr (.add (.lit 1) (.neg (.mul c c))))
        let sinExpr := RadExpr.root 2 sin2
        let signed := if positive then sinExpr else .neg sinExpr
        .radical (safeDenestAndNormalize signed)
      | other => other

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

/-- Compute cos(pπ/q) exactly. -/
partial def cosExact (p q : Int) : TrigResult :=
  if q ≤ 0 then .radical (.lit 0)
  else
    let g := (Int.natAbs p).gcd q.toNat
    let p' := p / Int.ofNat g
    let q' := q / Int.ofNat g
    cosReduced p' q'

/-- Compute sin(pπ/q) exactly. -/
partial def sinExact (p q : Int) : TrigResult :=
  if q ≤ 0 then .radical (.lit 0)
  else
    let g := (Int.natAbs p).gcd q.toNat
    let p' := p / Int.ofNat g
    let q' := q / Int.ofNat g
    sinReduced p' q'

/-- Compute tan(pπ/q) exactly, as sin/cos. -/
partial def tanExact (p q : Int) : Option TrigResult :=
  match sinExact p q, cosExact p q with
  | .radical s, .radical c =>
    some (.radical (.mul s (.inv c)))
  | _, _ => none

/-- Return the cyclotomic polynomial of n (annihilating polynomial for cos(2π/n)). -/
def cosMinPoly (n : Nat) : Poly Rat :=
  cyclotomic n

end Surd
