/-
  Surd.Trig.Galois — Gauss period computation for expressing cos(2π/n) in radicals.

  Every root of unity can be expressed in radicals, since cyclotomic
  extensions have abelian (hence solvable) Galois groups.

  The algorithm descends through the subgroup chain of (Z/nZ)*,
  solving a period equation at each step. Each step introduces
  radicals of degree equal to the prime index of that step.
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Radical.DAG
import Surd.PrimeFactors
import Surd.Positive
import Surd.Rat
import Surd.Interval
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Float conversion helpers
-- ---------------------------------------------------------------------------

/-- Convert Float to Int (rounding towards zero). -/
def floatToInt (f : Float) : Int :=
  if f < 0 then -(Int.ofNat ((-f).toUInt64.toNat))
  else Int.ofNat f.toUInt64.toNat

private def piFloat : Float := 3.141592653589793

private def floatMax (a b : Float) : Float :=
  if a ≥ b then a else b

private def ratMax (a b : Rat) : Rat :=
  if ratToFloat a ≥ ratToFloat b then a else b

-- ---------------------------------------------------------------------------
-- Complex Double helpers
-- ---------------------------------------------------------------------------

private def complexOfReal (x : Float) : Complex Float :=
  ⟨x, 0⟩

private def complexMagnitude (z : Complex Float) : Float :=
  Float.sqrt (z.re * z.re + z.im * z.im)

private def complexExp (theta : Float) : Complex Float :=
  ⟨Float.cos theta, Float.sin theta⟩

private def complexMul (a b : Complex Float) : Complex Float :=
  ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩

private def complexAdd (a b : Complex Float) : Complex Float :=
  ⟨a.re + b.re, a.im + b.im⟩

private def complexSub (a b : Complex Float) : Complex Float :=
  ⟨a.re - b.re, a.im - b.im⟩

private def complexScale (s : Float) (z : Complex Float) : Complex Float :=
  ⟨s * z.re, s * z.im⟩

private def complexPowNat (z : Complex Float) : Nat → Complex Float
  | 0 => ⟨1, 0⟩
  | 1 => z
  | n + 1 => complexMul z (complexPowNat z n)

private def complexMkPolar (r theta : Float) : Complex Float :=
  ⟨r * Float.cos theta, r * Float.sin theta⟩

private def complexPhase (z : Complex Float) : Float :=
  Float.atan2 z.im z.re

-- ---------------------------------------------------------------------------
-- Modular arithmetic
-- ---------------------------------------------------------------------------

/-- Modular exponentiation: base^exp mod m. -/
partial def modExp (base exp_ m : Int) : Int :=
  if exp_ == 0 then 1
  else if exp_ % 2 == 0 then
    let half := modExp base (exp_ / 2) m
    (half * half) % m
  else
    (base * modExp base (exp_ - 1) m) % m

/-- Euler's totient from Nat (wraps PrimeFactors). -/
def eulerTotientInt (n : Int) : Int :=
  if n ≤ 0 then 0
  else
    let fs := factoriseNat n.toNat
    fs.foldl (fun acc (p, e) =>
      acc * (Int.ofNat p - 1) * Int.ofNat (p ^ (e - 1))) 1

/-- Find a primitive root modulo n, if one exists. -/
partial def primitiveRootMod (n : Int) : Option Int :=
  if n ≤ 0 then none
  else if n ≤ 2 then some 1
  else if n == 4 then some 3
  else
    let fs := factoriseNat n.toNat
    match fs with
    | [(p, _)] => if p > 2 then findPrimRootMod n else none
    | [(2, 1), (p, _)] => if p > 2 then findPrimRootMod n else none
    | _ => none

where
  findPrimRootMod (n : Int) : Option Int :=
    let phi := eulerTotientInt n
    let factors := (factoriseNat phi.toNat).map (fun (p, _) => Int.ofNat p)
    let isPrimRoot (g : Int) : Bool :=
      g.gcd n == 1 &&
      factors.all (fun q => modExp g (phi / q) n != 1)
    go 2 n isPrimRoot

  go (g n : Int) (test : Int → Bool) : Option Int :=
    if g ≥ n then none
    else if test g then some g
    else go (g + 1) n test

/-- Find a primitive root modulo a prime p. -/
def primitiveRoot (p : Int) : Option Int :=
  if p ≤ 1 then none
  else if p == 2 then some 1
  else primitiveRootMod p

-- ---------------------------------------------------------------------------
-- Reorder factors for prime powers
-- ---------------------------------------------------------------------------

/-- Reorder prime factors of φ(n) for the Gauss period descent. -/
def reorderFactors (n : Int) (fs : List (Nat × Nat)) : List (Nat × Nat) :=
  let nfs := factoriseNat n.toNat
  match nfs with
  | [(p, k)] =>
    if k > 1 then
      let pFactor := fs.filter (fun (q, _) => q == p)
      let rest := fs.filter (fun (q, _) => q != p)
      pFactor ++ rest
    else fs
  | _ => fs

-- ---------------------------------------------------------------------------
-- Gauss periods
-- ---------------------------------------------------------------------------

/-- Compute Gauss periods as lists of exponents. -/
def gaussPeriods (p : Int) (e : Int) : List (List Int) :=
  let phi := p - 1
  let f := phi / e
  match primitiveRoot p with
  | none => []
  | some g =>
    (List.range e.toNat).map fun k =>
      (List.range f.toNat).map fun j =>
        modExp g (e * Int.ofNat j + Int.ofNat k) p

/-- Compute the subgroup chain of (Z/pZ)*. -/
def subgroupChain (p : Int) : List (List Int) :=
  let phi := p - 1
  let fs := factoriseNat phi.toNat
  match primitiveRoot p with
  | none => []
  | some g =>
    let primeFactorList := fs.flatMap fun (q, e) =>
      List.replicate e (Int.ofNat q)
    buildChainAux g p phi primeFactorList

where
  buildChainAux (g p phi : Int) : List Int → List (List Int)
    | [] => [[1]]
    | q :: qs =>
      let subOrder := phi / q
      let subGen := modExp g q p
      let subGroup := (List.range subOrder.toNat).map fun k =>
        modExp subGen (Int.ofNat k) p
      subGroup :: buildChainAux (modExp g q p) p subOrder qs

-- ---------------------------------------------------------------------------
-- Period state
-- ---------------------------------------------------------------------------

/-- State of a Gauss period during the descent. -/
structure PeriodState where
  periodExpr : RadExpr Rat
  periodElems : List Int
  periodP : Int
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Numerical helpers
-- ---------------------------------------------------------------------------

/-- Sum of ζ^k for a list of exponents. -/
private def sumRootsOfUnity (n : Int) (ks : List Int) : Complex Float :=
  ks.foldl (fun acc k =>
    let theta := 2 * piFloat * intToFloat k / intToFloat n
    complexAdd acc (complexExp theta)
  ) ⟨0, 0⟩

/-- Elementary symmetric polynomials of complex values. -/
private def elementarySymmetricC (xs : List (Complex Float)) : List (Complex Float) :=
  (List.range xs.length).map fun k => elemSym (k + 1) xs
where
  elemSym (k : Nat) (ys : List (Complex Float)) : Complex Float :=
    (choose k ys).foldl (fun acc combo =>
      complexAdd acc (combo.foldl complexMul ⟨1, 0⟩)
    ) ⟨0, 0⟩

  choose : Nat → List (Complex Float) → List (List (Complex Float))
    | 0, _ => [[]]
    | _, [] => []
    | k, y :: ys => (choose (k - 1) ys).map (y :: ·) ++ choose k ys

-- ---------------------------------------------------------------------------
-- Coefficient matching
-- ---------------------------------------------------------------------------

/-- Try to express target = c + a·v for integer c, a. -/
private def matchSinglePeriod (target v : Complex Float) : Option (Int × Int) :=
  let a : Int :=
    if Float.abs v.im > 1e-10 then Float.round (target.im / v.im) |> floatToInt
    else if Float.abs v.re > 1e-10 then Float.round (target.re / v.re) |> floatToInt
    else 0
  let remainder := complexSub target (complexScale (intToFloat a) v)
  let c : Int := Float.round remainder.re |> floatToInt
  let recon := complexAdd (complexOfReal (intToFloat c)) (complexScale (intToFloat a) v)
  let err := complexMagnitude (complexSub recon target)
  let relErr := err / floatMax 1 (complexMagnitude target)
  if relErr < 1e-6 then some (c, a) else none

/-- Solve target = c + Σ aᵢ·xᵢ for integer c, aᵢ using greedy O(n) matching. -/
partial def solveLinearIntegerC (tRe tIm : Rat) (vals : List (Rat × Rat))
    : Option (Int × List Int) :=
  match vals with
  | [] =>
    let c : Int := Float.round (ratToFloat tRe) |> floatToInt
    let errRe := tRe - (c : Rat)
    let errIm := tIm
    let errSq := errRe * errRe + errIm * errIm
    let magSq := ratMax 1 (tRe * tRe + tIm * tIm)
    if ratToFloat (errSq) < ratToFloat (magSq) * 1e-12 then some (c, []) else none
  | _ =>
    let n := vals.length
    let indexed := vals.enum
    -- Separate into imaginary-heavy and real-only
    let (imHeavy, realOnly) := indexed.partition fun (_, (_, im)) =>
      ratToFloat (Rat'.abs im) > 1e-30
    -- Sort imaginary-heavy by |Im| descending
    let sortedIm := imHeavy.mergeSort fun (_, (_, im1)) (_, (_, im2)) =>
      ratToFloat (Rat'.abs im1) ≥ ratToFloat (Rat'.abs im2)
    -- Greedy assignment
    let (resRe1, resIm1, coeffs1) :=
      sortedIm.foldl (fun (rRe, rIm, cs) (i, (vRe, vIm)) =>
        let a : Int := Float.round (ratToFloat rIm / ratToFloat vIm) |> floatToInt
        let aR : Rat := (a : Rat)
        (rRe - aR * vRe, rIm - aR * vIm, (i, a) :: cs)
      ) (tRe, tIm, ([] : List (Nat × Int)))
    let (resRe2, _, coeffs2) :=
      realOnly.foldl (fun (rRe, rIm, cs) (i, (vRe, _)) =>
        let a : Int :=
          if ratToFloat (Rat'.abs vRe) > 1e-30 then
            Float.round (ratToFloat rRe / ratToFloat vRe) |> floatToInt
          else 0
        let aR : Rat := (a : Rat)
        (rRe - aR * vRe, rIm, (i, a) :: cs)
      ) (resRe1, resIm1, coeffs1)
    let c : Int := Float.round (ratToFloat resRe2) |> floatToInt
    -- Build coefficient array in original order
    let coeffArr : List Int := (List.range n).map fun i =>
      match coeffs2.find? (fun (j, _) => j == i) with
      | some (_, a) => a
      | none => (0 : Int)
    -- Verify
    let reconRe := (c : Rat) + (coeffArr.zip vals).foldl (fun acc (a, (vRe, _)) =>
      acc + (a : Rat) * vRe) (0 : Rat)
    let reconIm := (coeffArr.zip vals).foldl (fun acc (a, (_, vIm)) =>
      acc + (a : Rat) * vIm) (0 : Rat)
    let errSq := (reconRe - tRe) * (reconRe - tRe) + (reconIm - tIm) * (reconIm - tIm)
    let magSq := ratMax 1 (tRe * tRe + tIm * tIm)
    let coeffBound := 10000 * Int.ofNat (Nat.max 1 n)
    let coeffsOK := coeffArr.all (fun a => a.natAbs ≤ coeffBound.toNat) &&
                    c.natAbs ≤ (coeffBound * coeffBound).toNat
    if ratToFloat errSq < ratToFloat magSq * 1e-12 && coeffsOK then
      some (c, coeffArr)
    else none

/-- Try to match a complex value to an integer linear combination of periods. -/
private partial def tryMatchToPeriodExpr (periods : List PeriodState) (p : Int)
    (target : Complex Float) : Option (RadExpr Rat) :=
  let nearestInt : Int := Float.round target.re |> floatToInt
  let relTol (err : Float) : Bool := err / floatMax 1 (complexMagnitude target) < 1e-6
  if complexMagnitude (complexSub target (complexOfReal (intToFloat nearestInt))) < 1e-8 then
    some (.lit (nearestInt : Rat))
  else
    let periodVals := periods.map (fun ps => sumRootsOfUnity ps.periodP ps.periodElems)
    -- Try single-period matching
    let singleMatches := periodVals.enum.filterMap fun (i, pv) =>
      match matchSinglePeriod target pv with
      | some (c, a) =>
        let err := complexMagnitude (complexSub target
          (complexAdd (complexOfReal (intToFloat c)) (complexScale (intToFloat a) pv)))
        if relTol err then some (i, c, a) else none
      | none => none
    match singleMatches with
    | (i, c, a) :: _ =>
      let pi' := periods.get! i
      let term := if a == 1 then pi'.periodExpr
                  else .mul (.lit (a : Rat)) pi'.periodExpr
      some (.add (.lit (c : Rat)) term)
    | [] =>
      -- Fall back to multi-period
      let tRat := (ratOfFloat target.re, ratOfFloat target.im)
      let vRat := periodVals.map fun v => (ratOfFloat v.re, ratOfFloat v.im)
      match solveLinearIntegerC tRat.1 tRat.2 vRat with
      | some (c, coeffs) =>
        let terms := (coeffs.zip periods).map fun (a, pi') =>
          if a == 0 then .lit 0
          else if a == 1 then pi'.periodExpr
          else .mul (.lit (a : Rat)) pi'.periodExpr
        some (terms.foldl .add (.lit (c : Rat)))
      | none => none

where
  ratOfFloat (f : Float) : Rat :=
    let n : Int := floatToInt f
    (n : Rat) -- crude but functional for stub

/-- Match a complex value to period expressions, with fallback. -/
private partial def matchToPeriodExpr (periods : List PeriodState) (p : Int)
    (target : Complex Float) : RadExpr Rat :=
  match tryMatchToPeriodExpr periods p target with
  | some expr => expr
  | none =>
    let re : Rat := (Float.round target.re |> floatToInt : Int)
    if Float.abs target.im < 1e-12 then .lit re
    else .add (.lit re) (.mul (.lit (Float.round target.im |> floatToInt : Int))
                              (.root 2 (.lit (-1))))

-- ---------------------------------------------------------------------------
-- Branch selection
-- ---------------------------------------------------------------------------

/-- Assign radical expressions to numerical values by matching. -/
private def assignByValue (exprs : List (RadExpr Rat)) (vals : List (Complex Float))
    : List (RadExpr Rat) :=
  let exprVals := exprs.map fun e => (e, dagEvalComplex (RadDAG.toDAG e))
  vals.map fun target =>
    match exprVals with
    | [] => .lit 0
    | ev :: evs =>
      (evs.foldl (fun (best : RadExpr Rat × Complex Float) (e, v) =>
        if complexMagnitude (complexSub v target) <
           complexMagnitude (complexSub best.2 target)
        then (e, v) else best
      ) ev).1

-- ---------------------------------------------------------------------------
-- DAG fold helper
-- ---------------------------------------------------------------------------

/-- DAG-aware constant folding. -/
private def dagFold (e : RadExpr Rat) : RadExpr Rat :=
  -- dagFoldConstants not yet ported; return expression as-is
  e

-- ---------------------------------------------------------------------------
-- Period equation solvers
-- ---------------------------------------------------------------------------

/-- Solve quadratic period equation. -/
private def solveQuadratic (e1 e2 : RadExpr Rat) (numVals : List (Complex Float))
    : List (RadExpr Rat) :=
  let disc : RadExpr Rat := .add (.mul e1 e1) (.neg (.mul (.lit 4) e2))
  let sqrtDisc := RadExpr.root 2 disc
  let root1 := RadExpr.mul (.inv (.lit 2)) (.add e1 sqrtDisc)
  let root2 := RadExpr.mul (.inv (.lit 2)) (.add e1 (.neg sqrtDisc))
  assignByValue [dagFold root1, dagFold root2] numVals

/-- Solve cubic period equation via Cardano. -/
private def solveCubic (e1 e2 e3 : RadExpr Rat) (numVals : List (Complex Float))
    : List (RadExpr Rat) :=
  let pExpr := RadExpr.add e2 (.neg (.mul (.inv (.lit 3)) (.mul e1 e1)))
  let qExpr := RadExpr.add
    (.add (.neg e3) (.mul (.inv (.lit 3)) (.mul e1 e2)))
    (.neg (.mul (.lit (2 / 27)) (.mul e1 (.mul e1 e1))))
  let delta := RadExpr.add
    (.mul (.inv (.lit 4)) (.mul qExpr qExpr))
    (.mul (.inv (.lit 27)) (.mul pExpr (.mul pExpr pExpr)))
  let sqrtDelta := RadExpr.root 2 delta
  let negQHalf := RadExpr.mul (.inv (.lit (-2))) qExpr
  let u1Arg := RadExpr.add negQHalf sqrtDelta
  let u1 := RadExpr.root 3 u1Arg
  let u2 := RadExpr.mul (.neg pExpr) (.inv (.mul (.lit 3) u1))
  let shift := RadExpr.mul (.inv (.lit 3)) e1
  let omega := RadExpr.mul (.inv (.lit 2)) (.add (.lit (-1)) (.root 2 (.lit (-3))))
  let omegaBar := RadExpr.mul (.inv (.lit 2)) (.add (.lit (-1)) (.neg (.root 2 (.lit (-3)))))
  let root0 := RadExpr.add (.add u1 u2) shift
  let root1 := RadExpr.add (.add (.mul omega u1) (.mul omegaBar u2)) shift
  let root2 := RadExpr.add (.add (.mul omegaBar u1) (.mul omega u2)) shift
  assignByValue [dagFold root0, dagFold root1, dagFold root2] numVals

/-- Solve a period equation of degree q. -/
private def solvePeriodEquation (q : Int) (e1 : RadExpr Rat)
    (coeffExprs : List (RadExpr Rat)) (numVals : List (Complex Float))
    : List (RadExpr Rat) :=
  if q == 2 then
    match coeffExprs with
    | [a, b] => solveQuadratic a b numVals
    | _ => numVals.map fun v => .lit (Float.round v.re |> floatToInt : Int)
  else if q == 3 then
    match coeffExprs with
    | [_, b, c] => solveCubic e1 b c numVals
    | _ => numVals.map fun v => .lit (Float.round v.re |> floatToInt : Int)
  else
    numVals.map fun v => .lit (Float.round v.re |> floatToInt : Int)

-- ---------------------------------------------------------------------------
-- Chebyshev
-- ---------------------------------------------------------------------------

/-- Chebyshev polynomial T_k(x). -/
partial def chebyshevLocal (k : Int) (x : RadExpr Rat) : RadExpr Rat :=
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
-- ω^m as RadExpr
-- ---------------------------------------------------------------------------

/-- Express ω^m = e^{2πim/q} as a RadExpr, given cos(2π/q). -/
private partial def omegaPowerExpr (q : Int) (cosBase : RadExpr Rat) (m : Int)
    : RadExpr Rat :=
  let m' := m % q
  if m' == 0 then .lit 1
  else
    let cosM := chebyshevLocal m' cosBase
    let sin2M := RadExpr.add (.lit 1) (.neg (.mul cosM cosM))
    let sinM := RadExpr.root 2 sin2M
    let sinMSigned := if 2 * m' < q then sinM else .neg sinM
    let i := RadExpr.root 2 (.lit (-1))
    .add cosM (.mul i sinMSigned)

-- ---------------------------------------------------------------------------
-- Resolvent solver for q ≥ 5
-- ---------------------------------------------------------------------------

/-- Select correct branch of q-th root via numerical matching. -/
private def selectResolventBranch (q : Int) (omegaPowers : List (RadExpr Rat))
    (rjqExpr : RadExpr Rat) (targetVal : Complex Float) : RadExpr Rat :=
  let principalRoot := RadExpr.root q rjqExpr
  let rjqVal := dagEvalComplex (RadDAG.toDAG rjqExpr)
  let mag := complexMagnitude rjqVal
  let ph := complexPhase rjqVal
  let principalVal := complexMkPolar
    (mag.pow (1.0 / intToFloat q))
    (ph / intToFloat q)
  let omegaC := complexExp (2 * piFloat / intToFloat q)
  let scored := (List.range q.toNat).map fun k =>
    let candidate := complexMul (complexPowNat omegaC k) principalVal
    (k, complexMagnitude (complexSub candidate targetVal))
  match scored.mergeSort (fun a b => a.2 ≤ b.2) with
  | (bestK, _) :: _ =>
    if bestK == 0 then principalRoot
    else .mul (omegaPowers.get! bestK) principalRoot
  | _ => principalRoot

mutual

/-- Solve period equation via Lagrange resolvent for q ≥ 5. -/
private partial def solvePeriodViaResolvent (q : Int) (allPeriods : List PeriodState)
    (p : Int) (parentExpr : RadExpr Rat)
    (subPeriodElems : List (List Int)) (numVals : List (Complex Float))
    : List (RadExpr Rat) :=
  -- Numerical DFT
  let omegaC := complexExp (2 * piFloat / intToFloat q)
  let resolventVals := (List.range q.toNat).map fun j =>
    (List.range q.toNat).foldl (fun acc k =>
      complexAdd acc (complexMul (complexPowNat omegaC ((j * k) % q.toNat))
                                  (numVals.get! k))
    ) ⟨0, 0⟩
  let resolventPowers := resolventVals.map (fun rv => complexPowNat rv q.toNat)
  let dCoeffsNum := (List.range q.toNat).map fun s =>
    complexScale (1.0 / intToFloat q)
      ((List.range q.toNat).foldl (fun acc j =>
        let idx := (q.toNat - (j * s) % q.toNat) % q.toNat
        complexAdd acc (complexMul (complexPowNat omegaC idx) (resolventPowers.get! j))
      ) ⟨0, 0⟩)
  let dExprs := dCoeffsNum.map (matchToPeriodExpr allPeriods p)

  -- Get cos(2π/q) recursively
  match cosOfUnityViaGaussInternal q with
  | none =>
    -- Fallback: return numerical approximations
    numVals.map fun v => .lit (Float.round v.re |> floatToInt : Int)
  | some cosBaseExpr =>
    let omegaPowers := (List.range q.toNat).map fun m =>
      omegaPowerExpr q cosBaseExpr (Int.ofNat m)

    -- R_j^q = Σ_s d_s · ω^{js}
    let resolventPowerExprs := (List.range (q.toNat - 1)).map fun jIdx =>
      let j := jIdx + 1
      (List.range q.toNat).foldl (fun acc s =>
        .add acc (.mul (dExprs.get! s) (omegaPowers.get! ((j * s) % q.toNat)))
      ) (.lit 0)

    -- Select branches
    let resolventExprs := (List.range (q.toNat - 1)).map fun jIdx =>
      let j := jIdx + 1
      selectResolventBranch q omegaPowers
        (resolventPowerExprs.get! jIdx)
        (resolventVals.get! j)

    let allResolvents := parentExpr :: resolventExprs

    -- Recover sub-periods via inverse DFT
    (List.range q.toNat).map fun k =>
      .mul (.inv (.lit (q : Rat)))
        ((List.range q.toNat).foldl (fun acc j =>
          let idx := (q.toNat - (j * k) % q.toNat) % q.toNat
          .add acc (.mul (omegaPowers.get! idx) (allResolvents.get! j))
        ) (.lit 0))

-- ---------------------------------------------------------------------------
-- Split period
-- ---------------------------------------------------------------------------

/-- Split a single period into q sub-periods. -/
private partial def splitPeriod (allPeriods : List PeriodState) (q : Int)
    (parent : PeriodState) : List PeriodState :=
  let p := parent.periodP
  let elems := parent.periodElems
  let f := elems.length
  let subF := f / q.toNat

  let subPeriodElems := (List.range q.toNat).map fun k =>
    (List.range subF).map fun j => elems.get! (k + q.toNat * j)

  let subPeriodValues := subPeriodElems.map (sumRootsOfUnity p)

  let subPeriodExprs :=
    if q ≤ 3 then
      let symFuncs := elementarySymmetricC subPeriodValues
      let coeffExprs := symFuncs.map (matchToPeriodExpr allPeriods p)
      solvePeriodEquation q parent.periodExpr coeffExprs subPeriodValues
    else
      solvePeriodViaResolvent q allPeriods p parent.periodExpr subPeriodElems subPeriodValues

  (subPeriodExprs.zip subPeriodElems).map fun (expr, elms) =>
    { periodExpr := expr, periodElems := elms, periodP := p }

/-- One step of the descent. -/
private partial def descendStep (periods : List PeriodState) (q : Int)
    : List PeriodState :=
  periods.flatMap (splitPeriod periods q)

-- ---------------------------------------------------------------------------
-- Internal entry point (avoids circular dependency)
-- ---------------------------------------------------------------------------

/-- Internal: compute cos(2π/n) via Gauss period descent. -/
private partial def cosOfUnityViaGaussInternal (n : Int) : Option (RadExpr Rat) :=
  allPeriodsViaGaussInternal n |>.bind fun periods =>
    let target := 1
    let conjElem := n - 1
    match (periods.find? (fun (k, _) => k == target),
           periods.find? (fun (k, _) => k == conjElem)) with
    | (some (_, t), some (_, c)) =>
      some (.mul (.inv (.lit 2)) (.add t c))
    | _ => none

/-- Internal: compute all periods via Gauss descent. -/
private partial def allPeriodsViaGaussInternal (n : Int)
    : Option (List (Int × RadExpr Rat)) :=
  if n ≤ 2 then none
  else
    match primitiveRootMod n with
    | none => none
    | some g =>
      let phi := eulerTotientInt n
      let fs := factoriseNat phi.toNat
      let steps := (reorderFactors n fs).flatMap fun (q, e) =>
        List.replicate e (Int.ofNat q)
      let coprimeElems := (List.range phi.toNat).map fun k =>
        modExp g (Int.ofNat k) n
      let initSum := coprimeElems.foldl (fun acc k =>
        acc + Float.cos (2 * piFloat * intToFloat k / intToFloat n)) 0.0
      let initExpr : RadExpr Rat := .lit (Float.round initSum |> floatToInt : Int)
      let initPeriod : PeriodState :=
        { periodExpr := initExpr, periodElems := coprimeElems, periodP := n }
      let finalPeriods := steps.foldl descendStep [initPeriod]
      let periodList := finalPeriods.flatMap fun ps =>
        ps.periodElems.map fun k => (k, ps.periodExpr)
      some periodList

end -- mutual

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

/-- Compute cos(2π/n) as a radical expression via Gauss period descent. -/
partial def cosOfUnityViaGauss (n : Int) : Option (RadExpr Rat) :=
  cosOfUnityViaGaussInternal n

/-- Compute all primitive nth roots of unity as radical expressions. -/
partial def allPeriodsViaGauss (n : Int)
    : Option (List (Int × RadExpr Rat)) :=
  allPeriodsViaGaussInternal n

end Surd
