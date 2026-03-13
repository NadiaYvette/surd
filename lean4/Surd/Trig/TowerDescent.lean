/-
  Surd.Trig.TowerDescent — Tower-based Gauss period descent.

  Mirrors the algorithm in Galois but represents periods as elements
  of a dynamically-built field tower (TowerElem) instead of flat RadExpr.
  This preserves the algebraic structure and avoids expression swell.
-/
import Surd.Field.DynTower
import Surd.Radical.Expr
import Surd.Radical.DAG
import Surd.Trig.Galois
import Surd.PrimeFactors
import Surd.Positive
import Surd.Rat
import Surd.Interval
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Float helpers
-- ---------------------------------------------------------------------------

private def piF : Float := 3.141592653589793
private def fmax (a b : Float) : Float := if a ≥ b then a else b

-- ---------------------------------------------------------------------------
-- Complex helpers (reuse from Galois)
-- ---------------------------------------------------------------------------

private def complexOfReal' (x : Float) : Complex Float := ⟨x, 0⟩
private def complexMag (z : Complex Float) : Float :=
  Float.sqrt (z.re * z.re + z.im * z.im)
private def complexExp' (theta : Float) : Complex Float :=
  ⟨Float.cos theta, Float.sin theta⟩
private def cmul (a b : Complex Float) : Complex Float :=
  ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩
private def cadd (a b : Complex Float) : Complex Float :=
  ⟨a.re + b.re, a.im + b.im⟩
private def csub (a b : Complex Float) : Complex Float :=
  ⟨a.re - b.re, a.im - b.im⟩
private def cscale (s : Float) (z : Complex Float) : Complex Float :=
  ⟨s * z.re, s * z.im⟩
private def cpow (z : Complex Float) : Nat → Complex Float
  | 0 => ⟨1, 0⟩
  | n + 1 => cmul z (cpow z n)

-- ---------------------------------------------------------------------------
-- Tower result
-- ---------------------------------------------------------------------------

/-- Result of tower-based descent. -/
structure TowerResult where
  trCos : TowerElem
  trSin : TowerElem
  trPeriods : List (Int × TowerElem)
  trRadExpr : RadExpr Rat

-- ---------------------------------------------------------------------------
-- Period state
-- ---------------------------------------------------------------------------

/-- Period state for tower descent. -/
structure TPeriodState where
  tpElem : TowerElem
  tpElems : List Int
  tpP : Int
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Embedding
-- ---------------------------------------------------------------------------

/-- Embed a tower element into a higher level. -/
private def tEmbed (lvl : TowerLevel) (e : TowerElem) : TowerElem :=
  .tExt (e :: List.replicate (lvl.tlDegree.toNat - 1) (.tRat 0)) lvl

-- ---------------------------------------------------------------------------
-- Sum of roots of unity (numerical)
-- ---------------------------------------------------------------------------

private def sumRootsOfUnityT (n : Int) (ks : List Int) : Complex Float :=
  ks.foldl (fun acc k =>
    let theta := 2 * piF * intToFloat k / intToFloat n
    cadd acc (complexExp' theta)
  ) ⟨0, 0⟩

-- ---------------------------------------------------------------------------
-- Numerical evaluation of tower elements
-- ---------------------------------------------------------------------------

/-- Approximate numerical evaluation of a tower element. -/
partial def evalTowerApprox (e : TowerElem) (_p : Int) : Complex Float :=
  match e with
  | .tRat r => complexOfReal' (ratToFloat r)
  | .tExt cs level =>
    let genVal := evalGenApprox level _p
    cs.enum.foldl (fun acc (i, c) =>
      let cv := evalTowerApprox c _p
      cadd acc (cmul cv (cpow genVal i))
    ) ⟨0, 0⟩

where
  evalGenApprox (level : TowerLevel) (p : Int) : Complex Float :=
    let rVal := evalTowerApprox level.tlRadicand p
    let mag := complexMag rVal
    let ph := Float.atan2 rVal.im rVal.re
    let n := level.tlRootDeg
    ⟨(mag.pow (1.0 / intToFloat n)) * Float.cos (ph / intToFloat n),
     (mag.pow (1.0 / intToFloat n)) * Float.sin (ph / intToFloat n)⟩

-- ---------------------------------------------------------------------------
-- Coefficient matching (tower version)
-- ---------------------------------------------------------------------------

/-- Match a numerical value to an integer linear combination of tower period elements. -/
private partial def matchToTowerElem (periods : List TPeriodState)
    (periodValues : List (Complex Float)) (_p : Int)
    (target : Complex Float) : TowerElem :=
  let nearestInt : Int := Float.round target.re |> floatToInt
  let relTol (err : Float) : Bool := err / fmax 1 (complexMag target) < 1e-6
  if complexMag (csub target (complexOfReal' (intToFloat nearestInt))) < 1e-8 then
    .tRat (nearestInt : Rat)
  else
    -- Try single-period matching
    let singleMatches := periodValues.enum.filterMap fun (i, pv) =>
      matchSinglePeriodT target pv |>.bind fun (c, a) =>
        let err := complexMag (csub target
          (cadd (complexOfReal' (intToFloat c)) (cscale (intToFloat a) pv)))
        if relTol err then some (i, c, a) else none
    match singleMatches with
    | (i, c, a) :: _ =>
      .tRat (c : Rat) + .tRat (a : Rat) * (periods.get! i).tpElem
    | [] =>
      -- Multi-period fallback
      let tRat := (ratOfF target.re, ratOfF target.im)
      let vRat := periodValues.map fun v => (ratOfF v.re, ratOfF v.im)
      match solveLinearIntegerC tRat.1 tRat.2 vRat with
      | some (c, coeffs) =>
        (coeffs.zip periods).foldl (fun acc (a, pi') =>
          if a == 0 then acc
          else acc + .tRat (a : Rat) * pi'.tpElem
        ) (.tRat (c : Rat))
      | none =>
        .tRat (ratOfF target.re)

where
  matchSinglePeriodT (target v : Complex Float) : Option (Int × Int) :=
    let a : Int :=
      if Float.abs v.im > 1e-10 then Float.round (target.im / v.im) |> floatToInt
      else if Float.abs v.re > 1e-10 then Float.round (target.re / v.re) |> floatToInt
      else 0
    let remainder := csub target (cscale (intToFloat a) v)
    let c : Int := Float.round remainder.re |> floatToInt
    let recon := cadd (complexOfReal' (intToFloat c)) (cscale (intToFloat a) v)
    let err := complexMag (csub recon target)
    let relErr := err / fmax 1 (complexMag target)
    if relErr < 1e-6 then some (c, a) else none

  ratOfF (f : Float) : Rat :=
    let n : Int := f|> floatToInt
    (n : Rat)

/-- Assign tower expressions to numerical values. -/
private def assignTowerByValue (exprs : List TowerElem)
    (vals : List (Complex Float)) : List TowerElem :=
  let exprVals := exprs.map fun e =>
    (e, dagEvalComplex (RadDAG.toDAG (towerToRadExpr e)))
  vals.map fun target =>
    match exprVals with
    | [] => .tRat 0
    | ev :: evs =>
      (evs.foldl (fun (best : TowerElem × Complex Float) (e, v) =>
        if complexMag (csub v target) < complexMag (csub best.2 target)
        then (e, v) else best
      ) ev).1

-- ---------------------------------------------------------------------------
-- Tower solvers
-- ---------------------------------------------------------------------------

/-- Quadratic solver in tower arithmetic. -/
private def towerSolveQuadratic (allPeriods : List TPeriodState)
    (nextId : Int) (parent : TPeriodState)
    (subPeriodElems : List (List Int))
    (subPeriodValues : List (Complex Float))
    (periodValues : List (Complex Float))
    (p : Int) : List TPeriodState × Int :=
  let e1 := parent.tpElem
  let e2Num := cmul (subPeriodValues.get! 0) (subPeriodValues.get! 1)
  let e2 := matchToTowerElem allPeriods periodValues p e2Num
  let disc := e1 * e1 - .tRat 4 * e2
  let (lvl, sqrtDisc) := adjoinTowerRoot nextId 2 disc
  let e1' := tEmbed lvl e1
  let root1 := (e1' + sqrtDisc) / .tRat 2
  let root2 := (e1' - sqrtDisc) / .tRat 2
  let roots := assignTowerByValue [root1, root2] subPeriodValues
  (roots.zip subPeriodElems |>.map fun (r, es) =>
    { tpElem := r, tpElems := es, tpP := p }, nextId + 1)

/-- Cubic solver in tower arithmetic. -/
private def towerSolveCubic (allPeriods : List TPeriodState)
    (nextId : Int) (parent : TPeriodState)
    (subPeriodElems : List (List Int))
    (subPeriodValues : List (Complex Float))
    (periodValues : List (Complex Float))
    (p : Int) : List TPeriodState × Int :=
  let e1 := parent.tpElem
  let e2Num := cadd (cadd (cmul (subPeriodValues.get! 0) (subPeriodValues.get! 1))
                          (cmul (subPeriodValues.get! 0) (subPeriodValues.get! 2)))
                     (cmul (subPeriodValues.get! 1) (subPeriodValues.get! 2))
  let e2 := matchToTowerElem allPeriods periodValues p e2Num
  let e3Num := cmul (cmul (subPeriodValues.get! 0) (subPeriodValues.get! 1))
                     (subPeriodValues.get! 2)
  let e3 := matchToTowerElem allPeriods periodValues p e3Num
  let pCoeff := e2 - e1 * e1 / .tRat 3
  let qCoeff := - e3 + e1 * e2 / .tRat 3 - .tRat (2 / 27) * e1 * e1 * e1
  let delta := qCoeff * qCoeff / .tRat 4 + pCoeff * pCoeff * pCoeff / .tRat 27
  let (lvl1, sqrtDelta) := adjoinTowerRoot nextId 2 delta
  let qCoeff' := tEmbed lvl1 qCoeff
  let pCoeff' := tEmbed lvl1 pCoeff
  let e1' := tEmbed lvl1 e1
  let u1Arg := - qCoeff' / .tRat 2 + sqrtDelta
  let (lvl2, u1) := adjoinTowerRoot (nextId + 1) 3 u1Arg
  let pCoeff'' := tEmbed lvl2 pCoeff'
  let u2 := - pCoeff'' / (.tRat 3 * u1)
  let neg3 := tEmbed lvl2 (tEmbed lvl1 (.tRat (-3)))
  let (lvl3, sqrtNeg3) := adjoinTowerRoot (nextId + 2) 2 neg3
  let omega := (.tRat (-1) + sqrtNeg3) / .tRat 2
  let omegaBar := (.tRat (-1) - sqrtNeg3) / .tRat 2
  let u1'' := tEmbed lvl3 u1
  let u2'' := tEmbed lvl3 u2
  let shift := tEmbed lvl3 (tEmbed lvl2 e1') / .tRat 3
  let root0 := u1'' + u2'' + shift
  let root1 := omega * u1'' + omegaBar * u2'' + shift
  let root2 := omegaBar * u1'' + omega * u2'' + shift
  let roots := assignTowerByValue [root0, root1, root2] subPeriodValues
  (roots.zip subPeriodElems |>.map fun (r, es) =>
    { tpElem := r, tpElems := es, tpP := p }, nextId + 3)

/-- Resolvent solver for q ≥ 5 in tower arithmetic. -/
private partial def towerSolveResolvent (q : Int)
    (allPeriods : List TPeriodState) (nextId : Int)
    (parent : TPeriodState)
    (subPeriodElems : List (List Int))
    (subPeriodValues : List (Complex Float))
    (periodValues : List (Complex Float))
    (p : Int) : List TPeriodState × Int :=
  if q == 5 then
    towerSolveQ5 allPeriods nextId parent subPeriodElems subPeriodValues periodValues p
  else
    -- Fallback: match sub-periods numerically
    let subPeriodTower := subPeriodValues.map
      (matchToTowerElem allPeriods periodValues p)
    (subPeriodTower.zip subPeriodElems |>.map fun (te, es) =>
      { tpElem := te, tpElems := es, tpP := p }, nextId)

where
  /-- Tower-native quintic resolvent (q = 5). -/
  towerSolveQ5 (allPeriods : List TPeriodState) (nextId : Int)
      (parent : TPeriodState)
      (subPeriodElems : List (List Int))
      (subPeriodValues : List (Complex Float))
      (periodValues : List (Complex Float))
      (p : Int) : List TPeriodState × Int :=
    let q := 5
    let omegaC := complexExp' (2 * piF / 5)
    let resolventVals := (List.range q).map fun j =>
      (List.range q).foldl (fun acc k =>
        cadd acc (cmul (cpow omegaC ((j * k) % q)) (subPeriodValues.get! k))
      ) ⟨0, 0⟩
    let resolventPowers := resolventVals.map (fun rv => cpow rv q)
    let dCoeffsNum := (List.range q).map fun s =>
      cscale (1.0 / 5.0)
        ((List.range q).foldl (fun acc j =>
          let idx := (q - (j * s) % q) % q
          cadd acc (cmul (cpow omegaC idx) (resolventPowers.get! j))
        ) ⟨0, 0⟩)
    let dCoeffs := dCoeffsNum.map (matchToTowerElem allPeriods periodValues p)

    -- Build ω₅ via square root adjunctions
    let (_, sqrt5) := adjoinTowerRoot nextId 2 (.tRat 5)
    let cos2pi5 := (sqrt5 - .tRat 1) / .tRat 4
    let sinRadicand := .tRat 10 + .tRat 2 * sqrt5
    let (_, sqrtSinRad) := adjoinTowerRoot (nextId + 1) 2 sinRadicand
    let sin2pi5 := sqrtSinRad / .tRat 4
    let (_, iUnit) := adjoinTowerRoot (nextId + 2) 2 (.tRat (-1))
    let omegaTE := cos2pi5 + iUnit * sin2pi5

    -- Powers ω₅^k
    let omPows := go5 q (.tRat 1) omegaTE []
    -- R_j^5 = Σ_s d_s · ω₅^{js}
    let rjq := (List.range q).map fun j =>
      (List.range q).foldl (fun acc s =>
        acc + (dCoeffs.get! s) * (omPows.get! ((j * s) % q))
      ) (.tRat 0)

    let r0 := parent.tpElem
    -- Adjoin ⁵√(R_j^5) for j = 1..4
    let (r1, nid1) := adjoinAndSelectT nextId 3 1 rjq omPows resolventVals q p
    let (r2, nid2) := adjoinAndSelectT nid1 0 2 rjq omPows resolventVals q p
    let (r3, nid3) := adjoinAndSelectT nid2 0 3 rjq omPows resolventVals q p
    let (r4, nid4) := adjoinAndSelectT nid3 0 4 rjq omPows resolventVals q p
    let rjs := [r0, r1, r2, r3, r4]

    -- Inverse DFT
    let subPeriodTower := (List.range q).map fun k =>
      .tRat (1 / 5 : Rat) *
        ((List.range q).foldl (fun acc j =>
          acc + (omPows.get! ((q - (j * k) % q) % q)) * (rjs.get! j)
        ) (.tRat 0))

    let assigned := assignTowerByValue subPeriodTower subPeriodValues
    (assigned.zip subPeriodElems |>.map fun (r, es) =>
      { tpElem := r, tpElems := es, tpP := p }, nid4)

  go5 (remaining : Nat) (cur omega : TowerElem) (acc : List TowerElem)
      : List TowerElem :=
    if remaining == 0 then acc
    else go5 (remaining - 1) (cur * omega) omega (acc ++ [cur])

  adjoinAndSelectT (nid : Int) (_offset j : Nat)
      (rjq omPows : List TowerElem)
      (resolventVals : List (Complex Float))
      (q : Nat) (p : Int) : TowerElem × Int :=
    let radicand := rjq.get! j
    let (_, alpha) := adjoinTowerRoot (nid + 1) 5 radicand
    let candidates := (List.range q).map fun m =>
      (omPows.get! m) * alpha
    let target := resolventVals.get! j
    let candVals := candidates.map (fun c => evalTowerApprox c p)
    let scored := candVals.enum.map fun (m, cv) =>
      (m, complexMag (csub cv target))
    match scored.mergeSort (fun a b => a.2 ≤ b.2) with
    | (bestM, _) :: _ => (candidates.get! bestM, nid + 2)
    | _ => (alpha, nid + 2)

-- ---------------------------------------------------------------------------
-- Split period (tower version)
-- ---------------------------------------------------------------------------

/-- Split one period into q sub-periods. -/
private partial def towerSplitPeriod (allPeriods : List TPeriodState)
    (q : Int) (p : Int) (nextId : Int) (parent : TPeriodState)
    : List TPeriodState × Int :=
  let elems := parent.tpElems
  let f := elems.length
  let subF := f / q.toNat
  let subPeriodElems := (List.range q.toNat).map fun k =>
    (List.range subF).map fun j => elems.get! (k + q.toNat * j)
  let subPeriodValues := subPeriodElems.map (sumRootsOfUnityT p)
  let periodValues := allPeriods.map (fun ps => sumRootsOfUnityT ps.tpP ps.tpElems)
  match q with
  | 2 => towerSolveQuadratic allPeriods nextId parent subPeriodElems subPeriodValues periodValues p
  | 3 => towerSolveCubic allPeriods nextId parent subPeriodElems subPeriodValues periodValues p
  | _ => towerSolveResolvent q allPeriods nextId parent subPeriodElems subPeriodValues periodValues p

/-- One step of the tower descent. -/
private partial def towerDescentStep (periodsAndId : List TPeriodState × Int)
    (q : Int) (p : Int) : List TPeriodState × Int :=
  let (periods, nextId) := periodsAndId
  periods.foldl (fun (acc, nid) per =>
    let (subs, nid') := towerSplitPeriod periods q p nid per
    (acc ++ subs, nid')
  ) ([], nextId)

-- ---------------------------------------------------------------------------
-- √(-1) as tower element
-- ---------------------------------------------------------------------------

private def towerSqrtNeg1 : TowerElem :=
  let (_, alpha) := adjoinTowerRoot 9999 2 (.tRat (-1))
  alpha

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

/-- Tower-based descent producing all periods. -/
partial def allPeriodsViaTower (n : Int) : Option TowerResult :=
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
        acc + Float.cos (2 * piF * intToFloat k / intToFloat n)) 0.0
      let initExpr : TowerElem := .tRat (Float.round initSum |> floatToInt : Int)
      let initPeriod : TPeriodState :=
        { tpElem := initExpr, tpElems := coprimeElems, tpP := n }
      let (finalPeriods, _) :=
        steps.foldl (fun acc q => towerDescentStep acc q n) ([initPeriod], 1)
      let periodList := finalPeriods.flatMap fun ps =>
        ps.tpElems.map fun k => (k, ps.tpElem)
      let target := 1
      let conjElem := n - 1
      match (periodList.find? (fun (k, _) => k == target),
             periodList.find? (fun (k, _) => k == conjElem)) with
      | (some (_, zeta), some (_, zetaConj)) =>
        let cosVal := (zeta + zetaConj) / .tRat 2
        let cosRad := towerToRadExpr cosVal
        let iElem := towerSqrtNeg1
        let sinVal := (zeta - zetaConj) / (.tRat 2 * iElem)
        some { trCos := cosVal
               trSin := sinVal
               trPeriods := periodList
               trRadExpr := cosRad }
      | _ => none

/-- Compute cos(2π/n) via tower-based descent. -/
partial def cosViaTower (n : Int) : Option (RadExpr Rat) :=
  allPeriodsViaTower n |>.map (fun r => r.trRadExpr)

end Surd
