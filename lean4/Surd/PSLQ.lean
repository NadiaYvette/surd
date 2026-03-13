/-
  Surd.PSLQ — Integer relation finding via the PSLQ algorithm.

  Given a vector x = (x₁, ..., xₙ) of real numbers, finds an integer
  vector m = (m₁, ..., mₙ) such that m·x = 0, or determines that
  no such relation exists within a given bound.

  Primary application: given α, find its minimal polynomial by
  searching for an integer relation among (1, α, α², ..., αᵈ).

  Reference: Ferguson & Bailey, "A Polynomial Time, Numerically
  Stable Integer Relation Algorithm" (1999).
-/
import Surd.Poly.Univariate
import Surd.Poly.Factoring
import Surd.Radical.Eval
import Surd.Trig.Galois
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Matrix/vector helpers (plain arrays)
-- ---------------------------------------------------------------------------

private abbrev Vec := Array Float
private abbrev Mat := Array (Array Float)

private def vecGet (v : Vec) (i : Nat) : Float :=
  v.getD i 0

private def matGet (m : Mat) (i j : Nat) : Float :=
  (m.getD i #[]).getD j 0

private def mkVec (n : Nat) (f : Nat → Float) : Vec :=
  (List.range n).toArray.map f

private def mkMat (rows cols : Nat) (f : Nat → Nat → Float) : Mat :=
  (List.range rows).toArray.map fun i =>
    (List.range cols).toArray.map fun j => f i j

private def identityMat (n : Nat) : Mat :=
  mkMat n n fun i j => if i == j then 1 else 0

-- ---------------------------------------------------------------------------
-- Givens rotation
-- ---------------------------------------------------------------------------

/-- Compute Givens rotation coefficients (c, s) to zero out b in [a, b]. -/
private def givensRotation (a b : Float) : Float × Float :=
  if b == 0 then (1, 0)
  else if Float.abs b > Float.abs a then
    let tau := -a / b
    let s := 1 / Float.sqrt (1 + tau * tau)
    (s * tau, s)
  else
    let tau := -b / a
    let c := 1 / Float.sqrt (1 + tau * tau)
    (c, c * tau)

-- ---------------------------------------------------------------------------
-- Swap helpers
-- ---------------------------------------------------------------------------

private def swapVec (v : Vec) (i j : Nat) : Vec :=
  let vi := vecGet v i
  let vj := vecGet v j
  (v.set! i vj).set! j vi

private def swapRows (m : Mat) (i j : Nat) : Mat :=
  let ri := m.getD i #[]
  let rj := m.getD j #[]
  (m.set! i rj).set! j ri

private def swapCols (m : Mat) (i j : Nat) : Mat :=
  m.map fun row =>
    let ci := row.getD i 0
    let cj := row.getD j 0
    (row.set! i cj).set! j ci

-- ---------------------------------------------------------------------------
-- Hermite reduction
-- ---------------------------------------------------------------------------

private def hermiteReduce (n : Nat) (h : Mat) (a : Mat) (b : Mat) (y : Vec)
    : Mat × Mat × Mat × Vec :=
  let nH := n - 1  -- number of H columns
  let pairs := (List.range (n - 1)).flatMap fun i' =>
    let i := i' + 1
    (List.range i).reverse.map fun j => (i, j)
  pairs.foldl (fun (h, a, b, y) (i, j) =>
    let hjj := matGet h j j
    if Float.abs hjj < 1e-20 then (h, a, b, y)
    else
      let t := Float.round (matGet h i j / hjj)
      if t == 0 then (h, a, b, y)
      else
        let h' := mkMat n nH fun r c =>
          if r == i then matGet h i c - t * matGet h j c
          else matGet h r c
        let a' := mkMat n n fun r c =>
          if r == i then matGet a i c - t * matGet a j c
          else matGet a r c
        let b' := mkMat n n fun r c =>
          if c == j then matGet b r j + t * matGet b r i
          else matGet b r c
        let y' := mkVec n fun k =>
          if k == j then vecGet y j - t * vecGet y i
          else vecGet y k
        (h', a', b', y')
  ) (h, a, b, y)

-- ---------------------------------------------------------------------------
-- PSLQ core
-- ---------------------------------------------------------------------------

/-- PSLQ integer relation finding.
    Given reals x₁, ..., xₙ, finds integers m₁, ..., mₙ with Σ mᵢxᵢ = 0. -/
partial def pslq (xs : List Float) (maxIter : Nat) : Option (List Int) :=
  let n := xs.length
  if n < 2 then none
  else if xs.any Float.isNaN || xs.any Float.isInf then none
  else
    let normX := Float.sqrt (xs.foldl (fun acc x => acc + x * x) 0)
    if normX < 1e-20 then none
    else
      let x := xs.toArray.map (· / normX)

      -- Partial sums of squares
      let ss := mkVec n fun j =>
        Float.sqrt ((List.range (n - j)).foldl (fun acc k =>
          acc + vecGet x (j + k) * vecGet x (j + k)) 0)

      -- Initial y = x
      let y0 := x

      -- Initial H: n × (n-1) lower trapezoidal
      let h0 := mkMat n (n - 1) fun i j =>
        let sj := vecGet ss j
        let sj1 := vecGet ss (j + 1)
        if i < j then 0
        else if i == j then
          if sj < 1e-20 then 0 else sj1 / sj
        else
          if sj < 1e-20 || sj1 < 1e-20 then 0
          else -(vecGet x i) * (vecGet x j) / (sj * sj1)

      let a0 := identityMat n
      let b0 := identityMat n
      let gam := Float.sqrt 2

      go y0 h0 a0 b0 0 n maxIter gam xs
where
  go (y : Vec) (h : Mat) (a : Mat) (b : Mat) (iter : Nat)
      (n : Nat) (maxIter : Nat) (gam : Float) (origXs : List Float)
      : Option (List Int) :=
    if iter ≥ maxIter then none
    else
      -- Step 1: Select pivot m to maximize γ^(j+1) |H_jj|
      let (pivotM, _) := (List.range (n - 1)).foldl (fun (best : Nat × Float) j =>
        let val := gam.pow (j.toFloat + 1) * Float.abs (matGet h j j)
        if val > best.2 then (j, val) else best
      ) (0, gam * Float.abs (matGet h 0 0))

      -- Step 2: Exchange rows pivotM ↔ pivotM+1
      let y1 := swapVec y pivotM (pivotM + 1)
      let h1 := swapRows h pivotM (pivotM + 1)
      let a1 := swapRows a pivotM (pivotM + 1)
      let b1 := swapCols b pivotM (pivotM + 1)

      -- Step 3: Givens rotation on H columns pivotM, pivotM+1
      let h2 := if pivotM < n - 2 then
        let hA := matGet h1 pivotM pivotM
        let hB := matGet h1 pivotM (pivotM + 1)
        let (co, si) := givensRotation hA hB
        mkMat n (n - 1) fun i j =>
          if j == pivotM then
            co * matGet h1 i pivotM + si * (if pivotM + 1 < n - 1 then matGet h1 i (pivotM + 1) else 0)
          else if j == pivotM + 1 then
            -si * matGet h1 i pivotM + co * matGet h1 i (pivotM + 1)
          else matGet h1 i j
      else h1

      -- Step 4: Hermite reduction
      let (h3, a2, b2, y2) := hermiteReduce n h2 a1 b1 y1

      -- Step 5: Check for integer relation
      match checkRelation n b2 origXs with
      | some rel => some rel
      | none => go y2 h3 a2 b2 (iter + 1) n maxIter gam origXs

  checkRelation (n : Nat) (b : Mat) (origXs : List Float) : Option (List Int) :=
    let origArr := origXs.toArray
    (List.range n).findSome? fun j =>
      let col := (List.range n).map fun i =>
        Float.round (matGet b i j) |> floatToInt
      let nonZero := col.any (· != 0)
      let dot := (List.range n).foldl (fun acc i =>
        acc + intToFloat (col.get! i) * vecGet origArr i) 0
      if nonZero && Float.abs dot < 1e-10 then some col else none

-- ---------------------------------------------------------------------------
-- Minimal polynomial search
-- ---------------------------------------------------------------------------

/-- Find the minimal polynomial of a real algebraic number.
    Given a numerical approximation α and a maximum degree d,
    searches for p(x) with integer coefficients such that p(α) = 0. -/
partial def findMinPoly (alpha : Float) (maxDeg : Nat) : Option (List Int) :=
  tryDegree 1
where
  tryDegree (d : Nat) : Option (List Int) :=
    if d > maxDeg then none
    else
      let powers := (List.range (d + 1)).map fun i =>
        alpha.pow i.toFloat
      match pslq powers 2000 with
      | some rel =>
        if rel.all (· == 0) then tryDegree (d + 1)
        else if isMinPoly rel alpha then some rel
        else tryDegree (d + 1)
      | none => tryDegree (d + 1)

  isMinPoly (rel : List Int) (alpha : Float) : Bool :=
    let d := rel.length - 1
    let leadingNonZero := match rel.get? d with
      | some c => c != 0
      | none => false
    let maxCoeff := rel.foldl (fun acc c => Nat.max acc c.natAbs) 0
    let smallCoeffs := maxCoeff ≤ 10000
    let residual := Float.abs ((rel.enum).foldl (fun acc (i, c) =>
      acc + intToFloat c * alpha.pow i.toFloat) 0)
    let relResidual := residual / (if intToFloat (Int.ofNat maxCoeff) > 1 then intToFloat (Int.ofNat maxCoeff) else 1)
    let toRat (c : Int) : Rat := c
    let poly : Poly Rat := Poly.mkPoly (rel.map toRat |>.toArray)
    let factors := factorSquareFree (Poly.monic poly)
    let irred := factors.length == 1
    leadingNonZero && smallCoeffs && relResidual < 1e-8 && irred

end Surd
