/-
  Surd.Poly.Groebner — Gröbner basis computation for multivariate polynomial
  ideals via Buchberger's algorithm.

  Optimizations: Buchberger's first criterion (coprime leading monomials),
  sugar strategy pair selection, inter-reduction.

  Specialized to Rat coefficients (matching MPoly).
-/
import Surd.Poly.Multivariate
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Monomial orderings
-- ---------------------------------------------------------------------------

/-- A total order on monomials. -/
abbrev MonoOrd := Mono → Mono → Ordering

/-- Graded reverse lexicographic order. -/
def grevlex : MonoOrd := compareGrevlex

/-- Graded lexicographic order. -/
def grlex : MonoOrd := fun a b =>
  match compare (Mono.degree a) (Mono.degree b) with
  | .eq => compare a b
  | c => c

/-- Pure lexicographic order. -/
def lexOrd : MonoOrd := fun a b =>
  compareLex a b
where
  compareLex (a b : Mono) : Ordering :=
    let allVars := (a.vars ++ b.vars).map (·.1) |>.eraseDups
    let sorted := allVars.mergeSort (· > ·)
    go sorted a.vars b.vars
  go : List Int → List (Int × Int) → List (Int × Int) → Ordering
    | [], _, _ => .eq
    | v :: vs, avars, bvars =>
      let ae := match avars.find? (fun p => p.1 == v) with
        | some (_, e) => e | none => 0
      let be := match bvars.find? (fun p => p.1 == v) with
        | some (_, e) => e | none => 0
      match compare ae be with
      | .eq => go vs avars bvars
      | c => c

/-- Elimination order: variables in the list are more expensive. -/
def elimOrd (elims : List Int) : MonoOrd := fun a b =>
  let splitMono (vs : List Int) (m : Mono) : Mono × Mono :=
    let (inSet, outSet) := m.vars.partition (fun p => vs.contains p.1)
    (⟨inSet⟩, ⟨outSet⟩)
  let (ae, ar) := splitMono elims a
  let (be, br) := splitMono elims b
  match grevlex ae be with
  | .eq => grevlex ar br
  | c => c

-- ---------------------------------------------------------------------------
-- Leading term extraction
-- ---------------------------------------------------------------------------

/-- Leading term of a polynomial under the given ordering. -/
def leadTermOrd (cmp : MonoOrd) (p : MPoly Rat) : Option (Mono × Rat) :=
  match p.terms with
  | [] => none
  | t :: ts => some (ts.foldl (fun best cur =>
      match cmp cur.1 best.1 with
      | .gt => cur
      | _ => best
    ) t)

/-- Leading monomial under ordering. -/
def leadMonoOrd (cmp : MonoOrd) (p : MPoly Rat) : Option Mono :=
  (leadTermOrd cmp p).map (·.1)

/-- Leading coefficient under ordering. -/
def leadCoeffOrd (cmp : MonoOrd) (p : MPoly Rat) : Option Rat :=
  (leadTermOrd cmp p).map (·.2)

-- ---------------------------------------------------------------------------
-- Polynomial division
-- ---------------------------------------------------------------------------

/-- Multiply a polynomial by a monomial. -/
private def mulByMono (m : Mono) (p : MPoly Rat) : MPoly Rat :=
  (MPoly.mk (p.terms.map fun (pm, pc) => (Mono.mul m pm, pc))).clean

/-- Reduce a polynomial to normal form modulo a list of divisors. -/
private partial def reducePoly (cmp : MonoOrd) (f : MPoly Rat)
    (divisors : List (MPoly Rat)) : MPoly Rat :=
  let lts := divisors.filterMap fun g =>
    match leadTermOrd cmp g with
    | some (lm, lc) => some (lm, lc, g)
    | none => none
  go f MPoly.zero lts
where
  go (p r : MPoly Rat) (lts : List (Mono × Rat × MPoly Rat)) : MPoly Rat :=
    if MPoly.isZero p then r
    else match leadTermOrd cmp p with
    | none => r
    | some (pm, pc) =>
      match findDiv pm lts with
      | some (dm, dc, g) =>
        let qm := Mono.div pm dm
        let qc := pc / dc
        let qt := MPoly.mk [(qm, qc)]
        let p' := MPoly.sub p (MPoly.mul qt g)
        go p' r lts
      | none =>
        let lt := MPoly.mk [(pm, pc)]
        let p' := MPoly.sub p lt
        go p' (MPoly.add r lt) lts
  findDiv (pm : Mono) : List (Mono × Rat × MPoly Rat) → Option (Mono × Rat × MPoly Rat)
    | [] => none
    | (dm, dc, g) :: rest =>
      if Mono.divides dm pm then some (dm, dc, g) else findDiv pm rest

/-- Divide a polynomial by a list of divisors.
    Returns (quotients, remainder). -/
partial def divModMPoly (cmp : MonoOrd) (f : MPoly Rat)
    (divisors : List (MPoly Rat)) : List (MPoly Rat) × MPoly Rat :=
  let n := divisors.length
  let lts := divisors.enum.filterMap fun (i, g) =>
    match leadTermOrd cmp g with
    | some (lm, lc) => some (i, lm, lc)
    | none => none
  go f (List.replicate n MPoly.zero) MPoly.zero lts
where
  go (p : MPoly Rat) (qs : List (MPoly Rat)) (r : MPoly Rat)
      (lts : List (Nat × Mono × Rat)) : List (MPoly Rat) × MPoly Rat :=
    if MPoly.isZero p then (qs, r)
    else match leadTermOrd cmp p with
    | none => (qs, r)
    | some (pm, pc) =>
      match findDiv pm lts with
      | some (i, dm, dc) =>
        let qm := Mono.div pm dm
        let qc := pc / dc
        let qi := MPoly.mk [(qm, qc)]
        let p' := MPoly.sub p (MPoly.mul qi (divisors.get! i))
        let qs' := qs.enum.map fun (j, q) =>
          if j == i then MPoly.add q qi else q
        go p' qs' r lts
      | none =>
        let lt := MPoly.mk [(pm, pc)]
        let p' := MPoly.sub p lt
        go p' qs (MPoly.add r lt) lts
  findDiv (pm : Mono) : List (Nat × Mono × Rat) → Option (Nat × Mono × Rat)
    | [] => none
    | (i, dm, dc) :: rest =>
      if Mono.divides dm pm then some (i, dm, dc) else findDiv pm rest

-- ---------------------------------------------------------------------------
-- S-polynomials
-- ---------------------------------------------------------------------------

/-- S-polynomial of two polynomials. -/
private def sPolynomial (cmp : MonoOrd) (f g : MPoly Rat) : MPoly Rat :=
  match leadTermOrd cmp f, leadTermOrd cmp g with
  | some (mf, cf), some (mg, cg) =>
    let l := Mono.lcm mf mg
    let qf := Mono.div l mf
    let qg := Mono.div l mg
    let tf := MPoly.scale (1 / cf) (mulByMono qf f)
    let tg := MPoly.scale (1 / cg) (mulByMono qg g)
    MPoly.sub tf tg
  | _, _ => MPoly.zero

-- ---------------------------------------------------------------------------
-- Buchberger's algorithm
-- ---------------------------------------------------------------------------

/-- A Gröbner basis together with its monomial ordering. -/
structure GroebnerBasis where
  gbOrd : MonoOrd
  gbPolys : List (MPoly Rat)

/-- Buchberger's first criterion: coprime leading monomials → skip. -/
private def buchbergerCriterion1 (cmp : MonoOrd) (f g : MPoly Rat) : Bool :=
  match leadMonoOrd cmp f, leadMonoOrd cmp g with
  | some mf, some mg => Mono.gcd mf mg == Mono.one
  | _, _ => true

/-- Buchberger with explicit initial pair set. -/
private partial def buchbergerWithPairs (cmp : MonoOrd)
    (initialBasis : List (MPoly Rat)) (initialPairs : List (Nat × Nat))
    : List (MPoly Rat) :=
  go initialBasis initialPairs
where
  go (basis : List (MPoly Rat)) : List (Nat × Nat) → List (MPoly Rat)
    | [] => basis
    | (i, j) :: pairs =>
      if i ≥ basis.length || j ≥ basis.length then go basis pairs
      else if buchbergerCriterion1 cmp (basis.get! i) (basis.get! j) then go basis pairs
      else
        let sp := sPolynomial cmp (basis.get! i) (basis.get! j)
        let r := reducePoly cmp sp basis
        if MPoly.isZero r then go basis pairs
        else
          let k := basis.length
          let newPairs := (List.range k).map fun m => (k, m)
          let basis' := basis ++ [r]
          go basis' (pairs ++ newPairs)

/-- Core Buchberger loop. -/
private partial def buchberger (cmp : MonoOrd) (gens : List (MPoly Rat))
    : List (MPoly Rat) :=
  let n := gens.length
  let pairs := (List.range n).flatMap fun i =>
    ((List.range (n - i - 1)).map fun j => (i, i + j + 1))
  buchbergerWithPairs cmp gens pairs

/-- Inter-reduce a basis. -/
private partial def interReduce (cmp : MonoOrd) (basis : List (MPoly Rat))
    : List (MPoly Rat) :=
  go [] basis
where
  go (done : List (MPoly Rat)) : List (MPoly Rat) → List (MPoly Rat)
    | [] => done.reverse
    | f :: fs =>
      let others := done ++ fs
      let f' := reducePoly cmp f others
      if MPoly.isZero f' then go done fs
      else
        let f'' := match leadCoeffOrd cmp f' with
          | some lc => if lc != 0 then MPoly.scale (1 / lc) f' else f'
          | none => f'
        go (f'' :: done) fs

/-- Compute a Gröbner basis for the ideal generated by the given polynomials. -/
partial def groebnerBasis (cmp : MonoOrd) (gens : List (MPoly Rat)) : GroebnerBasis :=
  let nonzero := gens.filter (! MPoly.isZero ·)
  let basis := buchberger cmp nonzero
  let reduced := interReduce cmp basis
  ⟨cmp, reduced⟩

/-- Extend an existing Gröbner basis with new generators. -/
partial def extendGroebnerBasis (newGens : List (MPoly Rat))
    (gb : GroebnerBasis) : GroebnerBasis :=
  let cmp := gb.gbOrd
  let existing := gb.gbPolys
  let nonzero := newGens.filter (! MPoly.isZero ·)
  let reduced := nonzero.filterMap fun g =>
    let r := reducePoly cmp g existing
    if MPoly.isZero r then none else some r
  let combined := existing ++ reduced
  let existLen := existing.length
  let newPairs := (List.range combined.length).flatMap fun i =>
    (List.range combined.length).filterMap fun j =>
      if i < j && (i ≥ existLen || j ≥ existLen) then some (i, j) else none
  let basis := buchbergerWithPairs cmp combined newPairs
  let final := interReduce cmp basis
  ⟨cmp, final⟩

/-- Reduce a polynomial modulo the Gröbner basis. -/
def reduceGB (gb : GroebnerBasis) (f : MPoly Rat) : MPoly Rat :=
  reducePoly gb.gbOrd f gb.gbPolys

/-- Reduce completely, making the result monic if nonzero. -/
def reduceCompletelyGB (gb : GroebnerBasis) (f : MPoly Rat) : MPoly Rat :=
  let r := reduceGB gb f
  match leadCoeffOrd gb.gbOrd r with
  | some lc => if lc != 0 then MPoly.scale (1 / lc) r else r
  | none => r

end Surd
