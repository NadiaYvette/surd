/-
  Surd.Poly.Multivariate — Sparse multivariate polynomials over an
  arbitrary coefficient ring.

  Representation: association list of (Mono, coefficient) pairs.
  Monomials are products of variables raised to positive integer powers,
  represented as sorted association lists of (variable, exponent).
-/
import Surd.Poly.Univariate
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Monomials
-- ---------------------------------------------------------------------------

/-- Monomial: product of variables with positive exponents.
    Invariant: sorted by variable id, no zero exponents. -/
structure Mono where
  vars : List (Int × Int)
  deriving BEq, Inhabited

/-- The constant monomial (empty product = 1). -/
def Mono.one : Mono := ⟨[]⟩

/-- Monomial from a single variable raised to a power. -/
def Mono.ofVar (v : Int) (n : Int) : Mono :=
  if n == 0 then Mono.one else ⟨[(v, n)]⟩

/-- Multiply two monomials. -/
def Mono.mul (a b : Mono) : Mono :=
  ⟨merge a.vars b.vars⟩
where
  merge : List (Int × Int) → List (Int × Int) → List (Int × Int)
    | [], ys => ys
    | xs, [] => xs
    | (v1, e1) :: xs, (v2, e2) :: ys =>
      if v1 < v2 then (v1, e1) :: merge xs ((v2, e2) :: ys)
      else if v1 > v2 then (v2, e2) :: merge ((v1, e1) :: xs) ys
      else
        let e := e1 + e2
        if e == 0 then merge xs ys
        else (v1, e) :: merge xs ys

/-- Total degree of a monomial. -/
def Mono.degree (m : Mono) : Int :=
  m.vars.foldl (fun acc (_, e) => acc + e) 0

/-- GCD of two monomials (componentwise minimum of exponents). -/
def Mono.gcd (a b : Mono) : Mono :=
  ⟨intersect a.vars b.vars⟩
where
  intersect : List (Int × Int) → List (Int × Int) → List (Int × Int)
    | [], _ => []
    | _, [] => []
    | (v1, e1) :: xs, (v2, e2) :: ys =>
      if v1 < v2 then intersect xs ((v2, e2) :: ys)
      else if v1 > v2 then intersect ((v1, e1) :: xs) ys
      else
        let e := Int.ofNat (Nat.min e1.toNat e2.toNat)
        if e == 0 then intersect xs ys
        else (v1, e) :: intersect xs ys

/-- LCM of two monomials (componentwise maximum). -/
def Mono.lcm (a b : Mono) : Mono :=
  ⟨union a.vars b.vars⟩
where
  union : List (Int × Int) → List (Int × Int) → List (Int × Int)
    | [], ys => ys
    | xs, [] => xs
    | (v1, e1) :: xs, (v2, e2) :: ys =>
      if v1 < v2 then (v1, e1) :: union xs ((v2, e2) :: ys)
      else if v1 > v2 then (v2, e2) :: union ((v1, e1) :: xs) ys
      else (v1, Int.ofNat (Nat.max e1.toNat e2.toNat)) :: union xs ys

/-- Does the first monomial divide the second? -/
def Mono.divides (a b : Mono) : Bool :=
  go a.vars b.vars
where
  go : List (Int × Int) → List (Int × Int) → Bool
    | [], _ => true
    | _ :: _, [] => false
    | (v1, e1) :: xs, (v2, e2) :: ys =>
      if v1 < v2 then false
      else if v1 > v2 then go ((v1, e1) :: xs) ys
      else e1 ≤ e2 && go xs ys

/-- Divide monomials: a / b. Precondition: b divides a. -/
def Mono.div (a b : Mono) : Mono :=
  ⟨divide a.vars b.vars⟩
where
  divide : List (Int × Int) → List (Int × Int) → List (Int × Int)
    | xs, [] => xs
    | [], _ => []
    | (v1, e1) :: xs, (v2, e2) :: ys =>
      if v1 < v2 then (v1, e1) :: divide xs ((v2, e2) :: ys)
      else if v1 > v2 then divide ((v1, e1) :: xs) ys
      else
        let e := e1 - e2
        if e == 0 then divide xs ys
        else (v1, e) :: divide xs ys

instance : Ord Mono where
  compare a b :=
    match compare (Mono.degree a) (Mono.degree b) with
    | .lt => .lt
    | .gt => .gt
    | .eq => compareLex a.vars b.vars
where
  compareLex : List (Int × Int) → List (Int × Int) → Ordering
    | [], [] => .eq
    | [], _ :: _ => .lt
    | _ :: _, [] => .gt
    | (v1, e1) :: xs, (v2, e2) :: ys =>
      match compare v1 v2 with
      | .eq => match compare e1 e2 with
        | .eq => compareLex xs ys
        | other => other
      | other => other

/-- Compare two monomials in grevlex order. -/
def compareGrevlex (a b : Mono) : Ordering :=
  match compare (Mono.degree a) (Mono.degree b) with
  | .lt => .lt
  | .gt => .gt
  | .eq =>
    let allVars := (a.vars.map Prod.fst ++ b.vars.map Prod.fst).mergeSort (· ≥ ·)
    let lookupE (vs : List (Int × Int)) (v : Int) : Int :=
      match vs.find? (fun (v', _) => v' == v) with
      | some (_, e) => e
      | none => 0
    go allVars a.vars b.vars lookupE
where
  go : List Int → List (Int × Int) → List (Int × Int) →
       (List (Int × Int) → Int → Int) → Ordering
    | [], _, _, _ => .eq
    | v :: vs, xs, ys, lk =>
      let ea := lk xs v
      let eb := lk ys v
      if ea < eb then .gt
      else if ea > eb then .lt
      else go vs xs ys lk

-- ---------------------------------------------------------------------------
-- Multivariate polynomial
-- ---------------------------------------------------------------------------

/-- Sparse multivariate polynomial over coefficient ring k.
    Invariant: no zero coefficients. -/
structure MPoly (k : Type) where
  terms : List (Mono × k)
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Core operations (specialized to Rat for simplicity)
-- ---------------------------------------------------------------------------

/-- Remove zero coefficients. -/
def MPoly.clean (p : MPoly Rat) : MPoly Rat :=
  ⟨p.terms.filter (fun (_, c) => !(c == (0 : Rat)))⟩

/-- The zero polynomial. -/
def MPoly.zero : MPoly Rat := ⟨[]⟩

/-- Test if a polynomial is zero. -/
def MPoly.isZero (p : MPoly Rat) : Bool := p.terms.isEmpty

/-- Constant polynomial. -/
def MPoly.const (c : Rat) : MPoly Rat :=
  if c == 0 then MPoly.zero else ⟨[(Mono.one, c)]⟩

/-- The constant polynomial 1. -/
def MPoly.one : MPoly Rat := MPoly.const 1

/-- A single variable as a polynomial. -/
def MPoly.var (v : Int) : MPoly Rat :=
  ⟨[(Mono.ofVar v 1, (1 : Rat))]⟩

/-- Add two polynomials. -/
def MPoly.add (a b : MPoly Rat) : MPoly Rat :=
  let combined := a.terms ++ b.terms
  let collected := combined.foldl (fun acc (m, c) =>
    match acc.find? (fun (m', _) => m' == m) with
    | some _ => acc.map fun (m', c') => if m' == m then (m', c' + c) else (m', c')
    | none => acc ++ [(m, c)]
  ) ([] : List (Mono × Rat))
  (MPoly.mk collected).clean

/-- Negate a polynomial. -/
def MPoly.neg (p : MPoly Rat) : MPoly Rat :=
  ⟨p.terms.map fun (m, c) => (m, -c)⟩

/-- Subtract two polynomials. -/
def MPoly.sub (a b : MPoly Rat) : MPoly Rat :=
  MPoly.add a (MPoly.neg b)

/-- Multiply two polynomials. -/
def MPoly.mul (a b : MPoly Rat) : MPoly Rat :=
  let rawTerms := a.terms.flatMap fun (m1, c1) =>
    b.terms.map fun (m2, c2) => (Mono.mul m1 m2, c1 * c2)
  let collected := rawTerms.foldl (fun acc (m, c) =>
    match acc.find? (fun (m', _) => m' == m) with
    | some _ => acc.map fun (m', c') => if m' == m then (m', c' + c) else (m', c')
    | none => acc ++ [(m, c)]
  ) ([] : List (Mono × Rat))
  (MPoly.mk collected).clean

/-- Scale a polynomial by a constant. -/
def MPoly.scale (c : Rat) (p : MPoly Rat) : MPoly Rat :=
  if c == 0 then MPoly.zero
  else (MPoly.mk (p.terms.map fun (m, coeff) => (m, c * coeff))).clean

/-- Total degree. -/
def MPoly.totalDegree (p : MPoly Rat) : Int :=
  if p.terms.isEmpty then 0
  else p.terms.foldl (fun acc (m, _) =>
    let d := Mono.degree m
    if d > acc then d else acc) 0

/-- Degree in a specific variable. -/
def MPoly.degreeIn (v : Int) (p : MPoly Rat) : Int :=
  p.terms.foldl (fun acc (m, _) =>
    let e := match m.vars.find? (fun (v', _) => v' == v) with
      | some (_, e) => e
      | none => 0
    if e > acc then e else acc) 0

/-- All variable ids appearing in the polynomial. -/
def MPoly.variables (p : MPoly Rat) : List Int :=
  let allVars := p.terms.flatMap fun (m, _) => m.vars.map Prod.fst
  allVars.mergeSort (· ≤ ·) |>.eraseDups

/-- Number of terms. -/
def MPoly.numTerms (p : MPoly Rat) : Nat := p.terms.length

/-- Is this a constant polynomial? -/
def MPoly.isConst (p : MPoly Rat) : Bool :=
  p.terms.isEmpty || (p.terms.length == 1 && p.terms.head!.1.vars.isEmpty)

/-- Extract the constant coefficient. -/
def MPoly.constCoeff (p : MPoly Rat) : Rat :=
  match p.terms.find? (fun (m, _) => m.vars.isEmpty) with
  | some (_, c) => c
  | none => 0

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

instance : BEq (MPoly Rat) where
  beq a b :=
    let a' := a.terms.filter (fun (_, c) => c != (0 : Rat))
    let b' := b.terms.filter (fun (_, c) => c != (0 : Rat))
    a'.length == b'.length &&
    a'.all (fun (m, c) => match b'.find? (fun (m', _) => m' == m) with
      | some (_, c') => c == c'
      | none => false)

instance : Add (MPoly Rat) where add := MPoly.add
instance : Sub (MPoly Rat) where sub := MPoly.sub
instance : Mul (MPoly Rat) where mul := MPoly.mul
instance : Neg (MPoly Rat) where neg := MPoly.neg
instance : OfNat (MPoly Rat) 0 where ofNat := MPoly.zero
instance : OfNat (MPoly Rat) 1 where ofNat := MPoly.one
-- Div instance defined after exactDivMPoly in mutual block below

-- ---------------------------------------------------------------------------
-- Conversion to/from univariate
-- ---------------------------------------------------------------------------

/-- Convert to univariate in the given variable, with MPoly coefficients. -/
def MPoly.toUnivariate (v : Int) (p : MPoly Rat) : Poly (MPoly Rat) :=
  let groups := p.terms.foldl (fun (acc : List (Nat × MPoly Rat)) (m, c) =>
    let deg := match m.vars.find? (fun (v', _) => v' == v) with
      | some (_, e) => e.toNat
      | none => 0
    let restMono : Mono := ⟨m.vars.filter (fun (v', _) => v' != v)⟩
    let coeff : MPoly Rat := ⟨[(restMono, c)]⟩
    match acc.find? (fun (d, _) => d == deg) with
    | some _ => acc.map fun (d, c') => if d == deg then (d, MPoly.add c' coeff) else (d, c')
    | none => acc ++ [(deg, coeff)]
  ) []
  let maxDeg := groups.foldl (fun acc (d, _) => Nat.max acc d) 0
  let coeffs := (List.range (maxDeg + 1)).map fun i =>
    match groups.find? (fun (d, _) => d == i) with
    | some (_, c) => c
    | none => MPoly.zero
  ⟨coeffs.toArray⟩

/-- Convert a univariate polynomial to multivariate. -/
def MPoly.fromUnivariate (v : Int) (p : Poly Rat) : MPoly Rat :=
  let terms := (List.range p.coeffs.size).filterMap fun i =>
    let c := p.coeffs[i]!
    if c == (0 : Rat) then none
    else some (Mono.ofVar v (Int.ofNat i), c)
  ⟨terms⟩

/-- Convert univariate with MPoly coefficients back to MPoly. -/
private def fromUnivariateM (v : Int) (p : Poly (MPoly Rat)) : MPoly Rat :=
  let cs := p.coeffs.toList
  cs.enum.foldl (fun acc (i, c) =>
    if MPoly.isZero c then acc
    else
      let varPow : MPoly Rat := if i == 0 then MPoly.const 1
        else ⟨[(Mono.ofVar v (Int.ofNat i), (1 : Rat))]⟩
      MPoly.add acc (MPoly.mul c varPow)
  ) MPoly.zero

-- ---------------------------------------------------------------------------
-- Multivariate GCD over Q
-- ---------------------------------------------------------------------------

/-- Pick a variable to recurse on. -/
private def pickVar (a b : MPoly Rat) : Option Int :=
  let vs := (a.variables ++ b.variables).eraseDups
  match vs with
  | [] => none
  | v :: _ => some v

/-- Make monic: divide by the coefficient of the largest monomial. -/
private def monicMPoly (p : MPoly Rat) : MPoly Rat :=
  if p.terms.isEmpty then MPoly.zero
  else
    let lc := p.terms.getLast!.2
    if lc == 0 then MPoly.zero
    else ⟨p.terms.map fun (m, c) => (m, c / lc)⟩

-- Instances needed for Poly (MPoly Rat) operations
instance : BEq (Poly (MPoly Rat)) where
  beq a b := a.coeffs.toList == b.coeffs.toList

instance : Inhabited (MPoly Rat) where
  default := MPoly.zero

-- Mul and other instances for Poly (MPoly Rat) via manual operations
private def polyMulMPoly (a b : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  if a.coeffs.size == 0 || b.coeffs.size == 0 then ⟨#[]⟩
  else
    let n := a.coeffs.size + b.coeffs.size - 1
    let init := Array.mkArray n MPoly.zero
    let result := (List.range a.coeffs.size).foldl (fun cs i =>
      (List.range b.coeffs.size).foldl (fun cs' j =>
        let cur := cs'.getD (i + j) MPoly.zero
        cs'.set! (i + j) (MPoly.add cur (MPoly.mul a.coeffs[i]! b.coeffs[j]!))
      ) cs
    ) init
    -- Strip trailing zeros
    let sz := result.size
    let finalSz := (List.range sz).reverse.foldl (fun s i =>
      if s == i + 1 && MPoly.isZero (result.getD i MPoly.zero) then i else s
    ) sz
    ⟨result.extract 0 finalSz⟩

private def polySubMPoly (a b : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  let n := Nat.max a.coeffs.size b.coeffs.size
  let cs := (List.range n).map fun i =>
    let ai := a.coeffs.getD i MPoly.zero
    let bi := b.coeffs.getD i MPoly.zero
    MPoly.sub ai bi
  -- Strip trailing zeros
  let cs' := cs.reverse.dropWhile MPoly.isZero |>.reverse
  ⟨cs'.toArray⟩

private def polyAddMPoly (a b : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  let n := Nat.max a.coeffs.size b.coeffs.size
  let cs := (List.range n).map fun i =>
    let ai := a.coeffs.getD i MPoly.zero
    let bi := b.coeffs.getD i MPoly.zero
    MPoly.add ai bi
  let cs' := cs.reverse.dropWhile MPoly.isZero |>.reverse
  ⟨cs'.toArray⟩

private def polyScaleMPoly (c : MPoly Rat) (p : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  let cs := p.coeffs.map (MPoly.mul c)
  let cs' := cs.toList.reverse.dropWhile MPoly.isZero |>.reverse
  ⟨cs'.toArray⟩

private def polyLeadCoeffMPoly (p : Poly (MPoly Rat)) : MPoly Rat :=
  if p.coeffs.size == 0 then MPoly.zero
  else p.coeffs[p.coeffs.size - 1]!

-- ---------------------------------------------------------------------------
-- Pseudo-division and GCD
-- ---------------------------------------------------------------------------

/-- Pseudo-remainder. -/
private partial def pseudoRemPoly (f g : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  let dg := g.coeffs.size
  if f.coeffs.size < dg || dg == 0 then f
  else
    let lcG := polyLeadCoeffMPoly g
    go f (f.coeffs.size - dg + 1) lcG dg g
where
  go (r : Poly (MPoly Rat)) (e : Nat) (lcG : MPoly Rat) (dg : Nat)
      (g : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
    if e == 0 || r.coeffs.size < dg then r
    else
      let lcR := polyLeadCoeffMPoly r
      let d := r.coeffs.size - dg
      let scaledR := polyScaleMPoly lcG r
      let shiftedG : Poly (MPoly Rat) :=
        let pad := List.replicate d MPoly.zero
        let scaled := g.coeffs.toList.map (MPoly.mul lcR)
        ⟨(pad ++ scaled).toArray⟩
      let r' := polySubMPoly scaledR shiftedG
      go r' (e - 1) lcG dg g

-- Forward declarations via mutual block
mutual

/-- GCD of two multivariate polynomials over Rational. -/
partial def gcdMPoly (a b : MPoly Rat) : MPoly Rat :=
  if MPoly.isZero a then monicMPoly b
  else if MPoly.isZero b then monicMPoly a
  else
    match pickVar a b with
    | none => MPoly.const 1
    | some v =>
      let ca := contentMPoly v a
      let cb := contentMPoly v b
      let cg := gcdMPoly ca cb
      let pa := primPartMPoly v a
      let pb := primPartMPoly v b
      let ua := MPoly.toUnivariate v pa
      let ub := MPoly.toUnivariate v pb
      let ug := pseudoGcdPoly ua ub
      let g0 := fromUnivariateM v ug
      let g1 := primPartMPoly v g0
      let g2 := MPoly.mul cg g1
      monicMPoly g2

/-- GCD of all coefficients w.r.t. a variable. -/
partial def contentMPoly (v : Int) (p : MPoly Rat) : MPoly Rat :=
  let uPoly := MPoly.toUnivariate v p
  let cs := uPoly.coeffs.toList.filter (fun c => !MPoly.isZero c)
  match cs with
  | [] => MPoly.zero
  | [c] => monicMPoly c
  | c :: rest => rest.foldl gcdMPoly c

/-- Primitive part w.r.t. a variable. -/
partial def primPartMPoly (v : Int) (p : MPoly Rat) : MPoly Rat :=
  let c := contentMPoly v p
  if MPoly.isZero c then MPoly.zero
  else exactDivMPoly p c

/-- Exact division (assumes divisibility). -/
partial def exactDivMPoly (a b : MPoly Rat) : MPoly Rat :=
  if MPoly.isZero b then MPoly.zero
  else if MPoly.isConst b then
    let c := MPoly.constCoeff b
    if c == 0 then MPoly.zero
    else ⟨a.terms.map fun (m, coeff) => (m, coeff / c)⟩
  else
    match pickVar a b with
    | none =>
      let ca := MPoly.constCoeff a
      let cb := MPoly.constCoeff b
      if cb == 0 then MPoly.zero else MPoly.const (ca / cb)
    | some v =>
      let ua := MPoly.toUnivariate v a
      let ub := MPoly.toUnivariate v b
      fromUnivariateM v (divPolyMPoly ua ub)

/-- GCD via pseudo-remainder sequence. -/
partial def pseudoGcdPoly (a b : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  if a.coeffs.size == 0 then b
  else if b.coeffs.size == 0 then a
  else if a.coeffs.size < b.coeffs.size then pseudoGcdPoly b a
  else go a b
where
  go (f g : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
    if g.coeffs.size == 0 then f
    else
      let r := pseudoRemPoly f g
      if r.coeffs.size == 0 then g
      else go g (primPartUPoly r)

  primPartUPoly (p : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
    let cs := p.coeffs.toList.filter (fun c => !MPoly.isZero c)
    match cs with
    | [] => ⟨#[]⟩
    | c0 :: rest =>
      let g := rest.foldl gcdMPoly c0
      if MPoly.isZero g then p
      else ⟨(p.coeffs.toList.map (fun c => exactDivMPoly c g)).toArray⟩

/-- Polynomial long division for Poly (MPoly Rat). -/
partial def divPolyMPoly (f g : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
  if g.coeffs.size == 0 then ⟨#[]⟩
  else
    let lcG := polyLeadCoeffMPoly g
    go f ⟨#[]⟩ g.coeffs.size lcG g
where
  go (r q : Poly (MPoly Rat)) (dg : Nat) (lcG : MPoly Rat)
      (g : Poly (MPoly Rat)) : Poly (MPoly Rat) :=
    if r.coeffs.size == 0 || r.coeffs.size < dg then q
    else
      let lcR := polyLeadCoeffMPoly r
      let d := r.coeffs.size - dg
      let c := exactDivMPoly lcR lcG
      let term : Poly (MPoly Rat) :=
        ⟨((List.replicate d MPoly.zero) ++ [c]).toArray⟩
      let r' := polySubMPoly r (polyMulMPoly term g)
      go r' (polyAddMPoly q term) dg lcG g

end -- mutual

instance : Div (MPoly Rat) where div a b := exactDivMPoly a b

-- ---------------------------------------------------------------------------
-- Leading term
-- ---------------------------------------------------------------------------

/-- Leading term under graded lex order. -/
def MPoly.leadTermGrlex (p : MPoly Rat) : Option (Mono × Rat) :=
  match p.terms with
  | [] => none
  | t :: ts => some (ts.foldl (fun best cur =>
      if Mono.degree cur.1 > Mono.degree best.1 then cur
      else if Mono.degree cur.1 == Mono.degree best.1 && compare cur.1 best.1 == .gt then cur
      else best
    ) t)

/-- Leading term under grevlex order. -/
def MPoly.leadTermGrevlex (p : MPoly Rat) : Option (Mono × Rat) :=
  match p.terms with
  | [] => none
  | t :: ts => some (ts.foldl (fun best cur =>
      match compareGrevlex cur.1 best.1 with
      | .gt => cur
      | _ => best
    ) t)

/-- Evaluate a monomial by substituting values for variables. -/
def Mono.eval (env : Int → Rat) (m : Mono) : Rat :=
  m.vars.foldl (fun acc (v, e) =>
    let base := env v
    let power := (List.range e.toNat).foldl (fun p _ => p * base) (1 : Rat)
    acc * power
  ) 1

/-- Evaluate an MPoly by substituting values for variables. -/
def MPoly.eval (env : Int → Rat) (p : MPoly Rat) : Rat :=
  p.terms.foldl (fun acc (m, c) => acc + c * Mono.eval env m) 0

end Surd
