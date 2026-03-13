/-
  Surd.Radical.NormalForm — Normal form for radical expressions:
  Q-linear combinations of products of radical atoms.

  Every expression in Q[√2, √3, ∛5, ...] has a unique representation
  as a sum of (rational coefficient × monomial), where each monomial
  is a product of radical atoms raised to bounded powers.
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Rat
import Surd.PrimeFactors
import Surd.Interval
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ==========================================================================
-- Sorted association list utilities
-- ==========================================================================

namespace SortedMap

variable {K V W : Type}

def insert [Ord K] (k : K) (v : V) : List (K × V) → List (K × V)
  | [] => [(k, v)]
  | (k', v') :: rest =>
    match Ord.compare k k' with
    | .lt => (k, v) :: (k', v') :: rest
    | .eq => (k, v) :: rest
    | .gt => (k', v') :: insert k v rest

def find? [Ord K] (k : K) : List (K × V) → Option V
  | [] => none
  | (k', v) :: rest =>
    match Ord.compare k k' with
    | .lt => none
    | .eq => some v
    | .gt => find? k rest

def erase [Ord K] (k : K) : List (K × V) → List (K × V)
  | [] => []
  | (k', v') :: rest =>
    match Ord.compare k k' with
    | .lt => (k', v') :: rest
    | .eq => rest
    | .gt => (k', v') :: erase k rest

def unionWith [Ord K] (f : V → V → V) : List (K × V) → List (K × V) → List (K × V)
  | [], bs => bs
  | as, [] => as
  | (ka, va) :: as, (kb, vb) :: bs =>
    match Ord.compare ka kb with
    | .lt => (ka, va) :: unionWith f as ((kb, vb) :: bs)
    | .eq => (ka, f va vb) :: unionWith f as bs
    | .gt => (kb, vb) :: unionWith f ((ka, va) :: as) bs

def mapVal (f : V → W) : List (K × V) → List (K × W)
  | [] => []
  | (k, v) :: rest => (k, f v) :: mapVal f rest

def filterVal (p : V → Bool) : List (K × V) → List (K × V)
  | [] => []
  | (k, v) :: rest => if p v then (k, v) :: filterVal p rest else filterVal p rest

end SortedMap

-- ==========================================================================
-- Types
-- ==========================================================================

inductive Atom where
  | ratRoot : Int → Rat → Atom
  | imagUnit : Atom
  | nestedRoot : Int → RadExpr Rat → Atom
  deriving Repr, BEq, Inhabited

protected def Atom.compare : Atom → Atom → Ordering
  | .imagUnit, .imagUnit => .eq
  | .imagUnit, _ => .lt
  | _, .imagUnit => .gt
  | .ratRoot n1 r1, .ratRoot n2 r2 =>
    match Ord.compare n1 n2 with | .eq => Ord.compare r1 r2 | o => o
  | .ratRoot _ _, .nestedRoot _ _ => .lt
  | .nestedRoot _ _, .ratRoot _ _ => .gt
  | .nestedRoot n1 e1, .nestedRoot n2 e2 =>
    match Ord.compare n1 n2 with | .eq => RadExpr.compare e1 e2 | o => o

instance : Ord Atom where compare := Atom.compare

structure Monomial where atoms : List (Atom × Int) deriving Repr, BEq

protected def Monomial.compare (a b : Monomial) : Ordering :=
  go a.atoms b.atoms
where
  go : List (Atom × Int) → List (Atom × Int) → Ordering
    | [], [] => .eq | [], _ :: _ => .lt | _ :: _, [] => .gt
    | (a1, e1) :: r1, (a2, e2) :: r2 =>
      match Atom.compare a1 a2 with
      | .eq => match Ord.compare e1 e2 with | .eq => go r1 r2 | o => o
      | o => o

instance : Ord Monomial where compare := Monomial.compare
instance : Inhabited Monomial where default := ⟨[]⟩

structure NormExpr where terms : List (Monomial × Rat) deriving Repr, BEq
instance : Inhabited NormExpr where default := ⟨[]⟩

-- ==========================================================================
-- Non-recursive operations
-- ==========================================================================

def unitMono : Monomial := ⟨[]⟩
def normZero : NormExpr := ⟨[]⟩
def normLit (r : Rat) : NormExpr := if r == 0 then normZero else ⟨[(unitMono, r)]⟩
def normAtom (a : Atom) : NormExpr := ⟨[(⟨[(a, 1)]⟩, 1)]⟩
def normIsZero (ne : NormExpr) : Bool := ne.terms.isEmpty
def normCoeff (ne : NormExpr) : Option Rat :=
  match ne.terms with | [] => some 0 | [(⟨[]⟩, c)] => some c | _ => none
def normAdd (a b : NormExpr) : NormExpr :=
  ⟨SortedMap.filterVal (· != 0) (SortedMap.unionWith (· + ·) a.terms b.terms)⟩
def normNeg (ne : NormExpr) : NormExpr := ⟨SortedMap.mapVal (fun c => -c) ne.terms⟩
def normSub (a b : NormExpr) : NormExpr := normAdd a (normNeg b)
def normScale (c : Rat) (ne : NormExpr) : NormExpr :=
  if c == 0 then normZero
  else ⟨SortedMap.filterVal (· != 0) (SortedMap.mapVal (· * c) ne.terms)⟩

private def ratPow (r : Rat) : Nat → Rat
  | 0 => 1 | 1 => r | n + 1 => r * ratPow r n

private def extractNthPowerInt (n : Int) (m : Int) : Rat × Rat :=
  if m == 0 then (0, 0) else if m == 1 || m == -1 then (1, m) else
  let absM := m.natAbs
  let facts := factoriseNat absM
  let outside := facts.foldl (fun acc (p, e) => acc * p ^ (e / n.toNat)) 1
  let inside := facts.foldl (fun acc (p, e) => acc * p ^ (e % n.toNat)) 1
  let sign : Int := if m < 0 then -1 else 1
  ((Int.ofNat outside : Int), (sign * Int.ofNat inside : Int))

private def reduceAtom (c : Rat) (m : List (Atom × Int)) (atom : Atom) (e : Int) :
    Rat × List (Atom × Int) :=
  match atom with
  | .imagUnit =>
    let e' := e % 4
    let (signFactor, hasI) := match e' with
      | 0 => ((1 : Int), false) | 1 => ((1 : Int), true)
      | 2 => ((-1 : Int), false) | 3 => ((-1 : Int), true)
      | _ => ((1 : Int), false)
    (c * signFactor, if hasI then SortedMap.insert .imagUnit 1 m else m)
  | .ratRoot n r =>
    let (full, rem') := (e / n, e % n)
    (c * ratPow r full.toNat, if rem' == 0 then m else SortedMap.insert atom rem' m)
  | .nestedRoot _ _ =>
    if e == 0 then (c, m) else (c, SortedMap.insert atom e m)

-- Non-mutual helpers
private def findRadicalAtom (ne : NormExpr) : Option Atom :=
  ne.terms.findSome? fun (mono, _) => mono.atoms.head?.map (·.1)

private def atomDegree : Atom → Int
  | .imagUnit => 2 | .ratRoot n _ => n | .nestedRoot n _ => n

private def extractAtomPower (atom : Atom) (mono : Monomial) : Int × Monomial :=
  match SortedMap.find? atom mono.atoms with
  | none => (0, mono) | some e => (e, ⟨SortedMap.erase atom mono.atoms⟩)

private def atomPow (a : Atom) (k : Int) : NormExpr :=
  if k == 0 then normLit 1 else ⟨[(⟨[(a, k)]⟩, 1)]⟩

-- NormExpr polynomial helpers (no dependency on mutual defs)
private def normPolyIsZero (p : List NormExpr) : Bool := p.all normIsZero
private def normPolyDeg (p : List NormExpr) : Int :=
  p.enum.foldl (fun d (i, x) => if normIsZero x then d else i) (-1)
private def normPolyLeadCoeff (p : List NormExpr) : Option NormExpr :=
  let d := normPolyDeg p; if d < 0 then none else p.get? d.toNat
private def normPolyTrim (p : List NormExpr) : List NormExpr :=
  p.reverse.dropWhile normIsZero |>.reverse
private def normPolyAdd : List NormExpr → List NormExpr → List NormExpr
  | [], bs => bs | as, [] => as
  | x :: xs, y :: ys => normAdd x y :: normPolyAdd xs ys
private def normPolySub (a b : List NormExpr) : List NormExpr :=
  normPolyAdd a (b.map normNeg)
private def normPolySetCoeff (i : Nat) (c : NormExpr) (xs : List NormExpr) : List NormExpr :=
  let padded := xs ++ List.replicate (i + 1 - xs.length) normZero
  padded.take i ++ [c] ++ padded.drop (i + 1)

-- ==========================================================================
-- Conversion helpers (needed by mutual block, no mutual deps)
-- ==========================================================================

private def atomToExpr (a : Atom) (e : Int) : RadExpr Rat :=
  match a, e with
  | _, 0 => .lit 1
  | .imagUnit, 1 => .root 2 (.lit (-1))
  | .imagUnit, _ => .pow (.root 2 (.lit (-1))) e
  | .ratRoot n r, 1 => .root n (.lit r)
  | .ratRoot n r, _ => .pow (.root n (.lit r)) e
  | .nestedRoot n inner, 1 => .root n inner
  | .nestedRoot n inner, _ => .pow (.root n inner) e

private def applyCoeff (c : Rat) (body : RadExpr Rat) : RadExpr Rat :=
  if c == 1 then body else if c == -1 then .neg body
  else match body with | .lit 1 => .lit c | _ => .mul (.lit c) body

private def buildProd : List (RadExpr Rat) → RadExpr Rat
  | [] => .lit 1 | [x] => x | x :: xs => xs.foldl .mul x

private def buildSum : List (RadExpr Rat) → RadExpr Rat
  | [] => .lit 0 | [x] => x | x :: xs => xs.foldl .add x

def fromNormExpr (ne : NormExpr) : RadExpr Rat :=
  match ne.terms with
  | [] => .lit 0
  | terms => buildSum (terms.map fun (⟨atoms⟩, c) =>
      applyCoeff c (buildProd (atoms.map fun (a, e) => atomToExpr a e)))

-- ==========================================================================
-- Mutual block: all recursively interdependent functions
-- ==========================================================================

mutual

partial def reduceNestedRoots (coeff : Rat) (atoms : List (Atom × Int)) : NormExpr :=
  match atoms.find? (fun (a, e) => match a with
    | .nestedRoot n _ => decide (e ≥ n) | _ => false) with
  | none => ⟨[(⟨atoms⟩, coeff)]⟩
  | some (.nestedRoot n inner, e) =>
    let (full, rem') := (e / n, e % n)
    let atoms' := if rem' == 0
      then SortedMap.erase (.nestedRoot n inner) atoms
      else SortedMap.insert (.nestedRoot n inner) rem' atoms
    normMul ⟨[(⟨atoms'⟩, coeff)]⟩ (normPow (toNormExpr inner) full.toNat)
  | _ => ⟨[(⟨atoms⟩, coeff)]⟩

partial def reduceMonomial (mono : Monomial) (coeff : Rat) : NormExpr :=
  let (coeff', atoms') := mono.atoms.foldl
    (fun (c, m) (atom, e) => reduceAtom c m atom e) (coeff, [])
  if coeff' == 0 then normZero else reduceNestedRoots coeff' atoms'

partial def normMul (a b : NormExpr) : NormExpr :=
  (a.terms.flatMap fun (m1, c1) =>
    b.terms.map fun (m2, c2) =>
      reduceMonomial ⟨SortedMap.unionWith (· + ·) m1.atoms m2.atoms⟩ (c1 * c2)
  ).foldl normAdd normZero

partial def normPow (e : NormExpr) : Nat → NormExpr
  | 0 => normLit 1 | 1 => e
  | n => if n % 2 == 0 then let h := normPow e (n / 2); normMul h h
         else normMul e (normPow e (n - 1))

partial def rootOfAtomPow (n : Int) (atom : Atom) (e : Int) : NormExpr :=
  match atom with
  | .ratRoot m r => reduceMonomial ⟨[(.ratRoot (m * n) r, e)]⟩ 1
  | .nestedRoot m inner => reduceMonomial ⟨[(.nestedRoot (m * n) inner, e)]⟩ 1
  | .imagUnit =>
    let e' := e % 4
    match e' with
    | 0 => normLit 1 | 2 => normRoot n (-1)
    | _ => let base := normRoot (2 * n) (-1)
           if e' == 1 then base else normNeg base

partial def normRoot (n : Int) (r : Rat) : NormExpr :=
  if r == 0 then normZero
  else if r == 1 then normLit 1
  else if r < 0 && n == 2 then normMul (normAtom .imagUnit) (normRoot 2 (-r))
  else if r < 0 && n % 2 == 0 then normMul (normAtom .imagUnit) (normRoot n (-r))
  else if r < 0 && n % 2 == 1 then normNeg (normRoot n (-r))
  else
    let (numOut, numIn) := extractNthPowerInt n r.num.natAbs
    let (denOut, denIn) := extractNthPowerInt n r.den
    let coeff : Rat := numOut / (denOut * denIn)
    let radicandNum := numIn * ratPow denIn (n.toNat - 1)
    if radicandNum == 1 then normScale coeff (normLit 1)
    else normScale coeff (normAtom (.ratRoot n radicandNum))

partial def invertMonomial (mono : Monomial) : NormExpr :=
  reduceMonomial ⟨SortedMap.mapVal (fun e => -e) mono.atoms⟩ 1

partial def normInv (ne : NormExpr) : NormExpr :=
  match ne.terms with
  | [] => normZero
  | [(mono, c)] => normScale (1 / c) (invertMonomial mono)
  | _ => rationalizeInv ne

partial def rationalizeInv (ne : NormExpr) : NormExpr :=
  match findRadicalAtom ne with
  | none => match normCoeff ne with
    | some r => if r != 0 then normLit (1 / r) else normZero
    | none => normZero
  | some atom =>
    let n := atomDegree atom
    let polyCoeffs := toAtomPoly atom ne n
    let mp := minPolyCoeffs n atom
    let (_, inv, _) := normPolyExtGcd polyCoeffs mp
    fromAtomPoly atom inv

partial def toNormExpr : RadExpr Rat → NormExpr
  | .lit r => normLit r
  | .neg a => normNeg (toNormExpr a)
  | .add a b => normAdd (toNormExpr a) (toNormExpr b)
  | .mul a b => normMul (toNormExpr a) (toNormExpr b)
  | .root n (.lit r) => normRoot n r
  | .root n a =>
    let inner := toNormExpr a
    match normCoeff inner with
    | some r => normRoot n r
    | none => match inner.terms with
      | [(⟨atoms⟩, c)] =>
        normMul (normRoot n c) (atoms.foldl (fun acc (atom, e) =>
          normMul acc (rootOfAtomPow n atom e)) (normLit 1))
      | _ => normAtom (.nestedRoot n (fromNormExpr inner))
  | .inv a => normInv (toNormExpr a)
  | .pow a n =>
    if n ≥ 0 then normPow (toNormExpr a) n.toNat
    else normInv (normPow (toNormExpr a) (-n).toNat)

-- Helpers that depend on mutual defs

partial def toAtomPoly (atom : Atom) (ne : NormExpr) (n : Int) : List NormExpr :=
  let grouped := ne.terms.foldl (fun (acc : List (Nat × NormExpr)) (mono, c) =>
    let (power, rest) := extractAtomPower atom mono
    let idx := (power % n).toNat
    match acc.find? (fun (i, _) => i == idx) with
    | some _ => acc.map fun (i, ne') =>
        if i == idx then (i, normAdd ne' ⟨[(rest, c)]⟩) else (i, ne')
    | none => acc ++ [(idx, ⟨[(rest, c)]⟩)]
  ) []
  List.range n.toNat |>.map fun i =>
    match grouped.find? (fun (j, _) => j == i) with
    | some (_, ne') => ne' | none => normZero

partial def minPolyCoeffs (n : Int) (atom : Atom) : List NormExpr :=
  match atom with
  | .imagUnit => [normLit 1, normZero, normLit 1]
  | .ratRoot _ r =>
    [normLit (-r)] ++ List.replicate (n.toNat - 1) normZero ++ [normLit 1]
  | .nestedRoot _ inner =>
    [normNeg (toNormExpr inner)] ++ List.replicate (n.toNat - 1) normZero ++ [normLit 1]

partial def fromAtomPoly (atom : Atom) (coeffs : List NormExpr) : NormExpr :=
  coeffs.enum.foldl (fun acc (i, c) =>
    normAdd acc (normMul c (atomPow atom i))) normZero

partial def normPolyScale (c : NormExpr) (p : List NormExpr) : List NormExpr :=
  p.map (normMul c)

partial def normPolyMul (a b : List NormExpr) : List NormExpr :=
  match a, b with
  | [], _ | _, [] => []
  | _, _ => a.enum.foldl (fun acc (i, ai) =>
      normPolyAdd acc (List.replicate i normZero ++ b.map (normMul ai))) []

partial def normPolyExtGcd (a b : List NormExpr) :
    List NormExpr × List NormExpr × List NormExpr :=
  go a b [normLit 1] [normZero] [normZero] [normLit 1]
where
  go (r0 r1 s0 s1 t0 t1 : List NormExpr) :
      List NormExpr × List NormExpr × List NormExpr :=
    if normPolyIsZero r1 then
      match normPolyLeadCoeff r0 with
      | none => ([normLit 1], s0, t0)
      | some lc =>
        let lcInv := normInv lc
        (normPolyScale lcInv r0, normPolyScale lcInv s0, normPolyScale lcInv t0)
    else
      let (q, r) := normPolyDivMod r0 r1
      go r1 r s1 (normPolySub s0 (normPolyMul q s1))
             t1 (normPolySub t0 (normPolyMul q t1))

partial def normPolyDivMod (num den : List NormExpr) : List NormExpr × List NormExpr :=
  if normPolyIsZero den then ([], num)
  else
    let degNum := normPolyDeg num
    let degDen := normPolyDeg den
    if degNum < degDen then ([], num)
    else
      let lcDenInv := match normPolyLeadCoeff den with
        | some c => normInv c | none => normZero
      divLoop degDen lcDenInv
        (List.replicate (degNum.toNat - degDen.toNat + 1) normZero) num den
where
  divLoop (degDen : Int) (lcDenInv : NormExpr) (q r den : List NormExpr) :
      List NormExpr × List NormExpr :=
    if normPolyIsZero r || normPolyDeg r < degDen then
      (normPolyTrim q, normPolyTrim r)
    else match normPolyLeadCoeff r with
      | none => (normPolyTrim q, normPolyTrim r)
      | some lcR =>
        let coeff := normMul lcR lcDenInv
        let shift := (normPolyDeg r - degDen).toNat
        let q' := normPolySetCoeff shift coeff q
        let sub := List.replicate shift normZero ++ [coeff]
        let r' := normPolyTrim (normPolySub r (normPolyMul sub den))
        divLoop degDen lcDenInv q' r' den

end -- mutual

end Surd
