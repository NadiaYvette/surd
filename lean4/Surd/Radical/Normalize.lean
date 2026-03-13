/-
  Surd.Radical.Normalize — Normalization passes for radical expressions.

  normalize = fixN 10 (collectTerms ∘ collectCoefficients ∘ distribute ∘
    sortCommutative ∘ extractPerfectPowers ∘ simplifyPowers ∘ foldConstants ∘ flattenArith)
-/
import Surd.Radical.Expr
import Surd.Rat
import Surd.PrimeFactors
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Apply f until fixed point or fuel runs out. -/
def fixN {α : Type} [BEq α] (fuel : Nat) (f : α → α) (x : α) : α :=
  match fuel with
  | 0 => x
  | n + 1 =>
    let x' := f x
    if x' == x then x else fixN n f x'

/-- Flatten nested Add/Mul and eliminate double negations. -/
def flattenArith : RadExpr Rat → RadExpr Rat
  | .neg (.neg a) => flattenArith a
  | .neg a => .neg (flattenArith a)
  | .add a b =>
    let a' := flattenArith a
    let b' := flattenArith b
    -- Flatten nested adds
    let as_ := flattenAdd a'
    let bs := flattenAdd b'
    rebuildAdd (as_ ++ bs)
  | .mul a b =>
    let a' := flattenArith a
    let b' := flattenArith b
    let as_ := flattenMul a'
    let bs := flattenMul b'
    rebuildMul (as_ ++ bs)
  | .inv a => .inv (flattenArith a)
  | .root n a => .root n (flattenArith a)
  | .pow a n => .pow (flattenArith a) n
  | e => e
where
  flattenAdd : RadExpr Rat → List (RadExpr Rat)
    | .add a b => flattenAdd a ++ flattenAdd b
    | e => [e]
  flattenMul : RadExpr Rat → List (RadExpr Rat)
    | .mul a b => flattenMul a ++ flattenMul b
    | e => [e]
  rebuildAdd : List (RadExpr Rat) → RadExpr Rat
    | [] => .lit 0
    | [x] => x
    | x :: xs => .add x (rebuildAdd xs)
  rebuildMul : List (RadExpr Rat) → RadExpr Rat
    | [] => .lit 1
    | [x] => x
    | x :: xs => .mul x (rebuildMul xs)

/-- Fold constant subexpressions. -/
def foldConstants : RadExpr Rat → RadExpr Rat
  | .neg (.lit r) => .lit (-r)
  | .neg a => .neg (foldConstants a)
  | .add (.lit a) (.lit b) => .lit (a + b)
  | .add a b => .add (foldConstants a) (foldConstants b)
  | .mul (.lit a) (.lit b) => .lit (a * b)
  | .mul (.lit 0) _ => .lit 0
  | .mul _ (.lit 0) => .lit 0
  | .mul (.lit 1) b => foldConstants b
  | .mul a (.lit 1) => foldConstants a
  | .mul a b => .mul (foldConstants a) (foldConstants b)
  | .inv (.lit r) => if r == 0 then .inv (.lit 0) else .lit (1 / r)
  | .inv a => .inv (foldConstants a)
  | .root _ (.lit 1) => .lit 1
  | .root _ (.lit 0) => .lit 0
  | .root n a => .root n (foldConstants a)
  | .pow (.lit r) n =>
    if n ≥ 0 then .lit (ratPow r n.toNat)
    else if r == 0 then .inv (.lit 0)
    else .lit (1 / ratPow r (-n).toNat)
  | .pow a n => .pow (foldConstants a) n
  | e => e
where
  ratPow (r : Rat) : Nat → Rat
    | 0 => 1
    | 1 => r
    | n + 1 => r * ratPow r n

/-- Simplify powers: (√a)² → a, (ⁿ√a)ⁿ → a, nested roots. -/
def simplifyPowers : RadExpr Rat → RadExpr Rat
  | .pow (.root n a) m =>
    if n == m then simplifyPowers a
    else .pow (.root n (simplifyPowers a)) m
  | .root n (.root m a) =>
    -- ⁿ√(ᵐ√a) = ⁿᵐ√a
    .root (n * m) (simplifyPowers a)
  | .pow _ 0 => .lit 1
  | .pow a 1 => simplifyPowers a
  | .neg a => .neg (simplifyPowers a)
  | .add a b => .add (simplifyPowers a) (simplifyPowers b)
  | .mul a b => .mul (simplifyPowers a) (simplifyPowers b)
  | .inv a => .inv (simplifyPowers a)
  | .root n a => .root n (simplifyPowers a)
  | .pow a n => .pow (simplifyPowers a) n
  | e => e

/-- Extract perfect nth powers from radicals.
    √12 → 2√3, ∛(8·x) → 2·∛x, etc. -/
def extractPerfectPowers : RadExpr Rat → RadExpr Rat
  | .root n (.lit r) =>
    if n ≤ 1 || r == 0 then .root n (.lit r)
    else extractFromRat n r
  | .neg a => .neg (extractPerfectPowers a)
  | .add a b => .add (extractPerfectPowers a) (extractPerfectPowers b)
  | .mul a b => .mul (extractPerfectPowers a) (extractPerfectPowers b)
  | .inv a => .inv (extractPerfectPowers a)
  | .root n a => .root n (extractPerfectPowers a)
  | .pow a k => .pow (extractPerfectPowers a) k
  | e => e
where
  extractFromRat (n : Int) (r : Rat) : RadExpr Rat :=
    let neg := r.num < 0 && n % 2 == 1
    let absR := if neg then Rat'.abs r else r
    let nn := n.toNat
    let (numOut, numIn) := extractNthPower nn absR.num.natAbs
    let (denOut, denIn) := extractNthPower nn absR.den
    let outside : Rat := (Int.ofNat numOut : Int) / (Int.ofNat denOut : Int)
    let sign : Int := if absR.num < 0 then -1 else 1
    let inside : Rat := (sign * Int.ofNat numIn : Int) / (Int.ofNat denIn : Int)
    let result :=
      if outside == 1 then .root n (.lit inside)
      else if inside == 1 then .lit outside
      else .mul (.lit outside) (.root n (.lit inside))
    if neg then .neg result else result
  extractNthPower (n : Nat) (m : Nat) : Nat × Nat :=
    if m ≤ 1 then (1, m)
    else
      let facts := factoriseNat m
      let outside := facts.foldl (fun acc (p, e) => acc * p ^ (e / n)) 1
      let inside := facts.foldl (fun acc (p, e) => acc * p ^ (e % n)) 1
      (outside, inside)

/-- Sort commutative operations (Add/Mul children) into canonical order. -/
partial def sortCommutative : RadExpr Rat → RadExpr Rat
  | .add a b =>
    let a' := sortCommutative a
    let b' := sortCommutative b
    let terms := flattenAdd (.add a' b') |>.mergeSort (compareExpr · · |>.isLE)
    rebuildAdd terms
  | .mul a b =>
    let a' := sortCommutative a
    let b' := sortCommutative b
    let terms := flattenMul (.mul a' b') |>.mergeSort (compareExpr · · |>.isLE)
    rebuildMul terms
  | .neg a => .neg (sortCommutative a)
  | .inv a => .inv (sortCommutative a)
  | .root n a => .root n (sortCommutative a)
  | .pow a n => .pow (sortCommutative a) n
  | e => e
where
  flattenAdd : RadExpr Rat → List (RadExpr Rat)
    | .add a b => flattenAdd a ++ flattenAdd b
    | e => [e]
  flattenMul : RadExpr Rat → List (RadExpr Rat)
    | .mul a b => flattenMul a ++ flattenMul b
    | e => [e]
  rebuildAdd : List (RadExpr Rat) → RadExpr Rat
    | [] => .lit 0
    | [x] => x
    | x :: xs => .add x (rebuildAdd xs)
  rebuildMul : List (RadExpr Rat) → RadExpr Rat
    | [] => .lit 1
    | [x] => x
    | x :: xs => .mul x (rebuildMul xs)
  compareExpr (a b : RadExpr Rat) : Ordering :=
    RadExpr.compare a b

/-- Distribute scalar multiplication over sums:
    c * (a + b) → c*a + c*b when c is a literal. -/
def distribute : RadExpr Rat → RadExpr Rat
  | .mul (.lit c) (.add a b) =>
    .add (.mul (.lit c) (distribute a)) (.mul (.lit c) (distribute b))
  | .mul (.add a b) (.lit c) =>
    .add (.mul (distribute a) (.lit c)) (.mul (distribute b) (.lit c))
  | .neg a => .neg (distribute a)
  | .add a b => .add (distribute a) (distribute b)
  | .mul a b => .mul (distribute a) (distribute b)
  | .inv a => .inv (distribute a)
  | .root n a => .root n (distribute a)
  | .pow a n => .pow (distribute a) n
  | e => e

/-- Collect like terms in sums: 3√5 + 2√5 → 5√5. -/
partial def collectTerms : RadExpr Rat → RadExpr Rat
  | .add a b =>
    let a' := collectTerms a
    let b' := collectTerms b
    let terms := flattenAdd (.add a' b')
    let grouped := groupByBase terms
    let merged := grouped.map fun (base, coeff) =>
      if coeff == 1 then base
      else if coeff == 0 then .lit 0
      else .mul (.lit coeff) base
    let filtered := merged.filter fun e => e != .lit 0
    rebuildAdd filtered
  | .neg a => .neg (collectTerms a)
  | .mul a b => .mul (collectTerms a) (collectTerms b)
  | .inv a => .inv (collectTerms a)
  | .root n a => .root n (collectTerms a)
  | .pow a n => .pow (collectTerms a) n
  | e => e
where
  flattenAdd : RadExpr Rat → List (RadExpr Rat)
    | .add a b => flattenAdd a ++ flattenAdd b
    | e => [e]
  rebuildAdd : List (RadExpr Rat) → RadExpr Rat
    | [] => .lit 0
    | [x] => x
    | x :: xs => .add x (rebuildAdd xs)
  extractCoeffBase : RadExpr Rat → Rat × RadExpr Rat
    | .lit r => (r, .lit 1)
    | .neg e =>
      let (c, b) := extractCoeffBase e
      (-c, b)
    | .mul (.lit c) e => (c, e)
    | .mul e (.lit c) => (c, e)
    | e => (1, e)
  groupByBase (terms : List (RadExpr Rat)) : List (RadExpr Rat × Rat) :=
    terms.foldl (fun acc term =>
      let (c, base) := extractCoeffBase term
      match acc.find? (fun (b, _) => b == base) with
      | some _ => acc.map fun (b, c') => if b == base then (b, c' + c) else (b, c')
      | none => acc ++ [(base, c)]
    ) []

/-- Collect coefficients in products: merge all literal factors. -/
def collectCoefficients : RadExpr Rat → RadExpr Rat
  | .mul a b =>
    let a' := collectCoefficients a
    let b' := collectCoefficients b
    let factors := flattenMul (.mul a' b')
    let (lits, rest) := factors.partition isLit
    let coeff := lits.foldl (fun acc e =>
      match e with | .lit r => acc * r | _ => acc) (1 : Rat)
    let body := rebuildMul rest
    if coeff == 1 then body
    else if coeff == 0 then .lit 0
    else .mul (.lit coeff) body
  | .neg a => .neg (collectCoefficients a)
  | .add a b => .add (collectCoefficients a) (collectCoefficients b)
  | .inv a => .inv (collectCoefficients a)
  | .root n a => .root n (collectCoefficients a)
  | .pow a n => .pow (collectCoefficients a) n
  | e => e
where
  isLit : RadExpr Rat → Bool
    | .lit _ => true
    | _ => false
  flattenMul : RadExpr Rat → List (RadExpr Rat)
    | .mul a b => flattenMul a ++ flattenMul b
    | e => [e]
  rebuildMul : List (RadExpr Rat) → RadExpr Rat
    | [] => .lit 1
    | [x] => x
    | x :: xs => .mul x (rebuildMul xs)

/-- Single normalization pass: all sub-passes composed. -/
def normalizeOnce (e : RadExpr Rat) : RadExpr Rat :=
  e |> flattenArith
    |> foldConstants
    |> simplifyPowers
    |> extractPerfectPowers
    |> sortCommutative
    |> distribute
    |> collectCoefficients
    |> collectTerms

/-- Full normalization: apply normalizeOnce to fixed point (max 10 iterations). -/
def normalize (e : RadExpr Rat) : RadExpr Rat :=
  fixN 10 normalizeOnce e

end Surd
