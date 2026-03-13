/-
  Surd.Radical.Denest.NthRoot — General nth-root denesting.

  Implements:
  1. Simple nth-root simplification: ⁿ√(aⁿ·b) = a·ⁿ√b
  2. Root-index reduction: ⁿ√(x^k) simplification
  3. Nested root collapse: ᵐ√(ⁿ√x) = ᵐⁿ√x
  4. Cube root denesting for the form ³√(a + b√c)

  Reference: Landau 1992, Zippel 1985
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.PrimeFactors
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Integer square root: returns some s if n = s², none otherwise. -/
private partial def exactSqrtInt (n : Int) : Option Int :=
  if n < 0 then none
  else if n == 0 then some 0
  else if n == 1 then some 1
  else
    let nNat := n.toNat
    let s := go nNat nNat
    if s * s == nNat then some (Int.ofNat s) else none
where
  go (x n : Nat) : Nat :=
    let x' := (x + n / x) / 2
    if x' ≥ x then x else go x' n

/-- Integer cube root: returns some s if n = s³, none otherwise. -/
private partial def exactCbrtInt (n : Int) : Option Int :=
  if n == 0 then some 0
  else
    let absN := n.natAbs
    let s := go absN absN
    if s * s * s == absN then
      if n < 0 then some (-(Int.ofNat s)) else some (Int.ofNat s)
    else none
where
  go (x n : Nat) : Nat :=
    let x' := (2 * x + n / (x * x)) / 3
    if x' ≥ x then x else go x' n

/-- Check if a rational is a perfect square. -/
private def isRationalSqrt (q : Rat) : Option Rat :=
  if q < 0 then none
  else if q == 0 then some 0
  else
    match exactSqrtInt q.num, exactSqrtInt (Int.ofNat q.den) with
    | some sn, some sd => if sd != 0 then some (sn / sd : Rat) else none
    | _, _ => none

/-- Check if a rational is a perfect cube. -/
private def isRationalCubeRoot (q : Rat) : Option Rat :=
  if q == 0 then some 0
  else
    match exactCbrtInt q.num, exactCbrtInt (Int.ofNat q.den) with
    | some sn, some sd => if sd != 0 then some (sn / sd : Rat) else none
    | _, _ => none

/-- Extract the largest perfect nth power factor from a natural number.
    Returns (extracted, remainder) such that m = extracted^n * remainder. -/
private def extractPower (n : Int) (m : Int) : Int × Int :=
  if m == 0 then (0, 0)
  else if n ≤ 0 then (1, m)
  else
    let absM := m.natAbs
    let fs := factoriseNat absM
    let nNat := n.toNat
    let extracted := fs.foldl (fun acc (p, e) =>
      acc * (Int.ofNat p) ^ (e / nNat)) (1 : Int)
    let remainder := fs.foldl (fun acc (p, e) =>
      acc * (Int.ofNat p) ^ (e % nNat)) (1 : Int)
    let sign : Int := if m < 0 then -1 else 1
    (extracted, sign * remainder)

/-- Simplify ⁿ√r for rational r by extracting perfect nth powers. -/
private def simplifyRootOfRational (n : Int) (r : Rat) : RadExpr Rat :=
  let num := r.num
  let den := (Int.ofNat r.den : Int)
  let (numOut, numRem) := extractPower n num
  let (denOut, denRem) := extractPower n den
  if numOut == 1 && denOut == 1 then .root n (.lit r)
  else
    let coeff : Rat := numOut / denOut
    let remRat : Rat := numRem / denRem
    .mul (.lit coeff) (.root n (.lit remRat))

/-- Divisors of a non-negative integer. -/
private def intDivisors (n : Int) : List Int :=
  if n == 0 then [1]
  else
    let absN := n.natAbs
    (List.range absN).filterMap fun d =>
      let d' := d + 1
      if absN % d' == 0 then some (Int.ofNat d') else none

/-- Find rational roots of ax³ + bx + c = 0 (no x² term). -/
private def rationalCubeEqRoots (a b c : Rat) : List Rat :=
  let f (x : Rat) : Rat := a * x * x * x + b * x + c
  let numC := Rat'.abs c |>.num |> Int.natAbs
  let denC := Rat'.abs c |>.den
  let numA := Rat'.abs a |>.num |> Int.natAbs
  let denA := Rat'.abs a |>.den
  let pDivisors := intDivisors (Int.ofNat (numC * denA + 1))  -- +1 ensures 1 is included
  let qDivisors := intDivisors (Int.ofNat (numA * denC + 1))
  -- Generate candidates
  let candidates : List Rat := pDivisors.flatMap fun p =>
    qDivisors.flatMap fun q =>
      if q != 0 then [p / q, -(p / q)] else []
  -- Also add 0 and small simple fractions
  let candidates := (0 : Rat) :: candidates
  candidates.filter fun x => f x == 0

/-- Match the pattern a + b√c in an expression (for cube root denesting). -/
private def matchSqrtNested3 : RadExpr Rat → Option (Rat × Rat × Rat)
  | .add (.lit a) (.mul (.lit b) (.root 2 (.lit c))) => some (a, b, c)
  | .add (.mul (.lit b) (.root 2 (.lit c))) (.lit a) => some (a, b, c)
  | .add (.lit a) (.root 2 (.lit c)) => some (a, 1, c)
  | .add (.root 2 (.lit c)) (.lit a) => some (a, 1, c)
  | .add (.lit a) (.neg (.mul (.lit b) (.root 2 (.lit c)))) => some (a, -b, c)
  | .add (.lit a) (.neg (.root 2 (.lit c))) => some (a, -1, c)
  | _ => none

/-- Try to denest ³√(a + b√c) into the form p + q√c
    where p, q are rational numbers satisfying:
      p³ + 3pq²c = a
      3p²q + q³c = b
    For this to work, a² - b²c must be a perfect cube. -/
def tryCubeRootDenest (a b c : Rat) : Option (RadExpr Rat) :=
  let norm := a * a - b * b * c
  match isRationalCubeRoot norm with
  | none => none
  | some cbrtNorm =>
    -- 4p³ - 3·cbrtNorm·p - a = 0
    let candidates := rationalCubeEqRoots 4 (-3 * cbrtNorm) (-a)
    match candidates with
    | [] => none
    | p :: _ =>
      let q2 := (p * p - cbrtNorm) / c
      match isRationalSqrt q2 with
      | none => none
      | some q =>
        -- Pick sign of q based on sign of b
        let bCheck := 3 * p * p * q + q * q * q * c
        let q' := if bCheck * (if b > 0 then 1 else -1) ≥ 0 then q else -q
        let result : RadExpr Rat := .add (.lit p) (.mul (.lit q') (.root 2 (.lit c)))
        -- Sanity check via Float evaluation
        let expected := evalFloat (.root 3 (.add (.lit a) (.mul (.lit b) (.root 2 (.lit c)))))
        let got := evalFloat result
        if Float.abs (expected - got) < 1.0e-10 then some result
        else none

mutual

/-- Recursively denest inner expressions. -/
partial def denestInner : RadExpr Rat → RadExpr Rat
  | .root m a => denestNthRoot (.root m a)
  | .neg a => .neg (denestInner a)
  | .add a b => .add (denestInner a) (denestInner b)
  | .mul a b => .mul (denestInner a) (denestInner b)
  | .inv a => .inv (denestInner a)
  | .pow a n => .pow (denestInner a) n
  | other => other

/-- Attempt to denest an nth root expression. -/
partial def denestNthRoot : RadExpr Rat → RadExpr Rat
  | .root n (.root m a) =>
    -- ᵐ√(ⁿ√a) = ᵐⁿ√a
    denestNthRoot (.root (m * n) (denestNthRoot (.root 1 a)))
  | .root n (.lit r) =>
    if r == 0 then .lit 0
    else if r == 1 then .lit 1
    else if r > 0 then simplifyRootOfRational n r
    else if n % 2 != 0 then .neg (denestNthRoot (.root n (.lit (-r))))
    else .root n (.lit r)
  | .root 3 inner =>
    -- Try cube root denesting: ³√(a + b√c) = p + q√c
    match matchSqrtNested3 inner with
    | some (a, b, c) =>
      match tryCubeRootDenest a b c with
      | some result => result
      | none => .root 3 (denestInner inner)
    | none => .root 3 (denestInner inner)
  | .root n a => .root n (denestInner a)
  | other => other

end -- mutual

end Surd
