/-
  Surd.Trig.RootOfUnity — Express roots of unity as radical expressions.

  A primitive nth root of unity ζₙ = e^(2πi/n) can ALWAYS be expressed
  in radicals, since cyclotomic extensions have abelian Galois groups.

  The compass-and-straightedge constructible case (n = 2^a · distinct
  Fermat primes) is the special case where only square roots are needed.
-/
import Surd.Radical.Expr
import Surd.PrimeFactors
import Surd.Positive
import Surd.Trig.Galois
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Known Fermat primes. -/
def fermatPrimes : List Nat := [3, 5, 17, 257, 65537]

/-- Check if cos(2π/n) is expressible using only nested square roots. -/
def isConstructible (n : Nat) : Bool :=
  let fs := factoriseNat n
  let oddFactors := fs.filter (fun (p, _) => p != 2)
  oddFactors.all (fun (p, e) => e == 1 && fermatPrimes.contains p)

-- ---------------------------------------------------------------------------
-- Hand-optimised closed forms
-- ---------------------------------------------------------------------------

-- cos(2π/5) = (√5 - 1) / 4
private def cos2piOver5 : RadExpr Rat :=
  .mul (.inv (.lit 4)) (.add (.root 2 (.lit 5)) (.lit (-1)))

-- cos(2π/10) = cos(π/5) = (1 + √5) / 4
private def cos2piOver10 : RadExpr Rat :=
  .mul (.inv (.lit 4)) (.add (.lit 1) (.root 2 (.lit 5)))

-- cos(2π/15) = (1 + √5 + √(30 - 6√5)) / 8
private def cos2piOver15 : RadExpr Rat :=
  .mul (.inv (.lit 8))
    (.add (.add (.lit 1) (.root 2 (.lit 5)))
          (.root 2 (.add (.lit 30) (.neg (.mul (.lit 6) (.root 2 (.lit 5)))))))

-- cos(2π/16) = cos(π/8) = √(2 + √2) / 2
private def cos2piOver16 : RadExpr Rat :=
  .mul (.inv (.lit 2)) (.root 2 (.add (.lit 2) (.root 2 (.lit 2))))

-- cos(2π/17): Gauss's famous 17-gon
private def cos2piOver17 : RadExpr Rat :=
  let s17 := RadExpr.root 2 (.lit 17)
  let a := RadExpr.root 2 (.add (.lit 34) (.neg (.mul (.lit 2) s17)))
  let b := RadExpr.root 2 (.add (.lit 34) (.mul (.lit 2) s17))
  .mul (.inv (.lit 16))
    (.add (.add (.add (.lit (-1)) s17) a)
          (.mul (.lit 2)
            (.root 2
              (.add (.add (.lit 17) (.mul (.lit 3) s17))
                    (.add (.neg a) (.neg (.mul (.lit 2) b)))))))

-- cos(2π/20) = cos(π/10) = √(10 + 2√5) / 4
private def cos2piOver20 : RadExpr Rat :=
  .mul (.inv (.lit 4)) (.root 2 (.add (.lit 10) (.mul (.lit 2) (.root 2 (.lit 5)))))

-- cos(2π/24) = cos(π/12) = (√6 + √2) / 4
private def cos2piOver24 : RadExpr Rat :=
  .mul (.inv (.lit 4)) (.add (.root 2 (.lit 6)) (.root 2 (.lit 2)))

-- ---------------------------------------------------------------------------
-- Powers of 2
-- ---------------------------------------------------------------------------

private def isPowerOf2 (n : Nat) : Bool :=
  n > 0 && (n &&& (n - 1)) == 0

/-- cos(2π/2^k) via half-angle recurrence. -/
private partial def cosOfPow2 : Nat → RadExpr Rat
  | 1 => .lit 1
  | 2 => .lit (-1)
  | 4 => .lit 0
  | k => .root 2 (.mul (.inv (.lit 2)) (.add (.lit 1) (cosOfPow2 (k / 2))))

-- ---------------------------------------------------------------------------
-- Composite
-- ---------------------------------------------------------------------------

/-- Extended GCD for integers: returns (a, b) such that a*x + b*y = gcd(x,y). -/
private partial def extGcdInt : Int → Int → Int × Int
  | 0, _ => (0, 1)
  | x, y =>
    let (q, r) := (y / x, y % x)
    let (a, b) := extGcdInt r x
    (b - q * a, a)

/-- Sign of sin(2πk/m). -/
private def sinSignFromAngle (k m : Int) : Int :=
  let k' := k % m
  if k' == 0 then 0
  else if 2 * k' < m then 1
  else if 2 * k' == m then 0
  else -1

/-- Simple Chebyshev: T_k(x). -/
private partial def chebyshevSimple (k : Int) (x : RadExpr Rat) : RadExpr Rat :=
  if k == 0 then .lit 1
  else if k == 1 then x
  else go 2 (.lit 1) x
where
  go (n : Int) (t0 t1 : RadExpr Rat) : RadExpr Rat :=
    if n > k then t1
    else
      let t2 := RadExpr.add (.mul (.mul (.lit 2) x) t1) (.neg t0)
      go (n + 1) t1 t2

/-- sin from cos: sin(θ) = √(1 - cos²(θ)). -/
private def sinFromCos (c : RadExpr Rat) : RadExpr Rat :=
  .root 2 (.add (.lit 1) (.neg (.mul c c)))

/-- Decompose n into (p, k) if n = p^k for prime p. -/
private def primePowerDecomp (n : Nat) : Option (Nat × Nat) :=
  let fs := factoriseNat n
  match fs with
  | [(p, k)] => some (p, k)
  | _ => none

mutual

/-- cos(2π/n) for composite n via CRT decomposition. -/
partial def cosOfUnityComposite (n : Nat) : Option (RadExpr Rat) :=
  let fs := factoriseNat n
  match fs with
  | [] => none
  | [_] => none -- prime power
  | (p1, e1) :: _ =>
    let n1 := p1 ^ e1
    let n2 := n / n1
    let (a, b) := extGcdInt (Int.ofNat n1) (Int.ofNat n2)
    let a' := a % Int.ofNat n2
    let b' := b % Int.ofNat n1
    let sA := sinSignFromAngle a' (Int.ofNat n2)
    let sB := sinSignFromAngle b' (Int.ofNat n1)
    let signFactor := sA * sB
    do
      let cosBase1 ← cosOfUnity n2
      let cosBase2 ← cosOfUnity n1
      let cosAE := chebyshevSimple a' cosBase1
      let cosBE := chebyshevSimple b' cosBase2
      if signFactor == 0 then
        some (.mul cosAE cosBE)
      else
        let sinAbsA := sinFromCos cosAE
        let sinAbsB := sinFromCos cosBE
        let sinProduct := RadExpr.mul sinAbsA sinAbsB
        let result :=
          if signFactor == 1 then .add (.mul cosAE cosBE) (.neg sinProduct)
          else .add (.mul cosAE cosBE) sinProduct
        some result

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

/-- Compute cos(2π/n) as a radical expression. -/
partial def cosOfUnity (n : Nat) : Option (RadExpr Rat) :=
  if n == 0 then none
  else if n == 1 then some (.lit 1)
  else if n == 2 then some (.lit (-1))
  else if n == 3 then some (.lit (-1 / 2))
  else if n == 4 then some (.lit 0)
  else if n == 5 then some cos2piOver5
  else if n == 6 then some (.lit (1 / 2))
  else if n == 8 then some (.mul (.inv (.lit 2)) (.root 2 (.lit 2)))
  else if n == 10 then some cos2piOver10
  else if n == 12 then some (.mul (.inv (.lit 2)) (.root 2 (.lit 3)))
  else if n == 15 then some cos2piOver15
  else if n == 16 then some cos2piOver16
  else if n == 17 then some cos2piOver17
  else if n == 20 then some cos2piOver20
  else if n == 24 then some cos2piOver24
  else if isPowerOf2 n then some (cosOfPow2 n)
  else if isPrime n then cosOfUnityViaGauss (Int.ofNat n)
  else
    match primePowerDecomp n with
    | some (p, k) =>
      if p > 2 && k > 1 then cosOfUnityViaGauss (Int.ofNat n)
      else cosOfUnityComposite n
    | none => cosOfUnityComposite n

/-- Compute sin(2π/n) as a radical expression. -/
partial def sinOfUnity (n : Nat) : Option (RadExpr Rat) :=
  if n == 0 then none
  else if n == 1 then some (.lit 0)
  else if n == 2 then some (.lit 0)
  else if n == 3 then some (.mul (.lit (1 / 2)) (.root 2 (.lit 3)))
  else if n == 4 then some (.lit 1)
  else if n == 6 then some (.mul (.lit (1 / 2)) (.root 2 (.lit 3)))
  else if n == 8 then some (.mul (.inv (.lit 2)) (.root 2 (.lit 2)))
  else if n == 12 then some (.lit (1 / 2))
  else
    match cosOfUnity n with
    | none => none
    | some c =>
      let sin2 := RadExpr.add (.lit 1) (.neg (.mul c c))
      some (.root 2 sin2)

end -- mutual

/-- All cos(2πk/n) for k coprime to n. -/
partial def allCosOfUnity (n : Nat) : Option (List (Int × RadExpr Rat)) := do
  let periods ← allPeriodsViaGauss (Int.ofNat n)
  let ni := Int.ofNat n
  some (periods.filterMap fun (k, pk) =>
    match periods.find? (fun (k', _) => k' == ni - k) with
    | some (_, pnk) =>
      some (k, .mul (.inv (.lit 2)) (.add pk pnk))
    | none => none)

/-- All sin(2πk/n) for k coprime to n. -/
partial def allSinOfUnity (n : Nat) : Option (List (Int × RadExpr Rat)) := do
  let periods ← allPeriodsViaGauss (Int.ofNat n)
  let ni := Int.ofNat n
  let i := RadExpr.root 2 (.lit (-1 : Rat))
  let negIOver2 : RadExpr Rat := .mul (.inv (.lit 2)) (.neg i)
  some (periods.filterMap fun (k, pk) =>
    match periods.find? (fun (k', _) => k' == ni - k) with
    | some (_, pnk) =>
      let diff := RadExpr.add pk (.neg pnk)
      some (k, .mul negIOver2 diff)
    | none => none)

end Surd
