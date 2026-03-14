/-
  Surd.Poly.Univariate — Dense univariate polynomial arithmetic.

  Polynomials are stored as Arrays of coefficients, low-degree first:
  #[a0, a1, ..., an] represents a0 + a1·x + ... + an·x^n.
  Invariant: trailing zeros are stripped (leading coefficient is nonzero),
  except for the zero polynomial which is #[].
-/
namespace Surd

/-- A univariate polynomial with coefficients in k.
    Stored as an Array of coefficients, low-degree first. -/
structure Poly (k : Type) where
  coeffs : Array k
  deriving Repr

namespace Poly

variable {k : Type}

section Ops
variable [BEq k] [Add k] [Sub k] [Mul k] [Neg k] [Div k]
  [OfNat k 0] [OfNat k 1] [Inhabited k]

/-- Strip trailing zeros from coefficient array. -/
private def stripZeros (cs : Array k) : Array k :=
  let rec go (i : Nat) : Nat :=
    match i with
    | 0 => 0
    | j + 1 => if cs[j]! == (0 : k) then go j else j + 1
  cs.extract 0 (go cs.size)

/-- Smart constructor: strip trailing zeros. -/
def mkPoly (cs : Array k) : Poly k :=
  ⟨stripZeros cs⟩

/-- The zero polynomial. -/
def zero : Poly k := ⟨#[]⟩

/-- A constant polynomial. -/
def const (c : k) : Poly k :=
  if c == (0 : k) then zero else ⟨#[c]⟩

/-- The polynomial x. -/
def x : Poly k := ⟨#[0, 1]⟩

/-- Is this the zero polynomial? -/
def isZero (p : Poly k) : Bool :=
  p.coeffs.isEmpty

/-- Degree of the polynomial. Returns none for the zero polynomial. -/
def degree (p : Poly k) : Option Nat :=
  if p.coeffs.isEmpty then none
  else some (p.coeffs.size - 1)

/-- Degree as Int, returning -1 for zero polynomial. -/
def degreeInt (p : Poly k) : Int :=
  if p.coeffs.isEmpty then -1
  else p.coeffs.size - 1

/-- Leading coefficient. -/
def leadCoeff (p : Poly k) : Option k :=
  if p.coeffs.isEmpty then none
  else some p.coeffs.back!

/-- Get coefficient at index i (0 if out of bounds). -/
def coeff (p : Poly k) (i : Nat) : k :=
  if h : i < p.coeffs.size then p.coeffs[i] else 0

/-- Evaluate a polynomial at a point using Horner's method. -/
def eval (p : Poly k) (val : k) : k :=
  p.coeffs.foldr (fun c acc => c + val * acc) 0

/-- Multiply by a scalar. -/
def scale (s : k) (p : Poly k) : Poly k :=
  if s == (0 : k) then zero
  else mkPoly (p.coeffs.map (· * s))

/-- Negate a polynomial. -/
def neg (p : Poly k) : Poly k :=
  ⟨p.coeffs.map (- ·)⟩

/-- Add two polynomials. -/
def add (p q : Poly k) : Poly k :=
  let n := max p.coeffs.size q.coeffs.size
  let cs := Array.ofFn (n := n) fun i =>
    p.coeff i + q.coeff i
  mkPoly cs

/-- Subtract two polynomials. -/
def sub (p q : Poly k) : Poly k :=
  let n := max p.coeffs.size q.coeffs.size
  let cs := Array.ofFn (n := n) fun i =>
    p.coeff i - q.coeff i
  mkPoly cs

/-- Multiply two polynomials (schoolbook). -/
def mul (p q : Poly k) : Poly k :=
  if p.isZero || q.isZero then zero
  else
    let n := p.coeffs.size + q.coeffs.size - 1
    let cs := Array.ofFn (n := n) fun i => Id.run do
      let mut s : k := 0
      for j in [:p.coeffs.size] do
        if i.val ≥ j && i.val - j < q.coeffs.size then
          s := s + p.coeffs[j]! * q.coeffs[i.val - j]!
      return s
    mkPoly cs

/-- Shift polynomial by n positions (multiply by x^n). -/
def shift (p : Poly k) (n : Nat) : Poly k :=
  if p.isZero then zero
  else ⟨(Array.mkArray n (0 : k)) ++ p.coeffs⟩

/-- Polynomial division with remainder: divMod p q = (quotient, remainder).
    Terminates because remainder degree strictly decreases. -/
def divMod (p q : Poly k) : Poly k × Poly k :=
  if q.isZero then (zero, p)
  else go p zero p.coeffs.size
where
  go (rem quot : Poly k) (fuel : Nat) : Poly k × Poly k :=
    match fuel with
    | 0 => (quot, rem)
    | fuel' + 1 =>
      if rem.degreeInt < q.degreeInt then (quot, rem)
      else
        match rem.leadCoeff, q.leadCoeff with
        | some rlc, some qlc =>
          let d := rem.coeffs.size - q.coeffs.size
          let c := rlc / qlc
          let term : Poly k := ⟨(Array.mkArray d (0 : k)).push c⟩
          let rem' := Poly.sub rem (Poly.mul term q)
          go rem' (Poly.add quot term) fuel'
        | _, _ => (quot, rem)

/-- Polynomial GCD via the Euclidean algorithm.
    Terminates because degree of second argument strictly decreases. -/
def gcd (p q : Poly k) : Poly k :=
  go p q (p.coeffs.size + q.coeffs.size + 1)
where
  go (a b : Poly k) (fuel : Nat) : Poly k :=
    match fuel with
    | 0 => a
    | fuel' + 1 =>
      if b.isZero then a
      else go b (divMod a b).2 fuel'

/-- Make a polynomial monic (leading coefficient = 1). -/
def monic (p : Poly k) : Poly k :=
  match p.leadCoeff with
  | none => zero
  | some lc => scale ((1 : k) / lc) p

/-- Formal derivative. -/
def diff (p : Poly k) : Poly k :=
  if p.coeffs.size ≤ 1 then zero
  else
    let cs := Array.ofFn (n := p.coeffs.size - 1) fun i => Id.run do
      let mut coeff : k := p.coeffs[i.val + 1]!
      for _ in [:i.val + 1] do
        coeff := coeff + p.coeffs[i.val + 1]!
      return coeff
    mkPoly cs

/-- Square-free part: p / gcd(p, p'). -/
def squareFree (p : Poly k) : Poly k :=
  let g := gcd p (diff p)
  (divMod p g).1

end Ops

instance [BEq k] : BEq (Poly k) where
  beq p q := p.coeffs == q.coeffs

instance : Inhabited (Poly k) where
  default := ⟨#[]⟩

instance [BEq k] [Add k] [Sub k] [Mul k] [Neg k] [Div k]
    [OfNat k 0] [OfNat k 1] [Inhabited k] : Add (Poly k) where
  add := add

instance [BEq k] [Add k] [Sub k] [Mul k] [Neg k] [Div k]
    [OfNat k 0] [OfNat k 1] [Inhabited k] : Sub (Poly k) where
  sub := sub

instance [BEq k] [Add k] [Sub k] [Mul k] [Neg k] [Div k]
    [OfNat k 0] [OfNat k 1] [Inhabited k] : Mul (Poly k) where
  mul := mul

instance [Neg k] : Neg (Poly k) where
  neg := neg

end Poly

end Surd
