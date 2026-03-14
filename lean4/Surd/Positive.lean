/-
  Surd.Positive — Strictly positive natural numbers.

  A subtype of Nat with a proof that the value is > 0.
  Used for root indices (the n in nth root) and prime factorisation
  inputs, eliminating non-positive error cases at the type level.
-/
namespace Surd

/-- A strictly positive natural number. -/
structure Positive where
  val : Nat
  pos : val > 0
  deriving Repr, BEq, Hashable

namespace Positive

instance : ToString Positive where
  toString p := toString p.val

instance : Ord Positive where
  compare a b := compare a.val b.val

/-- Smart constructor: returns none for zero. -/
def mk? (n : Nat) : Option Positive :=
  if h : n > 0 then some ⟨n, h⟩ else none

/-- Constructor with proof obligation. -/
def ofNat (n : Nat) (h : n > 0 := by omega) : Positive := ⟨n, h⟩

instance (n : Nat) : OfNat Positive (n + 1) where
  ofNat := ⟨n + 1, by omega⟩

/-- Addition of positive numbers is positive. -/
instance : Add Positive where
  add a b := ⟨a.val + b.val, Nat.add_pos_left a.pos b.val⟩

/-- Multiplication of positive numbers is positive. -/
instance : Mul Positive where
  mul a b := ⟨a.val * b.val, Nat.mul_pos a.pos b.pos⟩

@[simp] theorem Positive.add_pos_val (a b : Positive) : (a + b).val > 0 :=
  Nat.add_pos_left a.pos b.val

@[simp] theorem Positive.mul_pos_val (a b : Positive) : (a * b).val > 0 :=
  Nat.mul_pos a.pos b.pos

/-- Convert to Int. -/
def toInt (p : Positive) : Int := p.val

/-- Convert to Nat. -/
def toNat (p : Positive) : Nat := p.val

end Positive

end Surd
