/-
  Surd.Positive — Strictly positive natural numbers.
-/
namespace Surd

/-- A strictly positive natural number. -/
def Positive := { n : Nat // n > 0 }

namespace Positive

/-- Extract the underlying Nat. -/
def val (p : Positive) : Nat := p.1

instance : Repr Positive where
  reprPrec p n := reprPrec p.val n

instance : ToString Positive where
  toString p := toString p.val

instance : BEq Positive where
  beq a b := a.val == b.val

instance : Ord Positive where
  compare a b := compare a.val b.val

instance : Hashable Positive where
  hash p := hash p.val

/-- Smart constructor: returns none for zero. -/
def mk? (n : Nat) : Option Positive :=
  if h : n > 0 then some ⟨n, h⟩ else none

/-- Constructor with proof obligation. -/
def ofNat (n : Nat) (h : n > 0 := by omega) : Positive := ⟨n, h⟩

instance (n : Nat) : OfNat Positive (n + 1) where
  ofNat := ⟨n + 1, by omega⟩

/-- Addition of positive numbers is positive. -/
instance : Add Positive where
  add a b := ⟨a.val + b.val, Nat.add_pos_left a.2 b.val⟩

/-- Multiplication of positive numbers is positive. -/
instance : Mul Positive where
  mul a b := ⟨a.val * b.val, Nat.mul_pos a.2 b.2⟩

/-- Convert to Int. -/
def toInt (p : Positive) : Int := p.val

/-- Convert to Nat. -/
def toNat (p : Positive) : Nat := p.val

end Positive

end Surd
