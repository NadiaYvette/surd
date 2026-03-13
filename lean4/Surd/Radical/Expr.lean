/-
  Surd.Radical.Expr — Core AST for radical expressions.

  RadExpr k is parameterised by the coefficient type k.
  Normalization is explicit: these constructors are "dumb".
-/
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- A radical expression over a coefficient field k. -/
inductive RadExpr (k : Type) where
  | lit : k → RadExpr k
  | neg : RadExpr k → RadExpr k
  | add : RadExpr k → RadExpr k → RadExpr k
  | mul : RadExpr k → RadExpr k → RadExpr k
  | inv : RadExpr k → RadExpr k
  | root : Int → RadExpr k → RadExpr k
  | pow : RadExpr k → Int → RadExpr k
  deriving Repr, Inhabited

namespace RadExpr

variable {k k' : Type}

/-- Map a function over the coefficients. -/
def mapCoeffs (f : k → k') : RadExpr k → RadExpr k'
  | .lit c => .lit (f c)
  | .neg a => .neg (mapCoeffs f a)
  | .add a b => .add (mapCoeffs f a) (mapCoeffs f b)
  | .mul a b => .mul (mapCoeffs f a) (mapCoeffs f b)
  | .inv a => .inv (mapCoeffs f a)
  | .root n a => .root n (mapCoeffs f a)
  | .pow a n => .pow (mapCoeffs f a) n

/-- Depth of the expression tree. -/
def depth : RadExpr k → Nat
  | .lit _ => 0
  | .neg a => 1 + depth a
  | .add a b => 1 + max (depth a) (depth b)
  | .mul a b => 1 + max (depth a) (depth b)
  | .inv a => 1 + depth a
  | .root _ a => 1 + depth a
  | .pow a _ => 1 + depth a

/-- Number of nodes in the expression tree. -/
def size : RadExpr k → Nat
  | .lit _ => 1
  | .neg a => 1 + size a
  | .add a b => 1 + size a + size b
  | .mul a b => 1 + size a + size b
  | .inv a => 1 + size a
  | .root _ a => 1 + size a
  | .pow a _ => 1 + size a

/-- Subtraction: a - b = a + (-b). -/
def sub (a b : RadExpr k) : RadExpr k :=
  .add a (.neg b)

/-- Division: a / b = a * (1/b). -/
def div (a b : RadExpr k) : RadExpr k :=
  .mul a (.inv b)

/-- Square root shorthand. -/
def sqrt (x : RadExpr k) : RadExpr k :=
  .root 2 x

/-- Cube root shorthand. -/
def cbrt (x : RadExpr k) : RadExpr k :=
  .root 3 x

/-- Lift a coefficient to a radical expression. -/
def ofCoeff (c : k) : RadExpr k :=
  .lit c

/-- Collect all radical indices (Root n _) in the expression. -/
def collectRadicalIndices : RadExpr k → List Int
  | .lit _ => []
  | .neg a => collectRadicalIndices a
  | .add a b => collectRadicalIndices a ++ collectRadicalIndices b
  | .mul a b => collectRadicalIndices a ++ collectRadicalIndices b
  | .inv a => collectRadicalIndices a
  | .root n a => n :: collectRadicalIndices a
  | .pow a _ => collectRadicalIndices a

/-- Count the number of Root nodes. -/
def radicalCount : RadExpr k → Nat
  | .lit _ => 0
  | .neg a => radicalCount a
  | .add a b => radicalCount a + radicalCount b
  | .mul a b => radicalCount a + radicalCount b
  | .inv a => radicalCount a
  | .root _ a => 1 + radicalCount a
  | .pow a _ => radicalCount a

/-- BEq instance when k has BEq. -/
protected def beq [BEq k] : RadExpr k → RadExpr k → Bool
  | .lit a, .lit b => a == b
  | .neg a, .neg b => RadExpr.beq a b
  | .add a1 a2, .add b1 b2 => RadExpr.beq a1 b1 && RadExpr.beq a2 b2
  | .mul a1 a2, .mul b1 b2 => RadExpr.beq a1 b1 && RadExpr.beq a2 b2
  | .inv a, .inv b => RadExpr.beq a b
  | .root n a, .root m b => n == m && RadExpr.beq a b
  | .pow a n, .pow b m => n == m && RadExpr.beq a b
  | _, _ => false

instance [BEq k] : BEq (RadExpr k) where
  beq := RadExpr.beq

/-- Tag for constructor ordering. -/
private def tag : RadExpr k → Nat
  | .lit _ => 0
  | .neg _ => 1
  | .add _ _ => 2
  | .mul _ _ => 3
  | .inv _ => 4
  | .root _ _ => 5
  | .pow _ _ => 6

/-- Ord instance when k has Ord. -/
protected def compare [Ord k] : RadExpr k → RadExpr k → Ordering
  | .lit a, .lit b => Ord.compare a b
  | .neg a, .neg b => RadExpr.compare a b
  | .add a1 a2, .add b1 b2 =>
    match RadExpr.compare a1 b1 with
    | .eq => RadExpr.compare a2 b2
    | r => r
  | .mul a1 a2, .mul b1 b2 =>
    match RadExpr.compare a1 b1 with
    | .eq => RadExpr.compare a2 b2
    | r => r
  | .inv a, .inv b => RadExpr.compare a b
  | .root n a, .root m b =>
    match Ord.compare n m with
    | .eq => RadExpr.compare a b
    | r => r
  | .pow a n, .pow b m =>
    match Ord.compare n m with
    | .eq => RadExpr.compare a b
    | r => r
  | x, y => Ord.compare (tag x) (tag y)

instance [Ord k] : Ord (RadExpr k) where
  compare := RadExpr.compare

/-- Convenience: lift a rational literal. -/
def ratE (r : Rat) : RadExpr Rat := .lit r

/-- Convenience: lift an integer literal. -/
def intE (n : Int) : RadExpr Rat := .lit n

end RadExpr

end Surd
