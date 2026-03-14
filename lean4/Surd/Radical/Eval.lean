/-
  Surd.Radical.Eval — Numerical evaluation of radical expressions.
-/
import Surd.Radical.Expr
import Surd.Interval
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- A complex number (simple pair). -/
structure Complex (α : Type) where
  re : α
  im : α
  deriving Repr, BEq

instance {α : Type} [Inhabited α] : Inhabited (Complex α) where
  default := ⟨default, default⟩

namespace Complex

variable {α : Type}

def zero [OfNat α 0] : Complex α := ⟨0, 0⟩
def one [OfNat α 0] [OfNat α 1] : Complex α := ⟨1, 0⟩
def ofReal [OfNat α 0] (x : α) : Complex α := ⟨x, 0⟩

def add [Add α] (a b : Complex α) : Complex α :=
  ⟨a.re + b.re, a.im + b.im⟩

def sub [Sub α] (a b : Complex α) : Complex α :=
  ⟨a.re - b.re, a.im - b.im⟩

def neg [Neg α] (a : Complex α) : Complex α :=
  ⟨-a.re, -a.im⟩

def mul [Add α] [Sub α] [Mul α] (a b : Complex α) : Complex α :=
  ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩

def inv [Add α] [Sub α] [Mul α] [Div α] [Neg α] (a : Complex α) : Complex α :=
  let d := a.re * a.re + a.im * a.im
  ⟨a.re / d, -a.im / d⟩

instance [Add α] : Add (Complex α) := ⟨add⟩
instance [Sub α] : Sub (Complex α) := ⟨sub⟩
instance [Neg α] : Neg (Complex α) := ⟨neg⟩
instance [Add α] [Sub α] [Mul α] : Mul (Complex α) := ⟨mul⟩

end Complex

/-- Convert Int to Float. -/
def intToFloat (n : Int) : Float :=
  match n with
  | .ofNat k => k.toFloat
  | .negSucc k => -(k + 1).toFloat

/-- Convert Rat to Float. -/
def ratToFloat (r : Rat) : Float :=
  intToFloat r.num / r.den.toFloat

/-- Evaluate a RadExpr to Float. -/
def evalFloat : RadExpr Rat → Float
  | .lit r => ratToFloat r
  | .neg a => -evalFloat a
  | .add a b => evalFloat a + evalFloat b
  | .mul a b => evalFloat a * evalFloat b
  | .inv a => 1.0 / evalFloat a
  | .root n a =>
    let v := evalFloat a
    if n == 2 then Float.sqrt v
    else
      let fn := intToFloat n
      if v ≥ 0 then Float.exp (Float.log v / fn)
      else if n % 2 == 1 then -Float.exp (Float.log (-v) / fn)
      else 0.0 / 0.0  -- NaN
  | .pow a n =>
    let v := evalFloat a
    Float.pow v (intToFloat n)

/-- Principal nth root of a complex number (polar form). -/
def complexNthRoot (n : Int) (z : Complex Float) : Complex Float :=
  let mag := Float.sqrt (z.re * z.re + z.im * z.im)
  if mag < 1.0e-300 then Complex.zero
  else
    let theta := Float.atan2 z.im z.re
    let fn := intToFloat n
    let rn := Float.exp (Float.log mag / fn)
    let an := theta / fn
    ⟨rn * Float.cos an, rn * Float.sin an⟩

/-- Complex power by repeated squaring. -/
def complexPow (z : Complex Float) (n : Int) : Complex Float :=
  if n == 0 then Complex.one
  else if n < 0 then Complex.inv (complexPowNat z (-n).toNat)
  else complexPowNat z n.toNat
where
  complexPowNat (z : Complex Float) : Nat → Complex Float
    | 0 => Complex.one
    | 1 => z
    | n + 2 =>
      if (n + 2) % 2 == 0 then
        let half := complexPowNat z ((n + 2) / 2)
        Complex.mul half half
      else Complex.mul z (complexPowNat z (n + 1))

/-- Evaluate a RadExpr to Complex Float. -/
def evalComplex : RadExpr Rat → Complex Float
  | .lit r => Complex.ofReal (ratToFloat r)
  | .neg a => Complex.neg (evalComplex a)
  | .add a b => Complex.add (evalComplex a) (evalComplex b)
  | .mul a b => Complex.mul (evalComplex a) (evalComplex b)
  | .inv a => Complex.inv (evalComplex a)
  | .root n a => complexNthRoot n (evalComplex a)
  | .pow a n => complexPow (evalComplex a) n

instance : Inhabited Interval where
  default := ⟨0, 0⟩

instance : Inhabited ComplexInterval where
  default := ⟨default, default⟩

/-- Evaluate a RadExpr to a rational Interval. -/
partial def evalInterval : RadExpr Rat → Interval
  | .lit r => Interval.fromRat r
  | .neg a => Interval.ineg (evalInterval a)
  | .add a b => Interval.iadd (evalInterval a) (evalInterval b)
  | .mul a b => Interval.imul (evalInterval a) (evalInterval b)
  | .inv a => Interval.iinv (evalInterval a)
  | .root n a =>
    let iv := evalInterval a
    if n == 2 then Interval.isqrt iv
    else Interval.inth n.toNat iv
  | .pow a n =>
    if n ≥ 0 then Interval.ipow (evalInterval a) n.toNat
    else Interval.iinv (Interval.ipow (evalInterval a) (-n).toNat)

/-- Evaluate a RadExpr to a ComplexInterval. -/
partial def evalComplexInterval : RadExpr Rat → ComplexInterval
  | .lit r => ComplexInterval.fromRat r
  | .neg a => ComplexInterval.cineg (evalComplexInterval a)
  | .add a b => ComplexInterval.ciadd (evalComplexInterval a) (evalComplexInterval b)
  | .mul a b => ComplexInterval.cimul (evalComplexInterval a) (evalComplexInterval b)
  | .inv a =>
    let z := evalComplexInterval a
    let magSq := ComplexInterval.ciMagnitudeSq z
    ⟨z.re.idiv magSq, z.im.ineg.idiv magSq⟩
  | .root n a =>
    let iv := evalInterval (.root n a)
    ComplexInterval.fromReal iv
  | .pow a n =>
    if n ≥ 0 then cipow (evalComplexInterval a) n.toNat
    else
      let zn := cipow (evalComplexInterval a) (-n).toNat
      let magSq := ComplexInterval.ciMagnitudeSq zn
      ⟨zn.re.idiv magSq, zn.im.ineg.idiv magSq⟩
where
  cipow (z : ComplexInterval) : Nat → ComplexInterval
    | 0 => ComplexInterval.fromRat 1
    | 1 => z
    | n + 1 => ComplexInterval.cimul z (cipow z n)

end Surd
