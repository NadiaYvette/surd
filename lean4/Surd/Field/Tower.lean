/-
  Surd.Field.Tower — Tower of algebraic extensions.

  A field tower Q ⊂ Q(α₁) ⊂ Q(α₁)(α₂) ⊂ ... is built by iterated
  simple extensions. Each radical (nth root) introduces a new layer.

  This module provides the evaluation function `evalInField` for
  interpreting radical expressions in an extension field, given
  callbacks for embedding rationals and resolving roots.
-/
import Surd.Radical.Expr
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Evaluate a RadExpr in any field k, given:
    - embedR: embed a Rational into k
    - resolveRoot: given root degree n and evaluated radicand in k,
      return the nth root in k
    This is the key function for converting radical expressions
    into elements of extension fields. -/
partial def evalInField {k : Type} [Add k] [Sub k] [Mul k] [Neg k] [Div k]
    [OfNat k 0] [OfNat k 1] [Inhabited k]
    (embedR : Rat → k)
    (resolveRoot : Int → k → k)
    : RadExpr Rat → k
  | .lit r => embedR r
  | .neg a => -(evalInField embedR resolveRoot a)
  | .add a b => evalInField embedR resolveRoot a + evalInField embedR resolveRoot b
  | .mul a b => evalInField embedR resolveRoot a * evalInField embedR resolveRoot b
  | .inv a => 1 / evalInField embedR resolveRoot a
  | .root n a => resolveRoot n (evalInField embedR resolveRoot a)
  | .pow a n =>
    if n ≥ 0 then powK (evalInField embedR resolveRoot a) n.toNat
    else 1 / powK (evalInField embedR resolveRoot a) (-n).toNat
where
  powK (x : k) : Nat → k
    | 0 => 1
    | 1 => x
    | n + 1 => x * powK x n

end Surd
