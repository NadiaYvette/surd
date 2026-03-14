/-
  Surd.Poly.Lemmas — Basic simp lemmas for polynomial operations.
-/
import Surd.Poly.Univariate
import Std.Internal.Rat

open Std.Internal

namespace Surd

namespace Poly

@[simp] theorem zero_isZero {k : Type} : (Poly.zero : Poly k).isZero = true := by
  simp [zero, isZero, Array.isEmpty]

@[simp] theorem zero_coeffs_empty {k : Type} : (Poly.zero : Poly k).coeffs = #[] :=
  rfl

@[simp] theorem degree_zero {k : Type} [BEq k] [OfNat k 0] :
    (Poly.zero : Poly k).degree = none := by
  simp [degree, zero]

@[simp] theorem degreeInt_zero {k : Type} [BEq k] [OfNat k 0] :
    (Poly.zero : Poly k).degreeInt = -1 := by
  simp [degreeInt, zero]

end Poly

end Surd
