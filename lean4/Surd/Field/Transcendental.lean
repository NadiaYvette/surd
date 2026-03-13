/-
  Surd.Field.Transcendental — Transcendental extension field Q(x₁,...,xₙ).

  Elements are rational functions (ratios of multivariate polynomials).
  Use reduceFrac to reduce to lowest terms over Q.

  Specialized to Rat since MPoly is Rat-only.
-/
import Surd.Poly.Multivariate
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Rational functions
-- ---------------------------------------------------------------------------

/-- An element of Q(x₁,...,xₙ): a ratio of multivariate polynomials.
    Invariant: denominator is non-zero. -/
structure RatFunc where
  rfNum : MPoly Rat
  rfDen : MPoly Rat
  deriving Inhabited

/-- Smart constructor ensuring non-zero denominator. -/
def mkRatFunc (n d : MPoly Rat) : RatFunc :=
  ⟨n, d⟩

/-- Reduce a rational function to lowest terms. -/
partial def reduceFrac (r : RatFunc) : RatFunc :=
  if MPoly.isZero r.rfNum then ⟨MPoly.zero, MPoly.const 1⟩
  else
    let g := gcdMPoly r.rfNum r.rfDen
    if MPoly.isZero g || g == MPoly.const 1 then r
    else ⟨exactDivMPoly r.rfNum g, exactDivMPoly r.rfDen g⟩

/-- Constant rational function. -/
def constRF (c : Rat) : RatFunc :=
  ⟨MPoly.const c, MPoly.one⟩

/-- A transcendental variable as a rational function. -/
def varRF (v : Int) : RatFunc :=
  ⟨MPoly.var v, MPoly.one⟩

/-- Test if a rational function is a constant. -/
def isConstRF (r : RatFunc) : Bool :=
  MPoly.numTerms r.rfNum ≤ 1 && MPoly.numTerms r.rfDen == 1
  && MPoly.totalDegree r.rfNum == 0 && MPoly.totalDegree r.rfDen == 0

/-- Evaluate by substituting values for variables. -/
def evalRF (env : Int → Rat) (r : RatFunc) : Rat :=
  MPoly.eval env r.rfNum / MPoly.eval env r.rfDen

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

instance : BEq RatFunc where
  beq a b := MPoly.mul a.rfNum b.rfDen == MPoly.mul b.rfNum a.rfDen

instance : Add RatFunc where
  add a b := ⟨MPoly.add (MPoly.mul a.rfNum b.rfDen) (MPoly.mul b.rfNum a.rfDen),
               MPoly.mul a.rfDen b.rfDen⟩

instance : Sub RatFunc where
  sub a b := ⟨MPoly.sub (MPoly.mul a.rfNum b.rfDen) (MPoly.mul b.rfNum a.rfDen),
               MPoly.mul a.rfDen b.rfDen⟩

instance : Mul RatFunc where
  mul a b := ⟨MPoly.mul a.rfNum b.rfNum, MPoly.mul a.rfDen b.rfDen⟩

instance : Neg RatFunc where
  neg a := ⟨MPoly.neg a.rfNum, a.rfDen⟩

instance : Div RatFunc where
  div a b := ⟨MPoly.mul a.rfNum b.rfDen, MPoly.mul a.rfDen b.rfNum⟩

end Surd
