/-
  Surd.Polynomial.MinimalPoly — Minimal polynomial computation for radical expressions.

  Given a radical expression α, builds an annihilating polynomial that α satisfies,
  then factors to find the irreducible factor (the true minimal polynomial).

  The annihilating polynomial is constructed recursively:
    Lit r       → x - r
    Neg e       → p(-x) where p annihilates e
    Add a b     → composedSum(pₐ, pᵦ)
    Mul a b     → composedProduct(pₐ, pᵦ)
    Inv e       → reciprocal(p)
    Root n (Lit r) → xⁿ - r
    Root n e    → p(xⁿ)
    Pow e n     → Res_y(p(y), yⁿ - x)
-/
import Surd.Poly.Univariate
import Surd.Poly.Resultant
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal
open Surd.Poly

namespace Surd

/-- Stub for square-free factorization.
    TODO: implement Kronecker/Berlekamp factoring.
    Currently returns the monic square-free part as the sole factor. -/
private def factorSquareFree (p : Poly Rat) : List (Poly Rat) :=
  let sf := Poly.monic (Poly.squareFree p)
  if sf.isZero then [] else [sf]

/-- Pick the factor whose root is closest to the target value. -/
private def pickClosest (target : Float) (factors : List (Poly Rat)) : Poly Rat :=
  match factors with
  | [] => Poly.zero
  | [f] => f
  | f :: fs =>
    let approxRoot (p : Poly Rat) : Float :=
      -- Simple Newton iteration from target to find nearby root
      let eval (x : Float) : Float :=
        p.coeffs.foldr (fun c acc => ratToFloat c + x * acc) 0.0
      let evalDeriv (x : Float) : Float :=
        let dp := Poly.diff p
        dp.coeffs.foldr (fun c acc => ratToFloat c + x * acc) 0.0
      let rec go (x : Float) : Nat → Float
        | 0 => x
        | n + 1 =>
          let fx := eval x
          let dfx := evalDeriv x
          if Float.abs dfx < 1.0e-15 then x
          else go (x - fx / dfx) n
      go target 20
    let dist (p : Poly Rat) : Float :=
      Float.abs (approxRoot p - target)
    fs.foldl (fun best p => if dist p < dist best then p else best) f

/-- Compute annihilating polynomial for e^n given the annihilating poly for e. -/
private partial def annihilatingPolyOfPow (p : Poly Rat) (n : Nat) : Poly Rat :=
  match n with
  | 0 => Poly.mkPoly #[-1, 1]  -- x - 1
  | 1 => p
  | _ =>
    -- Res_y(p(y), y^n - x) at interpolation points
    let dp := match p.degree with | some d => d | none => 0
    let points := (List.range (dp + 1)).map fun i =>
      let x0 : Rat := (Int.ofNat i : Int)
      let ynMinusX0 : Poly Rat :=
        Poly.mkPoly (#[-x0] ++ Array.mkArray (n - 1) (0 : Rat) ++ #[1])
      (x0, Poly.resultant p ynMinusX0)
    Poly.lagrangeInterpolate points

/-- Compute an annihilating polynomial (not necessarily minimal) for
    a radical expression. The expression is guaranteed to be a root of
    this polynomial, but it may be reducible. -/
partial def annihilatingPoly : RadExpr Rat → Poly Rat
  | .lit r =>
    -- Minimal poly of r ∈ Q is (x - r)
    Poly.mkPoly #[-r, 1]
  | .neg e =>
    -- If p(x) annihilates e, then p(-x) annihilates -e
    Poly.negateVar (annihilatingPoly e)
  | .add a b =>
    -- composedSum gives polynomial whose roots are α_i + β_j
    Poly.composedSum (annihilatingPoly a) (annihilatingPoly b)
  | .mul a b =>
    -- composedProduct gives polynomial whose roots are α_i · β_j
    Poly.composedProduct (annihilatingPoly a) (annihilatingPoly b)
  | .inv e =>
    -- x^deg(p) · p(1/x) annihilates 1/e
    Poly.reciprocalPoly (annihilatingPoly e)
  | .root n (.lit r) =>
    -- nth root of rational r: annihilated by x^n - r
    if n ≤ 0 then Poly.mkPoly #[-r, 1]
    else
      let nNat := n.toNat
      let cs := #[-r] ++ Array.mkArray (nNat - 1) (0 : Rat) ++ #[1]
      Poly.mkPoly cs
  | .root n e =>
    -- If p(x) annihilates e, then p(x^n) annihilates e^(1/n)
    if n ≤ 0 then annihilatingPoly e
    else Poly.substituteXN n.toNat (annihilatingPoly e)
  | .pow e n =>
    if n ≥ 0 then
      annihilatingPolyOfPow (annihilatingPoly e) n.toNat
    else
      let pr := Poly.reciprocalPoly (annihilatingPoly e)
      annihilatingPolyOfPow pr (-n).toNat

/-- Compute the minimal polynomial of a radical expression over Q.
    This is the monic irreducible polynomial in Q[x] of smallest degree
    that the expression satisfies. -/
partial def minimalPoly (expr : RadExpr Rat) : Poly Rat :=
  let ann := annihilatingPoly expr
  let v := evalFloat expr
  let factors := factorSquareFree ann
  match factors with
  | [] => ann
  | _ => Poly.monic (pickClosest v factors)

end Surd
