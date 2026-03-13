/-
  Surd.Integration.Elliptic — Reduction of elliptic integrals to Legendre
  normal forms (F, E, Π).

  Given ∫ R(x) dx / √P(x) where P(x) has degree 3 or 4 with all real roots,
  expresses the result in terms of incomplete elliptic integrals.
-/
import Surd.Poly.Univariate
import Surd.Poly.Factoring
import Surd.Radical.Expr
import Surd.Radical.Normalize
import Surd.Radical.Pretty
import Surd.Radical.LaTeX
import Surd.Radical.NormalForm
import Surd.Algebraic.Number
import Surd.Algebraic.Convert
import Surd.Algebraic.RootIsolation
import Surd.Interval
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

/-- Which kind of Legendre elliptic integral. -/
inductive LegendreKind where
  | firstKind : LegendreKind
  | secondKind : LegendreKind
  | thirdKind : LegendreKind
  deriving Inhabited, BEq

/-- Expression for the amplitude φ = arcsin(√((x - shift) · scale)). -/
structure AmplitudeExpr where
  aeShift : RadExpr Rat
  aeScale : RadExpr Rat
  deriving Inhabited

/-- A single Legendre form term: coeff · F/E/Π(φ, [n,] k). -/
structure LegendreForm where
  lfKind : LegendreKind
  lfCoeff : RadExpr Rat
  lfAmplitude : AmplitudeExpr
  lfParameter : Option (RadExpr Rat)
  lfModulus : RadExpr Rat
  deriving Inhabited

/-- An elliptic integrand: R(x) dx / √P(x). -/
structure EllipticIntegrand where
  eiNum : Poly Rat
  eiDen : Poly Rat
  eiRadicand : Poly Rat
  deriving Inhabited

/-- Result of elliptic reduction. -/
structure EllipticResult where
  erTerms : List LegendreForm
  erModulus : RadExpr Rat
  erModulusSq : RadExpr Rat
  erRoots : List (RadExpr Rat)
  erLeadCoeff : Rat
  erDegree : Nat
  erJacobi : Bool
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Root finding
-- ---------------------------------------------------------------------------

private def algToDouble (a : AlgNum) : Float :=
  let toRat (n : Int) : Rat := n
  let eps : Rat := toRat 1 / toRat (10 ^ 15)
  ratToFloat (algApprox eps a)

private def isoToAlgRad (ii : IsolatingInterval) : Option (AlgNum × RadExpr Rat) :=
  match rootInInterval ii with
  | some r => some (algFromRational r, RadExpr.lit r)
  | none =>
    let p := ii.iiPoly
    let lo := ii.iiInterval.lo
    let hi := ii.iiInterval.hi
    let approx := ratToFloat ((lo + hi) / 2)
    match algFromPoly p approx with
    | some alg =>
      match algNumToRadExpr alg with
      | some rad => some (alg, normalize rad)
      | none => none
    | none => none

private def rootsOfFactor (fac : Poly Rat × Int) : Option (List (AlgNum × RadExpr Rat)) :=
  let (f, _) := fac
  let d := f.coeffs.size - 1
  if d == 0 then some []
  else if d == 1 then
    let c := f.coeffs.toList.head!
    let r := -c
    some [(algFromRational r, RadExpr.lit r)]
  else
    let iis := isolateRealRoots f
    iis.mapM isoToAlgRad

/-- Find all real roots of P(x) in descending order. -/
private def findRealRoots (p : Poly Rat) : Option (List (AlgNum × RadExpr Rat)) := do
  let d := p.coeffs.size - 1
  if d != 3 && d != 4 then failure
  let facs := factorPoly p
  let roots ← facs.mapM fun fac => rootsOfFactor fac
  let allRoots := roots.flatten
  if allRoots.length != d then failure
  let sorted := allRoots.mergeSort fun a b =>
    algToDouble a.1 ≥ algToDouble b.1
  pure sorted

-- ---------------------------------------------------------------------------
-- Modulus computation
-- ---------------------------------------------------------------------------

private def cubicModulus (roots : List (AlgNum × RadExpr Rat))
    : Option (AlgNum × RadExpr Rat) := do
  match roots with
  | [(e1a, _), (e2a, _), (e3a, _)] =>
    let k2alg := algDiv (algSub e2a e3a) (algSub e1a e3a)
    let k2rad ← algNumToRadExpr k2alg
    pure (k2alg, normalize k2rad)
  | _ => failure

private def quarticModulus (roots : List (AlgNum × RadExpr Rat))
    : Option (AlgNum × RadExpr Rat) := do
  match roots with
  | [(e1a, _), (e2a, _), (e3a, _), (e4a, _)] =>
    let num := algMul (algSub e2a e3a) (algSub e1a e4a)
    let den := algMul (algSub e1a e3a) (algSub e2a e4a)
    let k2alg := algDiv num den
    let k2rad ← algNumToRadExpr k2alg
    pure (k2alg, normalize k2rad)
  | _ => failure

private def computeModulusSq (roots : List (AlgNum × RadExpr Rat))
    : Option (AlgNum × RadExpr Rat) :=
  match roots.length with
  | 3 => cubicModulus roots
  | 4 => quarticModulus roots
  | _ => none

private def modulusFromSq (k2 : RadExpr Rat) : RadExpr Rat :=
  normalize (RadExpr.root 2 k2)

private def simplifyRad (e : RadExpr Rat) : RadExpr Rat :=
  simplifyViaCanonical (normalize (fromNormExpr (toNormExpr e)))

-- ---------------------------------------------------------------------------
-- Reduction to Legendre forms
-- ---------------------------------------------------------------------------

private def reduceDxOverSqrtP (c : Rat) (roots : List (AlgNum × RadExpr Rat))
    (lc : Rat) (d : Nat) (_k2rad kRad : RadExpr Rat)
    : Option (List LegendreForm) := do
  if d == 3 then do
    let e1a := (roots.get! 0).1
    let e2r := (roots.get! 1).2
    let e3a := (roots.get! 2).1
    let e3r := (roots.get! 2).2
    let diff13 := algSub e1a e3a
    let diff13rad ← algNumToRadExpr diff13
    let scaleSq := RadExpr.mul (RadExpr.lit lc) (normalize diff13rad)
    let scale := normalize (RadExpr.root 2 scaleSq)
    let coeff := simplifyRad (RadExpr.mul (RadExpr.lit (2 * c)) (RadExpr.inv scale))
    let diff23 := normalize (RadExpr.add (RadExpr.neg e3r) e2r)
    let amp : AmplitudeExpr := ⟨e3r, normalize (RadExpr.inv diff23)⟩
    pure [⟨LegendreKind.firstKind, coeff, amp, none, kRad⟩]
  else if d == 4 then do
    let e1a := (roots.get! 0).1
    let e2a := (roots.get! 1).1
    let e2r := (roots.get! 1).2
    let e3a := (roots.get! 2).1
    let e3r := (roots.get! 2).2
    let e4a := (roots.get! 3).1
    let diff13 := algSub e1a e3a
    let diff24 := algSub e2a e4a
    let diff13rad ← algNumToRadExpr diff13
    let diff24rad ← algNumToRadExpr diff24
    let scaleSq := RadExpr.mul (RadExpr.lit lc)
                    (RadExpr.mul (normalize diff13rad) (normalize diff24rad))
    let scale := normalize (RadExpr.root 2 scaleSq)
    let coeff := simplifyRad (RadExpr.mul (RadExpr.lit (2 * c)) (RadExpr.inv scale))
    let diff23 := normalize (RadExpr.add (RadExpr.neg e3r) e2r)
    let amp : AmplitudeExpr := ⟨e3r, normalize (RadExpr.inv diff23)⟩
    pure [⟨LegendreKind.firstKind, coeff, amp, none, kRad⟩]
  else failure

private def reduceSqrtP (c : Rat) (roots : List (AlgNum × RadExpr Rat))
    (lc : Rat) (d : Nat) (k2rad kRad : RadExpr Rat)
    : Option (List LegendreForm) := do
  if d == 3 then do
    let e1a := (roots.get! 0).1
    let e2a := (roots.get! 1).1
    let e2r := (roots.get! 1).2
    let e3a := (roots.get! 2).1
    let e3r := (roots.get! 2).2
    let diff13 := algSub e1a e3a
    let diff23 := algSub e2a e3a
    let diff13rad ← algNumToRadExpr diff13
    let diff23rad ← algNumToRadExpr diff23
    let d13 := normalize diff13rad
    let d23 := normalize diff23rad
    let sqrtScale := normalize (RadExpr.root 2 (RadExpr.mul (RadExpr.lit lc) d13))
    let coeffE := simplifyRad (RadExpr.mul (RadExpr.lit (2 * c / 3))
                               (RadExpr.mul sqrtScale d23))
    let oneMinusK2 := simplifyRad (RadExpr.add (RadExpr.lit 1) (RadExpr.neg k2rad))
    let coeffF := simplifyRad (RadExpr.mul (RadExpr.neg (RadExpr.lit (2 * c / 3)))
                               (RadExpr.mul sqrtScale (RadExpr.mul d23 oneMinusK2)))
    let diff23' := normalize (RadExpr.add (RadExpr.neg e3r) e2r)
    let amp : AmplitudeExpr := ⟨e3r, normalize (RadExpr.inv diff23')⟩
    pure [ ⟨LegendreKind.secondKind, coeffE, amp, none, kRad⟩
         , ⟨LegendreKind.firstKind, coeffF, amp, none, kRad⟩ ]
  else failure

private def reducePiTerm (c : Rat) (pole : Rat)
    (roots : List (AlgNum × RadExpr Rat))
    (lc : Rat) (d : Nat) (_k2rad kRad : RadExpr Rat)
    : Option (List LegendreForm) := do
  if d == 3 then do
    let e1a := (roots.get! 0).1
    let e2a := (roots.get! 1).1
    let e2r := (roots.get! 1).2
    let e3a := (roots.get! 2).1
    let e3r := (roots.get! 2).2
    let poleAlg := algFromRational pole
    let diff13 := algSub e1a e3a
    let diff23 := algSub e2a e3a
    let diffPoleE3 := algSub poleAlg e3a
    let diff13rad ← algNumToRadExpr diff13
    let diffPE3rad ← algNumToRadExpr diffPoleE3
    let nAlg := algDiv diff23 diffPoleE3
    let nRad ← algNumToRadExpr nAlg
    let scaleSq := RadExpr.mul (RadExpr.lit lc) (normalize diff13rad)
    let scale := normalize (RadExpr.root 2 scaleSq)
    let coeffDen := normalize (RadExpr.mul scale (normalize diffPE3rad))
    let coeff := simplifyRad (RadExpr.mul (RadExpr.lit (2 * c)) (RadExpr.inv coeffDen))
    let diff23' := normalize (RadExpr.add (RadExpr.neg e3r) e2r)
    let amp : AmplitudeExpr := ⟨e3r, normalize (RadExpr.inv diff23')⟩
    pure [⟨LegendreKind.thirdKind, coeff, amp, some (normalize nRad), kRad⟩]
  else failure

/-- Reduce an elliptic integral to Legendre normal forms. -/
partial def reduceElliptic (jacobi : Bool) (ei : EllipticIntegrand)
    : Option EllipticResult := do
  let p := ei.eiRadicand
  let d := p.coeffs.size - 1
  let lc := p.coeffs.toList.getLast!
  let roots ← findRealRoots p
  let (_, k2raw) ← computeModulusSq roots
  let k2rad := simplifyRad k2raw
  let kRad := simplifyRad (modulusFromSq k2rad)
  let num := ei.eiNum; let den := ei.eiDen
  let dNum := num.coeffs.size - 1; let dDen := den.coeffs.size - 1
  let terms ←
    if dNum == 0 && dDen == 0 then
      let c := match (num.coeffs.toList.head?, den.coeffs.toList.head?) with
        | (some n, some dd) => n / dd | _ => 1
      reduceDxOverSqrtP c roots lc d k2rad kRad
    else if dDen == 1 && dNum == 0 then
      let denCs := den.coeffs.toList
      let negA := denCs.get! 0; let s := denCs.get! 1
      let pole := -negA / s
      let c := match num.coeffs.toList.head? with
        | some n => n / s | _ => 1 / s
      reducePiTerm c pole roots lc d k2rad kRad
    else failure
  pure ⟨terms, kRad, k2rad, roots.map (·.2), lc, d, jacobi⟩

-- ---------------------------------------------------------------------------
-- Rendering helpers
-- ---------------------------------------------------------------------------

private def intercalate_ (sep : String) : List String → String
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ intercalate_ sep xs

private def prettyAmpInner (ae : AmplitudeExpr) : String :=
  match ae.aeShift with
  | RadExpr.lit r =>
    if r == 0 then "x"
    else if r < 0 then "x + " ++ pretty (RadExpr.lit (-r))
    else "x - " ++ pretty ae.aeShift
  | e => "(x - " ++ pretty e ++ ")"

private def latexAmpInner (ae : AmplitudeExpr) : String :=
  match ae.aeShift with
  | RadExpr.lit r =>
    if r == 0 then "x"
    else if r < 0 then "x + " ++ latex (RadExpr.lit (-r))
    else "x - " ++ latex ae.aeShift
  | e => "x - " ++ latex e

-- ---------------------------------------------------------------------------
-- Rendering: text
-- ---------------------------------------------------------------------------

private def prettyLegendreForm (jacobi : Bool) (lf : LegendreForm) : String :=
  let c := pretty lf.lfCoeff
  let k := pretty lf.lfModulus
  let phi := "arcsin(√(" ++ prettyAmpInner lf.lfAmplitude ++ "))"
  match lf.lfKind with
  | LegendreKind.firstKind =>
    if jacobi then c ++ " · sn⁻¹(√(" ++ prettyAmpInner lf.lfAmplitude ++ "), " ++ k ++ ")"
    else c ++ " · F(" ++ phi ++ ", " ++ k ++ ")"
  | LegendreKind.secondKind => c ++ " · E(" ++ phi ++ ", " ++ k ++ ")"
  | LegendreKind.thirdKind =>
    let n := match lf.lfParameter with | some p => pretty p | none => "?"
    c ++ " · Π(" ++ phi ++ ", " ++ n ++ ", " ++ k ++ ")"

def prettyEllipticResult (er : EllipticResult) : String :=
  let rootStrs := er.erRoots.map pretty
  let lines := [
    "Roots of P(x) (descending): " ++ intercalate_ ", " rootStrs,
    "Modulus: k = " ++ pretty er.erModulus,
    "        k² = " ++ pretty er.erModulusSq,
    "",
    "Reduction:"
  ] ++ er.erTerms.map fun lf => "  " ++ prettyLegendreForm er.erJacobi lf
  "\n".intercalate lines

-- ---------------------------------------------------------------------------
-- Rendering: LaTeX
-- ---------------------------------------------------------------------------

private def latexLegendreForm (jacobi : Bool) (lf : LegendreForm) : String :=
  let c := latex lf.lfCoeff
  let k := latex lf.lfModulus
  let phi := "\\arcsin\\sqrt{" ++ latexAmpInner lf.lfAmplitude ++ "}"
  match lf.lfKind with
  | LegendreKind.firstKind =>
    if jacobi then
      c ++ " \\operatorname{sn}^{-1}\\!\\left(\\sqrt{" ++ latexAmpInner lf.lfAmplitude ++ "}, " ++ k ++ "\\right)"
    else c ++ " F\\!\\left(" ++ phi ++ ", " ++ k ++ "\\right)"
  | LegendreKind.secondKind => c ++ " E\\!\\left(" ++ phi ++ ", " ++ k ++ "\\right)"
  | LegendreKind.thirdKind =>
    let n := match lf.lfParameter with | some p => latex p | none => "?"
    c ++ " \\Pi\\!\\left(" ++ phi ++ ", " ++ n ++ ", " ++ k ++ "\\right)"

def latexEllipticResult (er : EllipticResult) : String :=
  let rootStrs := er.erRoots.map latex
  let lines := [
    "\\text{Roots of } P(x) \\text{ (descending): } " ++ intercalate_ ", " rootStrs,
    "k = " ++ latex er.erModulus ++ ", \\quad k^2 = " ++ latex er.erModulusSq,
    "",
    "\\text{Reduction:}"
  ] ++ er.erTerms.map fun lf => "  " ++ latexLegendreForm er.erJacobi lf
  "\n".intercalate lines

end Surd
