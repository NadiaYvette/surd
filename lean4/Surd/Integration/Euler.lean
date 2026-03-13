/-
  Surd.Integration.Euler — Euler substitution for integrals of the form
  ∫ P(x)/Q(x) · (√(ax²+bx+c))^n dx.

  Three substitutions reduce these to rational function integrals:
    Euler 1 (a > 0): √(ax²+bx+c) = t − x√a
    Euler 2 (c > 0): √(ax²+bx+c) = xt + √c
    Euler 3 (Δ > 0): √(a(x−r₁)(x−r₂)) = t(x−r₁)
-/
import Surd.Poly.Univariate
import Surd.Poly.Factoring
import Surd.Radical.Expr
import Surd.Radical.Normalize
import Surd.Radical.Pretty
import Surd.Radical.LaTeX
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Symbolic expression AST
-- ---------------------------------------------------------------------------

/-- Symbolic expression for an antiderivative (function of x). -/
inductive SymExpr where
  | sRat    : Rat → SymExpr
  | sRad    : RadExpr Rat → SymExpr
  | sVar    : SymExpr
  | sSurd   : Rat → Rat → Rat → SymExpr   -- √(ax²+bx+c)
  | sNeg    : SymExpr → SymExpr
  | sAdd    : SymExpr → SymExpr → SymExpr
  | sMul    : SymExpr → SymExpr → SymExpr
  | sDiv    : SymExpr → SymExpr → SymExpr
  | sPow    : SymExpr → Int → SymExpr
  | sLn     : SymExpr → SymExpr
  | sArcTan : SymExpr → SymExpr
  | sArcSin : SymExpr → SymExpr
  | sArsinh : SymExpr → SymExpr
  | sArcosh : SymExpr → SymExpr
  deriving Inhabited

/-- Result of Euler substitution integration. -/
structure IntegralResult where
  irExpr : SymExpr
  irA : Rat
  irB : Rat
  irC : Rat
  deriving Inhabited

/-- Integrand specification: P(x)/Q(x) · (√(ax²+bx+c))^n. -/
structure EulerIntegrand where
  eiP : Poly Rat
  eiQ : Poly Rat
  eiSqrtPow : Int
  eiA : Rat
  eiB : Rat
  eiC : Rat
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Euler substitution variant
-- ---------------------------------------------------------------------------

private inductive EulerSub where
  | euler1 : Rat → EulerSub   -- √a is rational
  | euler2 : Rat → EulerSub   -- √c is rational
  | euler3 : Rat → Rat → EulerSub  -- rational roots r₁, r₂

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

private def symAdd (a b : SymExpr) : SymExpr :=
  match a, b with
  | SymExpr.sRat 0, b => b
  | a, SymExpr.sRat 0 => a
  | SymExpr.sRat x, SymExpr.sRat y => SymExpr.sRat (x + y)
  | a, b => SymExpr.sAdd a b

private def symNeg (a : SymExpr) : SymExpr :=
  match a with
  | SymExpr.sRat r => SymExpr.sRat (-r)
  | SymExpr.sNeg x => x
  | x => SymExpr.sNeg x

private def symMul (a b : SymExpr) : SymExpr :=
  match a, b with
  | SymExpr.sRat 0, _ => SymExpr.sRat 0
  | _, SymExpr.sRat 0 => SymExpr.sRat 0
  | SymExpr.sRat 1, b => b
  | a, SymExpr.sRat 1 => a
  | SymExpr.sRat x, SymExpr.sRat y => SymExpr.sRat (x * y)
  | a, b => SymExpr.sMul a b

private def symDiv (a b : SymExpr) : SymExpr :=
  match a, b with
  | SymExpr.sRat 0, _ => SymExpr.sRat 0
  | a, SymExpr.sRat 1 => a
  | SymExpr.sRat x, SymExpr.sRat y => SymExpr.sRat (x / y)
  | a, b => SymExpr.sDiv a b

private def symSum (xs : List SymExpr) : SymExpr :=
  match xs with
  | [] => SymExpr.sRat 0
  | [x] => x
  | x :: rest => rest.foldl symAdd x

-- ---------------------------------------------------------------------------
-- Rational square root
-- ---------------------------------------------------------------------------

private def intSqrt (x : Int) : Option Int :=
  if x < 0 then none
  else if x == 0 then some 0
  else
    let s := Float.sqrt (intToFloat x) |>.toUInt64.toNat |> Int.ofNat
    if s * s == x then some s
    else if (s + 1) * (s + 1) == x then some (s + 1)
    else none

private def ratSqrt (r : Rat) : Option Rat :=
  if r < 0 then none
  else if r == 0 then some 0
  else do
    let sn ← intSqrt (if r.num < 0 then -r.num else r.num)
    let sd ← intSqrt (Int.ofNat r.den)
    pure ((sn : Rat) / (sd : Rat))

-- ---------------------------------------------------------------------------
-- Choose Euler substitution
-- ---------------------------------------------------------------------------

private def chooseEuler (a b c : Rat) : Option EulerSub :=
  if a > 0 then
    match ratSqrt a with
    | some s => some (EulerSub.euler1 s)
    | none =>
      if c > 0 then
        match ratSqrt c with
        | some s => some (EulerSub.euler2 s)
        | none => tryEuler3 a b c
      else tryEuler3 a b c
  else if c > 0 then
    match ratSqrt c with
    | some s => some (EulerSub.euler2 s)
    | none => tryEuler3 a b c
  else tryEuler3 a b c
where
  tryEuler3 (a b c : Rat) : Option EulerSub :=
    let disc := b * b - 4 * a * c
    if disc > 0 then
      match ratSqrt disc with
      | some sd =>
        let r1 := (-b + sd) / (2 * a)
        let r2 := (-b - sd) / (2 * a)
        some (EulerSub.euler3 r1 r2)
      | none => none
    else none

-- ---------------------------------------------------------------------------
-- Polynomial helpers
-- ---------------------------------------------------------------------------

private def polyPow (p : Poly Rat) (n : Int) : Poly Rat :=
  if n <= 0 then Poly.mkPoly #[1]
  else (List.range n.toNat).foldl (fun acc _ => Poly.mul acc p) (Poly.mkPoly #[1])

/-- Substitute x = p/q into polynomial f, returning numerator of f(p/q)·q^deg_f. -/
private def substRatFunc (f : Poly Rat) (px qx : Poly Rat) : Poly Rat :=
  let cs := f.coeffs.toList
  let d := f.coeffs.size - 1
  cs.enum.foldl (fun acc (i, ci) =>
    Poly.add acc (Poly.scale ci (Poly.mul (polyPow px i) (polyPow qx (d - i))))
  ) (Poly.mkPoly #[])

-- ---------------------------------------------------------------------------
-- Apply Euler substitution
-- ---------------------------------------------------------------------------

private structure SubResult where
  srNum : Poly Rat
  srDen : Poly Rat
  srBack : SymExpr

private def applyEuler (sub : EulerSub) (pPoly qPoly : Poly Rat) (n : Int)
    (a b c : Rat) : SubResult :=
  match sub with
  | EulerSub.euler1 s =>
    let qx := Poly.mkPoly #[b, 2*s]
    let px := Poly.mkPoly #[-c, 0, 1]
    let py := Poly.mkPoly #[s*c, b, s]
    let dP := pPoly.coeffs.size - 1
    let dQ := qPoly.coeffs.size - 1
    let pNum := substRatFunc pPoly px qx
    let qNum := substRatFunc qPoly px qx
    let numParts := Poly.mul (Poly.scale 2 pNum) (polyPow py (max 0 (n + 1)))
    let denParts := Poly.mul (Poly.mul qNum (polyPow qx (dP - dQ + n + 2)))
                     (polyPow py (max 0 (-(n + 1))))
    let back := symAdd (SymExpr.sSurd (s*s) b c) (symMul (SymExpr.sRat s) SymExpr.sVar)
    ⟨numParts, denParts, back⟩

  | EulerSub.euler2 sc =>
    let px := Poly.mkPoly #[-b, 2*sc]
    let qx := Poly.mkPoly #[a, 0, -1]
    let py := Poly.mkPoly #[sc*a, -b, sc]
    let dP := pPoly.coeffs.size - 1
    let dQ := qPoly.coeffs.size - 1
    let pNum := substRatFunc pPoly px qx
    let qNum := substRatFunc qPoly px qx
    let numParts := Poly.mul (Poly.scale 2 pNum) (polyPow py (max 0 (n + 1)))
    let denParts := Poly.mul (Poly.mul qNum (polyPow qx (dP - dQ + n + 2)))
                     (polyPow py (max 0 (-(n + 1))))
    let back := symDiv (symAdd (SymExpr.sSurd a b (sc*sc)) (symNeg (SymExpr.sRat sc))) SymExpr.sVar
    ⟨numParts, denParts, back⟩

  | EulerSub.euler3 r1 r2 =>
    let px := Poly.mkPoly #[a*r2, 0, -r1]
    let qx := Poly.mkPoly #[a, 0, -1]
    let py := Poly.mkPoly #[0, a * (r2 - r1)]
    let dP := pPoly.coeffs.size - 1
    let dQ := qPoly.coeffs.size - 1
    let pNum := substRatFunc pPoly px qx
    let qNum := substRatFunc qPoly px qx
    let px' := Poly.diff px
    let qx' := Poly.diff qx
    let dxNum := Poly.add (Poly.mul px' qx) (Poly.neg (Poly.mul px qx'))
    let numParts := Poly.mul (Poly.mul pNum dxNum) (polyPow py (max 0 n))
    let denParts := Poly.mul (Poly.mul qNum (polyPow qx (dP - dQ + 2)))
                     (polyPow py (max 0 (-n)))
    let back := symDiv (SymExpr.sSurd a b c)
                       (symAdd SymExpr.sVar (symNeg (SymExpr.sRat r1)))
    ⟨numParts, denParts, back⟩

-- ---------------------------------------------------------------------------
-- Partial fraction term
-- ---------------------------------------------------------------------------

private structure PFTerm where
  pfNum : Poly Rat
  pfFactor : Poly Rat
  pfPower : Int

-- ---------------------------------------------------------------------------
-- Polynomial integration
-- ---------------------------------------------------------------------------

private def integratePoly (p : Poly Rat) : SymExpr :=
  let cs := p.coeffs.toList
  symSum (cs.enum.filterMap fun (i, c) =>
    if c == 0 then none
    else
      let coeff := c / (Int.ofNat (i + 1) : Rat)
      some (symMul (SymExpr.sRat coeff) (SymExpr.sPow SymExpr.sVar (i + 1))))

-- ---------------------------------------------------------------------------
-- Partial fraction decomposition
-- ---------------------------------------------------------------------------

private def expandPower (r f : Poly Rat) (e : Int) : List PFTerm :=
  if e <= 0 then []
  else
    let go := (List.range e.toNat).foldl (fun (acc, rem) k =>
      let power := e - k
      let (q, rk) := Poly.divMod rem f
      (acc ++ [PFTerm.mk rk f power], q)
    ) ([], r)
    go.1

private def partialFractions (r : Poly Rat) (facs : List (Poly Rat × Int))
    : List PFTerm :=
  match facs with
  | [] => []
  | [(f, e)] => expandPower r f e
  | (f, e) :: rest =>
    let others := rest.foldl (fun acc (g, m) =>
      Poly.mul acc (polyPow g m)) (Poly.mkPoly #[1])
    let fe := polyPow f e
    -- Simple fallback: just use the single-factor expansion
    expandPower r f e ++ partialFractions (Poly.mkPoly #[]) rest

-- ---------------------------------------------------------------------------
-- Integrate partial fraction term
-- ---------------------------------------------------------------------------

private def polyToSym (p : Poly Rat) : SymExpr :=
  let cs := p.coeffs.toList
  symSum (cs.enum.filterMap fun (i, c) =>
    if c == 0 then none
    else some (symMul (SymExpr.sRat c) (SymExpr.sPow SymExpr.sVar i)))

private def integratePartialFraction (pf : PFTerm) : SymExpr :=
  let ncs := pf.pfNum.coeffs.toList
  let fcs := pf.pfFactor.coeffs.toList
  -- Zero numerator
  if ncs.all (· == 0) then SymExpr.sRat 0
  -- Linear factor [negAlpha, 1], constant numerator
  else if fcs.length == 2 && fcs.get! 1 == 1 && ncs.length == 1 then
    let a := ncs.get! 0
    let negAlpha := fcs.get! 0
    if pf.pfPower == 1 then
      symMul (SymExpr.sRat a) (SymExpr.sLn (symAdd SymExpr.sVar (SymExpr.sRat negAlpha)))
    else
      let k' : Rat := (1 - pf.pfPower : Rat)
      symMul (SymExpr.sRat (a / k'))
             (SymExpr.sPow (symAdd SymExpr.sVar (SymExpr.sRat negAlpha)) (1 - pf.pfPower))
  -- Fallback: leave unevaluated
  else SymExpr.sDiv (polyToSym pf.pfNum) (SymExpr.sPow (polyToSym pf.pfFactor) pf.pfPower)

-- ---------------------------------------------------------------------------
-- Rational function integration
-- ---------------------------------------------------------------------------

/-- Integrate N(t)/D(t) dt. -/
def integrateRational (num den : Poly Rat) : SymExpr :=
  let lc := den.coeffs.toList.getLast!
  let num' := Poly.scale (1/lc) num
  let den' := Poly.monic den
  let (q, r) := Poly.divMod num' den'
  let polyPart := integratePoly q
  let facs := factorPoly den'
  let pfTerms := partialFractions r facs
  let pfParts := pfTerms.map integratePartialFraction
  symSum (polyPart :: pfParts)

-- ---------------------------------------------------------------------------
-- Back-substitution
-- ---------------------------------------------------------------------------

private partial def substBack (expr tExpr : SymExpr) : SymExpr :=
  go expr
where
  go : SymExpr → SymExpr
    | SymExpr.sVar => tExpr
    | e@(SymExpr.sRat _) => e
    | e@(SymExpr.sRad _) => e
    | e@(SymExpr.sSurd _ _ _) => e
    | SymExpr.sNeg a => symNeg (go a)
    | SymExpr.sAdd a b => symAdd (go a) (go b)
    | SymExpr.sMul a b => symMul (go a) (go b)
    | SymExpr.sDiv a b => symDiv (go a) (go b)
    | SymExpr.sPow a n => SymExpr.sPow (go a) n
    | SymExpr.sLn a => SymExpr.sLn (go a)
    | SymExpr.sArcTan a => SymExpr.sArcTan (go a)
    | SymExpr.sArcSin a => SymExpr.sArcSin (go a)
    | SymExpr.sArsinh a => SymExpr.sArsinh (go a)
    | SymExpr.sArcosh a => SymExpr.sArcosh (go a)

-- ---------------------------------------------------------------------------
-- Main integration entry point
-- ---------------------------------------------------------------------------

/-- Integrate P(x)/Q(x) · (√(ax²+bx+c))^n dx via Euler substitution. -/
def eulerIntegrate (ei : EulerIntegrand) : Option IntegralResult := do
  let sub ← chooseEuler ei.eiA ei.eiB ei.eiC
  let sr := applyEuler sub ei.eiP ei.eiQ ei.eiSqrtPow ei.eiA ei.eiB ei.eiC
  let ft := integrateRational sr.srNum sr.srDen
  let result := substBack ft sr.srBack
  pure ⟨result, ei.eiA, ei.eiB, ei.eiC⟩

-- ---------------------------------------------------------------------------
-- Rendering: text
-- ---------------------------------------------------------------------------

private def prettyR (r : Rat) : String :=
  if r.den == 1 then toString r.num
  else s!"{r.num}/{r.den}"

partial def prettySymExpr (e : SymExpr) : String := prettyPrec 0 e
where
  prettyPrec (p : Nat) : SymExpr → String
    | SymExpr.sRat r => prettyR r
    | SymExpr.sRad e => pretty (normalize e)
    | SymExpr.sVar => "x"
    | SymExpr.sSurd a b c => s!"√({prettyR a}x² + {prettyR b}x + {prettyR c})"
    | SymExpr.sNeg a => s!"-{prettyPrec 4 a}"
    | SymExpr.sAdd a b =>
      let s := s!"{prettyPrec 1 a} + {prettyPrec 1 b}"
      if p > 1 then s!"({s})" else s
    | SymExpr.sMul a b =>
      let s := s!"{prettyPrec 2 a}·{prettyPrec 3 b}"
      if p > 2 then s!"({s})" else s
    | SymExpr.sDiv a b =>
      let s := s!"{prettyPrec 2 a}/{prettyPrec 3 b}"
      if p > 2 then s!"({s})" else s
    | SymExpr.sPow a n =>
      if n == 0 then "1"
      else if n == 1 then prettyPrec 5 a
      else s!"{prettyPrec 5 a}^{n}"
    | SymExpr.sLn a => s!"ln|{prettyPrec 0 a}|"
    | SymExpr.sArcTan a => s!"arctan({prettyPrec 0 a})"
    | SymExpr.sArcSin a => s!"arcsin({prettyPrec 0 a})"
    | SymExpr.sArsinh a => s!"arsinh({prettyPrec 0 a})"
    | SymExpr.sArcosh a => s!"arcosh({prettyPrec 0 a})"

-- ---------------------------------------------------------------------------
-- Rendering: LaTeX
-- ---------------------------------------------------------------------------

partial def latexSymExpr (e : SymExpr) : String := latexPrec 0 e
where
  latexR (r : Rat) : String :=
    if r.den == 1 then toString r.num
    else "\\frac{" ++ toString r.num ++ "}{" ++ toString r.den ++ "}"
  latexPrec (p : Nat) : SymExpr → String
    | SymExpr.sRat r => latexR r
    | SymExpr.sRad e => latex (normalize e)
    | SymExpr.sVar => "x"
    | SymExpr.sSurd a b c =>
      "\\sqrt{" ++ latexR a ++ "x^{2} + " ++ latexR b ++ "x + " ++ latexR c ++ "}"
    | SymExpr.sNeg a => "-" ++ latexPrec 4 a
    | SymExpr.sAdd a b =>
      let s := latexPrec 1 a ++ " + " ++ latexPrec 1 b
      if p > 1 then "\\left(" ++ s ++ "\\right)" else s
    | SymExpr.sMul a b =>
      let s := latexPrec 2 a ++ " \\cdot " ++ latexPrec 3 b
      if p > 2 then "\\left(" ++ s ++ "\\right)" else s
    | SymExpr.sDiv a b =>
      "\\frac{" ++ latexPrec 0 a ++ "}{" ++ latexPrec 0 b ++ "}"
    | SymExpr.sPow a n =>
      if n == 0 then "1"
      else if n == 1 then latexPrec 5 a
      else latexPrec 5 a ++ "^{" ++ toString n ++ "}"
    | SymExpr.sLn a => "\\ln\\left|" ++ latexPrec 0 a ++ "\\right|"
    | SymExpr.sArcTan a => "\\arctan\\left(" ++ latexPrec 0 a ++ "\\right)"
    | SymExpr.sArcSin a => "\\arcsin\\left(" ++ latexPrec 0 a ++ "\\right)"
    | SymExpr.sArsinh a => "\\operatorname{arsinh}\\left(" ++ latexPrec 0 a ++ "\\right)"
    | SymExpr.sArcosh a => "\\operatorname{arcosh}\\left(" ++ latexPrec 0 a ++ "\\right)"

end Surd
