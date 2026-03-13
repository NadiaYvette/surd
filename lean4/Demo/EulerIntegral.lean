/-
  Demo: Euler substitution integrals.

  Computes antiderivatives of ∫ P(x)/Q(x) · (√(ax²+bx+c))^n dx
  using Euler's three substitutions, with radical coefficients
  simplified by surd's normalization machinery.
-/
import Surd.Integration.Euler
import Surd.Poly.Univariate
import Surd.Rat

open Std.Internal
open Surd

structure Example where
  exName : String
  exIntgd : EulerIntegrand
  exKnown : String

def examples : List Example :=
  [ { exName := "∫ dx / √(x²+1)"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := 1, eiB := 0, eiC := 1 }
      exKnown := "ln|x + √(x²+1)|" }
  , { exName := "∫ dx / √(x²-1)"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := 1, eiB := 0, eiC := -1 }
      exKnown := "ln|x + √(x²-1)|" }
  , { exName := "∫ dx / √(x²+2x+2)"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := 1, eiB := 2, eiC := 2 }
      exKnown := "ln|x + 1 + √(x²+2x+2)|" }
  , { exName := "∫ x dx / √(x²+1)"
      exIntgd := { eiP := Poly.mkPoly #[0, 1], eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := 1, eiB := 0, eiC := 1 }
      exKnown := "√(x²+1)" }
  , { exName := "∫ dx / √(4x²+1)"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := 4, eiB := 0, eiC := 1 }
      exKnown := "(1/2)·ln|2x + √(4x²+1)|" }
  , { exName := "∫ dx / ((x+1)·√(x²+1))"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.mkPoly #[1, 1]
                   eiSqrtPow := -1, eiA := 1, eiB := 0, eiC := 1 }
      exKnown := "..." }
  , { exName := "∫ √(x²+1) dx"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := 1, eiA := 1, eiB := 0, eiC := 1 }
      exKnown := "(x·√(x²+1) + ln|x + √(x²+1)|) / 2" }
  , { exName := "∫ dx / √(1-x²)  [Euler 2]"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := -1, eiB := 0, eiC := 1 }
      exKnown := "arcsin(x)" }
  , { exName := "∫ dx / √(x²-3x+2)  [Euler 3]"
      exIntgd := { eiP := Poly.const 1, eiQ := Poly.const 1
                   eiSqrtPow := -1, eiA := 1, eiB := -3, eiC := 2 }
      exKnown := "ln|x - 3/2 + √(x²-3x+2)|" }
  ]

def renderExample (fmt : String) (ex : Example) : IO Unit := do
  IO.println s!"─── {ex.exName} ───"
  match eulerIntegrate ex.exIntgd with
  | none => IO.println "  (no rational Euler substitution found)"
  | some result =>
    match fmt with
    | "latex" => do
      IO.println s!"  = {latexSymExpr result.irExpr}"
      IO.println s!"  Known: {ex.exKnown}"
    | _ => do
      IO.println s!"  = {prettySymExpr result.irExpr}"
      IO.println s!"  Known: {ex.exKnown}"
  IO.println ""

def main (args : List String) : IO Unit := do
  let fmt := match args.head? with
    | some "latex" => "latex"
    | _ => "text"
  for ex in examples do
    renderExample fmt ex
