/-
  Demo: elliptic integral reduction to Legendre normal forms.

  Shows how ∫ R(x) dx / √P(x) with P degree 3 or 4 reduces to
  F(φ,k), E(φ,k), and Π(φ,n,k) with exact radical modulus k.
-/
import Surd.Integration.Elliptic
import Surd.Poly.Univariate
import Surd.Rat

open Std.Internal
open Surd

structure Example where
  exName : String
  exIntgd : EllipticIntegrand
  exNotes : String

private def ratFrac (n d : Int) : Rat := (n : Rat) / (d : Rat)

def examples : List Example :=
  [ { exName := "∫ dx / √(x³ - x)"
      exIntgd := { eiNum := Poly.const 1, eiDen := Poly.const 1
                   eiRadicand := Poly.mkPoly #[0, -1, 0, 1] }
      exNotes := "roots 1, 0, -1; k² = 1/2" }
  , { exName := "∫ dx / √(x³ - 1)"
      exIntgd := { eiNum := Poly.const 1, eiDen := Poly.const 1
                   eiRadicand := Poly.mkPoly #[-1, 0, 0, 1] }
      exNotes := "one rational root at 1, two complex — should fail (not all real)" }
  , { exName := "∫ dx / √((1-x²)(1-½x²))"
      exIntgd := { eiNum := Poly.const 1, eiDen := Poly.const 1
                   eiRadicand := Poly.mkPoly #[1, 0, ratFrac (-3) 2, 0, ratFrac 1 2] }
      exNotes := "already Legendre-like; roots ±1, ±√2; k² = 1/2" }
  , { exName := "∫ dx / √(4x³ - 4x)"
      exIntgd := { eiNum := Poly.const 1, eiDen := Poly.const 1
                   eiRadicand := Poly.mkPoly #[0, -4, 0, 4] }
      exNotes := "same roots as #1, leading coeff 4; k² = 1/2" }
  , { exName := "∫ dx / √((x²-1)(x²-4))"
      exIntgd := { eiNum := Poly.const 1, eiDen := Poly.const 1
                   eiRadicand := Poly.mkPoly #[4, 0, -5, 0, 1] }
      exNotes := "roots 2, 1, -1, -2; quartic" }
  , { exName := "∫ dx / ((x-3)·√(x³ - x))"
      exIntgd := { eiNum := Poly.const 1
                   eiDen := Poly.mkPoly #[-3, 1]
                   eiRadicand := Poly.mkPoly #[0, -1, 0, 1] }
      exNotes := "Π form: pole at x=3" }
  , { exName := "∫ dx / √(x³ - 7x + 6)"
      exIntgd := { eiNum := Poly.const 1, eiDen := Poly.const 1
                   eiRadicand := Poly.mkPoly #[6, -7, 0, 1] }
      exNotes := "roots 2, 1, -3; factors (x-1)(x-2)(x+3)" }
  ]

def renderExample (fmt : String) (jacobi : Bool) (ex : Example) : IO Unit := do
  IO.println s!"─── {ex.exName} ───"
  IO.println s!"  ({ex.exNotes})"
  match reduceElliptic jacobi ex.exIntgd with
  | none => IO.println "  (cannot reduce: not all roots are real, or degree unsupported)\n"
  | some result =>
    match fmt with
    | "latex" => IO.println (latexEllipticResult result)
    | _ => IO.println (prettyEllipticResult result)

def main (args : List String) : IO Unit := do
  let fmt := match args.head? with
    | some "latex" => "latex"
    | _ => "text"
  IO.println "══════ Legendre form ══════\n"
  for ex in examples do
    renderExample fmt false ex
  IO.println "\n══════ Jacobi inverse form ══════\n"
  match examples.get? 0 with
  | some ex0 => renderExample fmt true ex0
  | none => pure ()
  match examples.get? 4 with
  | some ex4 => renderExample fmt true ex4
  | none => pure ()
