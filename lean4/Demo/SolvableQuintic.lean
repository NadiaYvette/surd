/-
  Solvable polynomial solver demo.

  Demonstrates Galois-theoretic identification and radical tower construction
  for polynomials over Q. Classifies each polynomial's Galois group
  among the transitive subgroups of Sₙ, then — when the group is solvable
  — constructs explicit radical expressions for the roots.

  Supports degree 5 and all prime degrees.
-/
import Surd.Galois.Identify
import Surd.Galois.RadicalTower
import Surd.Galois.Solve
import Surd.Galois.TransitiveGroup
import Surd.Galois.Resolvent
import Surd.Poly.Univariate
import Surd.Radical.DAG
import Surd.Radical.Pretty
import Surd.Rat

open Std.Internal
open Surd

structure Example where
  exName : String
  exPoly : Poly Rat
  exNotes : String

def examples : List Example :=
  [ -- Degree 5 examples
    { exName := "x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1"
      exPoly := Poly.mkPoly #[1, 3, -3, -4, 1, 1]
      exNotes := "C5 -- minimal polynomial of 2cos(2pi/11)" }
  , { exName := "x^5 - 2"
      exPoly := Poly.mkPoly #[-2, 0, 0, 0, 0, 1]
      exNotes := "F20 -- Gal(Q(5th-root(2),zeta5)/Q) = Z/5 semi Z/4" }
  , { exName := "x^5 + 20x + 32"
      exPoly := Poly.mkPoly #[32, 20, 0, 0, 0, 1]
      exNotes := "D5 -- dihedral group of order 10" }
  , { exName := "x^5 - 4x + 2"
      exPoly := Poly.mkPoly #[2, -4, 0, 0, 0, 1]
      exNotes := "S5 -- irreducible by Eisenstein (not solvable)" }
  -- Degree 7 example (prime degree, cyclic)
  , { exName := "x^7 + x^6 - 6x^5 - 5x^4 + 10x^3 + 6x^2 - 4x - 1"
      exPoly := Poly.mkPoly #[-1, -4, 6, 10, -5, -6, 1, 1]
      exNotes := "Z7 -- minimal polynomial of 2cos(2pi/29), cyclic" }
  -- Degree 3 example (prime, always solvable)
  , { exName := "x^3 - 2"
      exPoly := Poly.mkPoly #[-2, 0, 0, 1]
      exNotes := "S3 -- cube root of 2" }
  ]

def runExample (ex : Example) : IO Unit := do
  IO.println s!"--- {ex.exName} ---"
  IO.println s!"  ({ex.exNotes})"

  let f := ex.exPoly
  let deg := f.coeffs.size - 1
  let disc := discriminantOf f
  let discSq := isSquareRational disc

  IO.println s!"  degree: {deg}"
  IO.println s!"  disc(f) = {disc}"
  IO.println s!"  disc is square: {discSq}"

  match identifyGaloisGroup f with
  | none => IO.println "  (Galois group identification failed)\n"
  | some gr => do
    let tg := gr.grGroup
    IO.println s!"  Galois group: {tg.tgName} (order {tg.tgOrder})"
    IO.println s!"  Solvable: {tg.tgSolvable}"

    -- Show numerical roots
    let roots := gr.grRoots
    let realRoots := roots.filter fun r => Float.abs r.im < 1e-6
    IO.println s!"  Real roots: {realRoots.length}"
    for r in realRoots do
      IO.println s!"    {r.re}"

    if tg.tgSolvable then
      match solveViaTowerN gr f with
      | none => IO.println "  (radical tower construction failed)\n"
      | some radExprs => do
        IO.println "  Radical expressions:"
        let realExprs := radExprs.filter fun e =>
          let v := dagEvalComplex (RadDAG.toDAG e)
          Float.abs v.im < 1e-6
        let showExprs := if realExprs.isEmpty then radExprs.take 1 else realExprs
        for e in showExprs do
          let v := dagEvalComplex (RadDAG.toDAG e)
          IO.println s!"    {pretty e}"
          IO.println s!"    approx {v.re}"
        IO.println ""
    else
      IO.println "  (not solvable -- no radical expression exists)\n"

def main : IO Unit := do
  IO.println "====== Solvable Polynomial Solver ======\n"
  for ex in examples do
    runExample ex
