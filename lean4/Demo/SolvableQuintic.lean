/-
  Solvable quintic solver demo.

  Demonstrates Galois-theoretic identification and radical tower construction
  for degree-5 polynomials over Q. Classifies each polynomial's Galois group
  among the five transitive subgroups of S₅, then — when the group is solvable
  — constructs explicit radical expressions for the roots.
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
  [ { exName := "x⁵ + x⁴ - 4x³ - 3x² + 3x + 1"
      exPoly := Poly.mkPoly #[1, 3, -3, -4, 1, 1]
      exNotes := "C₅ — minimal polynomial of 2cos(2π/11)" }
  , { exName := "x⁵ - 2"
      exPoly := Poly.mkPoly #[-2, 0, 0, 0, 0, 1]
      exNotes := "F₂₀ — Gal(Q(⁵√2,ζ₅)/Q) = Z/5 ⋊ Z/4" }
  , { exName := "x⁵ + 20x + 32"
      exPoly := Poly.mkPoly #[32, 20, 0, 0, 0, 1]
      exNotes := "D₅ — dihedral group of order 10" }
  , { exName := "x⁵ - 4x + 2"
      exPoly := Poly.mkPoly #[2, -4, 0, 0, 0, 1]
      exNotes := "S₅ — irreducible by Eisenstein (not solvable)" }
  , { exName := "x⁵ - x - 1"
      exPoly := Poly.mkPoly #[-1, -1, 0, 0, 0, 1]
      exNotes := "S₅ (not solvable)" }
  ]

def runExample (ex : Example) : IO Unit := do
  IO.println s!"─── {ex.exName} ───"
  IO.println s!"  ({ex.exNotes})"

  let f := ex.exPoly
  let disc := discriminantOf f
  let discSq := isSquareRational disc

  IO.println s!"  disc(f) = {disc}"
  IO.println s!"  disc is square: {discSq}"

  match identifyGaloisGroup5 f with
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
      match solveViaTower gr f with
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
          IO.println s!"    ≈ {v.re}"
        IO.println ""
    else
      IO.println "  (not solvable — no radical expression exists)\n"

def main : IO Unit := do
  IO.println "══════ Solvable Quintic Solver ══════\n"
  for ex in examples do
    runExample ex
