/-
  Generate tables of exact trigonometric values.

  Usage:
    surd-trig-table [OPTIONS] SPEC [SPEC ...]

  Each SPEC is one of:
    N          — tabulate cos and sin at kπ/N for k = 0, 1, ..., N
    p/q        — single angle pπ/q
    N..M       — tabulate at kπ/q for each q in N..M, k = 0..q

  Options:
    --format=latex     LaTeX longtable (default)
    --format=text      plain text (Unicode)
    --format=tower     field extension tower display
    --standalone       emit a complete LaTeX document (default)
    --no-standalone    table fragment only
    --cos-only         omit sine column
    --sin-only         omit cosine column
    --tan              include tangent column
-/
import Surd.Trig
import Surd.Trig.TowerDescent
import Surd.Field.DynTower.Display
import Surd.Radical.LaTeX
import Surd.Radical.Pretty
import Surd.Radical.Normalize
import Surd.Radical.Expr
import Surd.Poly.Univariate
import Surd.Rat

open Std.Internal
open Surd

private def showPoly (p : Poly Rat) : String := reprStr p

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

inductive Format where | latex | text | tower
  deriving BEq

inductive Cols where | cosAndSin | cosOnly | sinOnly
  deriving BEq

structure Config where
  cfgFormat : Format := .latex
  cfgStandalone : Bool := true
  cfgCols : Cols := .cosAndSin
  cfgTan : Bool := false

-- ---------------------------------------------------------------------------
-- Angle specification
-- ---------------------------------------------------------------------------

inductive Spec where
  | single : Int → Int → Spec
  | denom : Int → Spec
  | range : Int → Int → Spec

abbrev Angle := Int × Int

private def intRange (n : Nat) : List Int :=
  (List.range n).map (Int.ofNat ·)

def specAngles : Spec → List Angle
  | .single p q => [(p, q)]
  | .denom n => (intRange (n.toNat + 1)).map fun k => (k, n)
  | .range lo hi =>
    let qs := (intRange ((hi - lo).toNat + 1)).map fun i => lo + i
    qs.flatMap fun q => (intRange (q.toNat + 1)).map fun k => (k, q)

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

private def isDigitChar (c : Char) : Bool := c >= '0' && c <= '9'
private def allDigits (s : String) : Bool := !s.isEmpty && s.toList.all isDigitChar

private def parseSpec (s : String) : Option Spec :=
  -- Check for N..M range
  match s.splitOn ".." with
  | [a, b] =>
    if allDigits a && allDigits b then
      some (.range a.toInt! b.toInt!)
    else none
  | _ =>
  -- Check for p/q
  match s.splitOn "/" with
  | [p, q] =>
    if allDigits q && !q.isEmpty then
      match p.toInt? with
      | some pv => some (.single pv q.toInt!)
      | none => none
    else none
  | _ =>
  -- Check for single denominator N
  if allDigits s then some (.denom s.toInt!)
  else none

private def applyOpt (cfg : Config) (opt : String) : Option Config :=
  if opt == "--format=latex" then some { cfg with cfgFormat := .latex }
  else if opt == "--format=text" then some { cfg with cfgFormat := .text }
  else if opt == "--format=tower" then some { cfg with cfgFormat := .tower }
  else if opt == "--standalone" then some { cfg with cfgStandalone := true }
  else if opt == "--no-standalone" then some { cfg with cfgStandalone := false }
  else if opt == "--cos-only" then some { cfg with cfgCols := .cosOnly }
  else if opt == "--sin-only" then some { cfg with cfgCols := .sinOnly }
  else if opt == "--tan" then some { cfg with cfgTan := true }
  else none

private def parseArgs (args : List String) : Option (Config × List Spec) :=
  let opts := args.filter (·.startsWith "--")
  let rest := args.filter (!·.startsWith "--")
  let cfg := opts.foldl (fun acc o => acc.bind (applyOpt · o)) (some {})
  let specs := rest.filterMap parseSpec
  if specs.isEmpty then none
  else cfg.map (·, specs)

-- ---------------------------------------------------------------------------
-- Rendering helpers
-- ---------------------------------------------------------------------------

private def latexFrac (p q : Int) : String :=
  if p == 0 then "0"
  else if q == 1 then
    if p == 1 then "\\pi"
    else if p == -1 then "-\\pi"
    else toString p ++ "\\pi"
  else
    let (sn, np) :=
      if p < 0 && Int.natAbs p == 1 then ("-", "")
      else if p < 0 then ("-", toString (Int.natAbs p))
      else if p == 1 then ("", "")
      else ("", toString p)
    sn ++ "\\frac{" ++ np ++ "\\pi}{" ++ toString q ++ "}"

private def textFrac (p q : Int) : String :=
  if p == 0 then "0"
  else if q == 1 then
    if p == 1 then "π"
    else if p == -1 then "-π"
    else toString p ++ "π"
  else if p == 1 then "π/" ++ toString q
  else if p == -1 then "-π/" ++ toString q
  else toString p ++ "π/" ++ toString q

private def renderAngle (fmt : Format) (a : Angle) : String :=
  match fmt with
  | .latex => latexFrac a.1 a.2
  | .text => textFrac a.1 a.2
  | .tower => textFrac a.1 a.2

-- ---------------------------------------------------------------------------
-- Result rendering
-- ---------------------------------------------------------------------------

structure RenderedResult where
  rrDefs : List (String × String) := []
  rrExpr : String

private partial def renderResult (cfg : Config) (tr : TrigResult) : RenderedResult :=
  match tr with
  | .radical e =>
    match cfg.cfgFormat with
    | .latex =>
      let (defs, expr) := latexDAG e
      { rrDefs := defs, rrExpr := expr }
    | _ => { rrExpr := pretty e }
  | .minPoly p =>
    match cfg.cfgFormat with
    | .latex => { rrExpr := "\\text{minpoly: }" ++ showPoly p }
    | _ => { rrExpr := "minpoly: " ++ showPoly p }

-- ---------------------------------------------------------------------------
-- Row computation
-- ---------------------------------------------------------------------------

structure Row where
  rowAngle : String
  rowCos : Option RenderedResult := none
  rowSin : Option RenderedResult := none
  rowTan : Option RenderedResult := none

private def isZero : RadExpr Rat → Bool
  | .lit r => r == 0
  | _ => false

private partial def computeTan (cfg : Config) (c s : TrigResult) : RenderedResult :=
  match s, c with
  | .radical se, .radical ce =>
    if isZero ce then
      match cfg.cfgFormat with
      | .latex => { rrExpr := "\\text{undefined}" }
      | _ => { rrExpr := "undefined" }
    else if isZero se then renderResult cfg (.radical (.lit 0))
    else renderResult cfg (simplifyTrigResult (.radical (normalize (.mul se (.inv ce)))))
  | _, _ =>
    match cfg.cfgFormat with
    | .latex => { rrExpr := "\\text{undefined}" }
    | _ => { rrExpr := "undefined" }

private partial def computeRow (cfg : Config) (a : Angle) : Row :=
  let simpCos := simplifyTrigResult (cosExact a.1 a.2)
  let simpSin := simplifyTrigResult (sinExact a.1 a.2)
  { rowAngle := renderAngle cfg.cfgFormat a
    rowCos := if cfg.cfgCols != .sinOnly then some (renderResult cfg simpCos) else none
    rowSin := if cfg.cfgCols != .cosOnly then some (renderResult cfg simpSin) else none
    rowTan := if cfg.cfgTan then some (computeTan cfg simpCos simpSin) else none }

-- ---------------------------------------------------------------------------
-- LaTeX table
-- ---------------------------------------------------------------------------

private def latexPreamble : String :=
  "\\documentclass[11pt]{article}\n\
   \\usepackage[a4paper,margin=1in]{geometry}\n\
   \\usepackage{amsmath,amssymb}\n\
   \\usepackage{longtable}\n\
   \\usepackage{booktabs}\n\
   \\title{Exact Trigonometric Values}\n\
   \\author{Generated by \\texttt{surd}}\n\
   \\date{}\n\
   \\begin{document}\n\
   \\maketitle\n\n"

private def numCols (cfg : Config) : Nat :=
  (if cfg.cfgCols != .sinOnly then 1 else 0)
  + (if cfg.cfgCols != .cosOnly then 1 else 0)
  + (if cfg.cfgTan then 1 else 0)

private def colSpec (cfg : Config) : String :=
  "c" ++ String.mk (List.replicate (numCols cfg) 'l')

private def latexHeader (cfg : Config) : String :=
  let cols := ["$\\theta$"]
    ++ (if cfg.cfgCols != .sinOnly then ["$\\cos\\theta$"] else [])
    ++ (if cfg.cfgCols != .cosOnly then ["$\\sin\\theta$"] else [])
    ++ (if cfg.cfgTan then ["$\\tan\\theta$"] else [])
  " & ".intercalate cols

private def latexRow (r : Row) : String :=
  let wrapMath (s : String) : String := "$" ++ s ++ "$"
  let fields := [r.rowAngle]
    ++ (match r.rowCos with | some rr => [rr.rrExpr] | none => [])
    ++ (match r.rowSin with | some rr => [rr.rrExpr] | none => [])
    ++ (match r.rowTan with | some rr => [rr.rrExpr] | none => [])
  " & ".intercalate (fields.map wrapMath) ++ " \\\\"

private def rowDefs (r : Row) : List (String × String) :=
  let getDefs (o : Option RenderedResult) := match o with
    | some rr => rr.rrDefs
    | none => []
  getDefs r.rowCos ++ getDefs r.rowSin ++ getDefs r.rowTan

private partial def dedup : List (String × String) → List (String × String)
  | [] => []
  | (n, d) :: rest => (n, d) :: dedup (rest.filter fun (n', _) => n' != n)

private def latexTable (cfg : Config) (rows : List Row) : String :=
  let allDefs := dedup (rows.flatMap rowDefs)
  let defsBlock :=
    if allDefs.isEmpty then ""
    else
      let defLines := allDefs.map fun (name, defn) => name ++ " &= " ++ defn ++ " \\\\"
      "\\noindent\\textbf{Where:}\n\\begin{align*}\n"
      ++ "\n".intercalate defLines
      ++ "\n\\end{align*}\n\n"
  let body := "\n".intercalate (rows.map latexRow)
  let table :=
    "\\begin{longtable}{" ++ colSpec cfg ++ "}\n"
    ++ "\\toprule\n"
    ++ latexHeader cfg ++ " \\\\\n"
    ++ "\\midrule\n"
    ++ "\\endhead\n"
    ++ body ++ "\n"
    ++ "\\bottomrule\n"
    ++ "\\end{longtable}"
  if cfg.cfgStandalone then latexPreamble ++ defsBlock ++ table ++ "\n\\end{document}\n"
  else defsBlock ++ table

-- ---------------------------------------------------------------------------
-- Text table
-- ---------------------------------------------------------------------------

private def textRow (r : Row) : String :=
  let getExpr (o : Option RenderedResult) := match o with
    | some rr => [rr.rrExpr]
    | none => []
  let fields := [r.rowAngle] ++ getExpr r.rowCos ++ getExpr r.rowSin ++ getExpr r.rowTan
  "  │  ".intercalate fields

private def textHeader (cfg : Config) : String :=
  let cols := ["θ"]
    ++ (if cfg.cfgCols != .sinOnly then ["cos θ"] else [])
    ++ (if cfg.cfgCols != .cosOnly then ["sin θ"] else [])
    ++ (if cfg.cfgTan then ["tan θ"] else [])
  "  │  ".intercalate cols

private def textTable (cfg : Config) (rows : List Row) : String :=
  let hdr := textHeader cfg
  let sep := String.mk (hdr.toList.map fun _ => '-')
  hdr ++ "\n" ++ sep ++ "\n"
  ++ "\n".intercalate (rows.map textRow)

-- ---------------------------------------------------------------------------
-- Tower output
-- ---------------------------------------------------------------------------

private partial def towerAngle (cfg : Config) (a : Angle) : String :=
  let p := a.1; let q := a.2
  let g := Int.gcd (Int.natAbs p) (2 * q).toNat
  let n := (2 * q / g).toNat
  let cosLabel := "\\cos " ++ latexFrac p q
  let sinLabel := "\\sin " ++ latexFrac p q
  match allPeriodsViaTower n with
  | some tr =>
    let cosDisp := extractTower cosLabel tr.trCos
    let sinDisp := extractTower sinLabel tr.trSin
    let cosOut := latexTower cosDisp
    let sinOut := latexTower sinDisp
    let sections :=
      if cfg.cfgCols == .cosOnly then [cosOut]
      else if cfg.cfgCols == .sinOnly then [sinOut]
      else [cosOut, "", sinOut]
    "\n".intercalate sections
  | none =>
    let result := simplifyTrigResult (cosExact p q)
    match result with
    | .radical e =>
      "\\begin{align*}\n& " ++ cosLabel ++ " = " ++ latex e
      ++ "\n\\end{align*}\n"
    | .minPoly poly =>
      "\\begin{align*}\n& " ++ cosLabel ++ " = \\text{minpoly: }"
      ++ showPoly poly ++ "\n\\end{align*}\n"

private partial def towerOutput (cfg : Config) (angles : List Angle) : String :=
  let preamble := if cfg.cfgStandalone then latexPreamble else ""
  let postamble := if cfg.cfgStandalone then "\\end{document}\n" else ""
  let body := "\n".intercalate (angles.map (towerAngle cfg))
  preamble ++ body ++ postamble

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

private def usage : String :=
  "Usage: surd-trig-table [OPTIONS] SPEC [SPEC ...]\n\n" ++
  "SPEC is one of:\n" ++
  "  N          tabulate cos/sin at kπ/N for k = 0..N\n" ++
  "  p/q        single angle pπ/q\n" ++
  "  N..M       tabulate for each denominator q in N..M\n\n" ++
  "Options:\n" ++
  "  --format=latex     LaTeX longtable (default)\n" ++
  "  --format=text      plain text (Unicode)\n" ++
  "  --format=tower     field extension tower display\n" ++
  "  --standalone       complete LaTeX document (default)\n" ++
  "  --no-standalone    table fragment only\n" ++
  "  --cos-only         omit sine column\n" ++
  "  --sin-only         omit cosine column\n" ++
  "  --tan              include tangent column"

partial def main (args : List String) : IO Unit := do
  match parseArgs args with
  | none =>
    IO.eprintln "Error: no valid angle specifications given"
    IO.eprintln ""
    IO.eprintln usage
    return
  | some (cfg, specs) =>
    let angles := specs.flatMap specAngles
    match cfg.cfgFormat with
    | .tower => IO.print (towerOutput cfg angles)
    | _ =>
      let rows := angles.map (computeRow cfg)
      let output := match cfg.cfgFormat with
        | .latex => latexTable cfg rows
        | _ => textTable cfg rows
      IO.print output
