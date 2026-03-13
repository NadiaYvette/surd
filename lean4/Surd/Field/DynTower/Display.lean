/-
  Surd.Field.DynTower.Display — Display tower elements as structured field
  extension towers.

  Instead of expanding tower elements into radical trees (which explode
  exponentially for solvable quintics etc.), this module renders the
  tower structure directly: each level is shown as a field extension
  with its minimal polynomial, and elements are polynomials in the
  generator of each level.
-/
import Surd.Field.DynTower
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Tower structure extraction
-- ---------------------------------------------------------------------------

/-- A single extension step in the tower. -/
structure ExtensionStep where
  esLevel : TowerLevel
  esName : String
  esLatexName : String
  esDegree : Int
  esRadicand : TowerElem
  deriving Inhabited

/-- Complete tower display data. -/
structure TowerDisplay where
  tdSteps : List ExtensionStep
  tdElement : TowerElem
  tdLabel : String

-- ---------------------------------------------------------------------------
-- Canonical form for radicand equivalence
-- ---------------------------------------------------------------------------

/-- Canonical form for comparison: strip tower level wrappers,
    keeping only the essential structure. -/
inductive CanonElem where
  | cRat : Rat → CanonElem
  | cExt : List CanonElem → Int → CanonElem → CanonElem

instance : Inhabited CanonElem where default := .cRat 0

private partial def canonElemBEq : CanonElem → CanonElem → Bool
  | .cRat a, .cRat b => a == b
  | .cExt cs1 d1 r1, .cExt cs2 d2 r2 =>
    d1 == d2 && canonElemBEq r1 r2 && cs1.length == cs2.length &&
    (cs1.zip cs2).all fun (a, b) => canonElemBEq a b
  | _, _ => false

instance : BEq CanonElem where beq := canonElemBEq

/-- Canonicalize a TowerElem for structural comparison. -/
partial def canonicalize : TowerElem → CanonElem
  | .tRat r => .cRat r
  | .tExt cs lvl =>
    match cs with
    | c :: rest =>
      if rest.all TowerElem.tIsZero then canonicalize c
      else .cExt (cs.map canonicalize) lvl.tlRootDeg (canonicalize lvl.tlRadicand)
    | [] => .cRat 0

/-- Check if two radicands are semantically equivalent. -/
def radicandEquiv (a b : TowerElem) : Bool :=
  canonicalize a == canonicalize b

-- ---------------------------------------------------------------------------
-- Level collection
-- ---------------------------------------------------------------------------

/-- Collect all distinct TowerLevels referenced by a TowerElem. -/
partial def collectLevels : TowerElem → List (Int × TowerLevel)
  | .tRat _ => []
  | .tExt cs lvl =>
    let fromCoeffs := cs.flatMap collectLevels
    let fromRadicand := collectLevels lvl.tlRadicand
    let all := fromCoeffs ++ fromRadicand ++ [(lvl.tlId, lvl)]
    -- Deduplicate by level ID
    all.foldl (fun acc (id, l) =>
      if acc.any (fun (id', _) => id' == id) then acc
      else acc ++ [(id, l)]
    ) []

-- ---------------------------------------------------------------------------
-- Greek names
-- ---------------------------------------------------------------------------

private def greekNames : List String :=
  ["α", "β", "γ", "δ", "ε", "ζ_t", "η_t", "θ_t"]

private def greekLatexNames : List String :=
  ["\\alpha", "\\beta", "\\gamma", "\\delta", "\\varepsilon",
   "\\zeta_t", "\\eta_t", "\\theta_t"]

private def greekName (i : Nat) : String :=
  if i < greekNames.length then greekNames.get! i
  else "α" ++ toString (i - greekNames.length + 2)

private def greekLatexName (i : Nat) : String :=
  if i < greekLatexNames.length then greekLatexNames.get! i
  else "\\alpha_{" ++ toString (i - greekLatexNames.length + 2) ++ "}"

-- ---------------------------------------------------------------------------
-- Merge equivalent levels
-- ---------------------------------------------------------------------------

/-- Merge tower levels that have the same degree and radicand.
    Returns deduplicated steps and a list of (levelId, step) mappings. -/
private partial def mergeEquivGo (lvls : List TowerLevel) (nextName : Nat)
    : List ExtensionStep × List (Int × ExtensionStep) :=
  match lvls with
  | [] => ([], [])
  | l :: ls =>
    let (moreEquivs, nonEquivs) := ls.partition fun l' =>
      l.tlDegree == l'.tlDegree && radicandEquiv l.tlRadicand l'.tlRadicand
    let step : ExtensionStep :=
      { esLevel := l
        esName := greekName nextName
        esLatexName := greekLatexName nextName
        esDegree := l.tlDegree
        esRadicand := l.tlRadicand }
    let idEntries := (l.tlId, step) :: moreEquivs.map (fun l' => (l'.tlId, step))
    let (restSteps, restMap) := mergeEquivGo nonEquivs (nextName + 1)
    (step :: restSteps, idEntries ++ restMap)

private def mergeEquivLevels (lvls : List TowerLevel)
    : List ExtensionStep × List (Int × ExtensionStep) :=
  mergeEquivGo lvls 0

-- ---------------------------------------------------------------------------
-- Extract tower structure
-- ---------------------------------------------------------------------------

/-- Extract the tower structure from a TowerElem. -/
def extractTower (label : String) (e : TowerElem) : TowerDisplay :=
  let levels := collectLevels e
  let sorted := levels.mergeSort fun (a, _) (b, _) => a ≤ b
  let (steps, _) := mergeEquivLevels (sorted.map Prod.snd)
  { tdSteps := steps
    tdElement := e
    tdLabel := label }

-- ---------------------------------------------------------------------------
-- Name map
-- ---------------------------------------------------------------------------

private abbrev NameMap := List (Int × ExtensionStep)

private def buildNameMap (td : TowerDisplay) : NameMap :=
  let levels := collectLevels td.tdElement
  let steps := td.tdSteps
  levels.filterMap fun (lid, lvl) =>
    match steps.find? (fun s =>
      s.esDegree == lvl.tlDegree && radicandEquiv s.esRadicand lvl.tlRadicand) with
    | some s => some (lid, s)
    | none =>
      match steps.find? (fun s => s.esLevel.tlId == lvl.tlId) with
      | some s => some (lid, s)
      | none => none

private def lookupName (nm : NameMap) (lid : Int) : Option ExtensionStep :=
  nm.find? (fun (id, _) => id == lid) |>.map Prod.snd

-- ---------------------------------------------------------------------------
-- LaTeX rendering helpers
-- ---------------------------------------------------------------------------

private def latexRatTE (r : Rat) : String :=
  let n := r.num
  let d := r.den
  if d == 1 then toString n
  else if n < 0 then "-\\frac{" ++ toString (-n) ++ "}{" ++ toString d ++ "}"
  else "\\frac{" ++ toString n ++ "}{" ++ toString d ++ "}"

private def rootExpr (deg : Int) (r : String) : String :=
  if deg == 2 then "\\sqrt{" ++ r ++ "}"
  else if deg == 3 then "\\sqrt[3]{" ++ r ++ "}"
  else "\\sqrt[" ++ toString deg ++ "]{" ++ r ++ "}"

/-- Check if a string contains + or - after the first character. -/
private def hasInternalPlusMinus (s : String) : Bool :=
  (s.drop 1).any (fun c => c == '+' || c == '-')

private def wrapIfCompound (s : String) : String :=
  if hasInternalPlusMinus s then
    "\\left(" ++ s ++ "\\right) \\cdot"
  else s

-- ---------------------------------------------------------------------------
-- LaTeX rendering of TowerElem
-- ---------------------------------------------------------------------------

/-- Render a TowerElem as LaTeX, using named generators. -/
partial def latexTE (nm : NameMap) : TowerElem → String
  | .tRat r => latexRatTE r
  | .tExt cs lvl =>
    match lookupName nm lvl.tlId with
    | none => "?_{" ++ toString lvl.tlId ++ "}"
    | some step =>
      let terms := collectLatexTerms step.esLatexName nm cs
      match terms with
      | [] => "0"
      | _ => renderLatexTerms terms

where
  /-- Collect non-zero terms as (positive?, rendered) pairs. -/
  collectLatexTerms (name : String) (nm : NameMap) (cs : List TowerElem)
      : List (Bool × String) :=
    cs.enum.filterMap fun (i, c) =>
      if c.tIsZero then none
      else some (renderCoeffTerm name nm c i)

  /-- Render a single term: coefficient * generator^power. -/
  renderCoeffTerm (name : String) (nm : NameMap) (c : TowerElem) (i : Nat)
      : Bool × String :=
    if i == 0 then
      let s := latexTE nm c
      if s.startsWith "-" then (false, s.drop 1)
      else (true, s)
    else
      let genPart := if i == 1 then name else name ++ "^{" ++ toString i ++ "}"
      match c with
      | .tRat r =>
        if r == 1 then (true, genPart)
        else if r == -1 then (false, genPart)
        else if r > 0 then (true, latexRatTE r ++ " " ++ genPart)
        else (false, latexRatTE (-r) ++ " " ++ genPart)
      | _ =>
        let s := latexTE nm c
        if s.startsWith "-" then (false, wrapIfCompound (s.drop 1) ++ " " ++ genPart)
        else (true, wrapIfCompound s ++ " " ++ genPart)

  /-- Render signed terms with + and - separators. -/
  renderLatexTerms : List (Bool × String) → String
    | [] => "0"
    | (s, t) :: rest =>
      let hd := if s then t else "-" ++ t
      hd ++ String.join (rest.map fun (s', e) =>
        if s' then " + " ++ e else " - " ++ e)

/-- Render a single extension step in LaTeX. -/
private def latexStep (nm : NameMap) (step : ExtensionStep) : List String :=
  let name := step.esLatexName
  let n := step.esDegree
  let rad := latexTE nm step.esRadicand
  let lhs := name ++ "^{" ++ toString n ++ "}"
  let bracketInline := "\\bigl[" ++ name ++ " = " ++ rootExpr n rad ++ "\\bigr]"
  if rad.length ≤ 80 then
    [lhs ++ " &= &" ++ rad ++ " \\qquad " ++ bracketInline ++ " \\\\"]
  else
    let bracketRow := "\\bigl[" ++ name ++ " &= &" ++ rootExpr n rad ++ "\\bigr]"
    [lhs ++ " &= &" ++ rad ++ " \\\\", bracketRow ++ " \\\\"]

/-- Render a tower element as a complete LaTeX display. -/
def latexTower (td : TowerDisplay) : String :=
  let nm := buildNameMap td
  let steps := td.tdSteps.flatMap (latexStep nm)
  let elemStr := latexTE nm td.tdElement
  let elemLines :=
    if elemStr.length ≤ 120 then [td.tdLabel ++ " &= &" ++ elemStr]
    else [td.tdLabel ++ " &= &" ++ elemStr]  -- simplified: no split logic
  let lines :=
    ["\\begin{alignat*}{2}"]
    ++ (if td.tdSteps.isEmpty then [] else ["& & \\textbf{Tower of extensions:} \\\\"])
    ++ steps
    ++ elemLines
    ++ ["\\end{alignat*}"]
  "\n".intercalate lines ++ "\n"

/-- Render a TowerElem as LaTeX, using named generators from a display. -/
def latexTowerElem (td : TowerDisplay) (e : TowerElem) : String :=
  latexTE (buildNameMap td) e

-- ---------------------------------------------------------------------------
-- Text rendering helpers
-- ---------------------------------------------------------------------------

private def prettyRatTE (r : Rat) : String :=
  let n := r.num
  let d := r.den
  if d == 1 then toString n
  else if n < 0 then "(-" ++ toString (-n) ++ "/" ++ toString d ++ ")"
  else "(" ++ toString n ++ "/" ++ toString d ++ ")"

private def rootStr (deg : Int) : String :=
  if deg == 2 then "√"
  else if deg == 3 then "∛"
  else toString deg ++ "√"

-- ---------------------------------------------------------------------------
-- Text rendering of TowerElem
-- ---------------------------------------------------------------------------

/-- Render a TowerElem as text, using named generators. -/
partial def prettyTE (nm : NameMap) : TowerElem → String
  | .tRat r => prettyRatTE r
  | .tExt cs lvl =>
    match lookupName nm lvl.tlId with
    | none => "?_" ++ toString lvl.tlId
    | some step =>
      let terms := collectTextTerms step.esName nm cs
      match terms with
      | [] => "0"
      | _ => renderTextTerms terms

where
  collectTextTerms (name : String) (nm : NameMap) (cs : List TowerElem)
      : List (Bool × String) :=
    cs.enum.filterMap fun (i, c) =>
      if c.tIsZero then none
      else some (renderTextCoeffTerm name nm c i)

  renderTextCoeffTerm (name : String) (nm : NameMap) (c : TowerElem) (i : Nat)
      : Bool × String :=
    if i == 0 then
      let s := prettyTE nm c
      if s.startsWith "-" then (false, s.drop 1)
      else (true, s)
    else
      let genPart := if i == 1 then name else name ++ "^" ++ toString i
      match c with
      | .tRat r =>
        if r == 1 then (true, genPart)
        else if r == -1 then (false, genPart)
        else if r > 0 then (true, prettyRatTE r ++ "·" ++ genPart)
        else (false, prettyRatTE (-r) ++ "·" ++ genPart)
      | _ =>
        let s := prettyTE nm c
        if s.startsWith "-" then (false, "(" ++ s.drop 1 ++ ")·" ++ genPart)
        else if hasInternalPlusMinus s then (true, "(" ++ s ++ ")·" ++ genPart)
        else (true, s ++ "·" ++ genPart)

  renderTextTerms : List (Bool × String) → String
    | [] => "0"
    | (s, t) :: rest =>
      let hd := if s then t else "-" ++ t
      hd ++ String.join (rest.map fun (s', e) =>
        if s' then " + " ++ e else " - " ++ e)

/-- Render a single extension step as text. -/
private def prettyStep (nm : NameMap) (step : ExtensionStep) : String :=
  let name := step.esName
  let n := step.esDegree
  let rad := prettyTE nm step.esRadicand
  "  " ++ name ++ "^" ++ toString n ++ " = " ++ rad
    ++ "    [" ++ name ++ " = " ++ rootStr n ++ "(" ++ rad ++ ")]"

/-- Render a tower element as a complete text display. -/
def prettyTower (td : TowerDisplay) : String :=
  let nm := buildNameMap td
  let steps := td.tdSteps.map (prettyStep nm)
  let elemStr := prettyTE nm td.tdElement
  let lines :=
    (if td.tdSteps.isEmpty then [] else ["Tower of extensions:"])
    ++ steps
    ++ ["", td.tdLabel ++ " = " ++ elemStr]
  "\n".intercalate lines ++ "\n"

/-- Render a TowerElem as text, using named generators from a display. -/
def prettyTowerElem (td : TowerDisplay) (e : TowerElem) : String :=
  prettyTE (buildNameMap td) e

end Surd
