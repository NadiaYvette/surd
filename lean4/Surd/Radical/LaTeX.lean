/-
  Surd.Radical.LaTeX — LaTeX rendering of radical expressions.

  Includes DAG-based rendering that extracts multiply-referenced
  subexpressions as named definitions for large expressions.
-/
import Surd.Radical.Expr
import Surd.Radical.DAG
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- --------------------------------------------------------------------------
-- Precedence levels
-- --------------------------------------------------------------------------

private def precAdd : Nat := 1
private def precMul : Nat := 2
private def precNeg : Nat := 3
private def precPow : Nat := 4

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

private def parensIf (b : Bool) (s : String) : String :=
  if b then "\\left(" ++ s ++ "\\right)" else s

private def latexRat (r : Rat) : String :=
  let n := r.num
  let d := r.den
  if d == 1 then toString n
  else if n < 0 then "-\\frac{" ++ toString (- n) ++ "}{" ++ toString d ++ "}"
  else "\\frac{" ++ toString n ++ "}{" ++ toString d ++ "}"

private def joinMul : List String → String
  | [] => ""
  | [x] => x
  | x :: xs => x ++ String.join (xs.map (" \\cdot " ++ ·))

-- --------------------------------------------------------------------------
-- Flatten helpers — flattenMul before flattenAdd (forward ref)
-- --------------------------------------------------------------------------

private def flattenMul : RadExpr Rat → List (RadExpr Rat)
  | .mul a b => flattenMul a ++ flattenMul b
  | e => [e]

private def rebuildMul : List (RadExpr Rat) → RadExpr Rat
  | [] => .lit 1
  | [x] => x
  | x :: xs => xs.foldl .mul x

private def flattenAdd : RadExpr Rat → List (Bool × RadExpr Rat)
  | .add a b => flattenAdd a ++ flattenAdd b
  | .neg e => (flattenAdd e).map fun (s, t) => (!s, t)
  | .lit r => if r < 0 then [(false, .lit (-r))] else [(true, .lit r)]
  | .mul (.neg a) b => [(false, .mul a b)]
  | e@(.mul _ _) =>
    match flattenMul e with
    | .lit r :: rest =>
      if r < 0 then [(false, rebuildMul (.lit (-r) :: rest))]
      else [(true, e)]
    | _ => [(true, e)]
  | e => [(true, e)]

-- --------------------------------------------------------------------------
-- Tree-based LaTeX rendering
-- --------------------------------------------------------------------------

private def latexRadicand (e : RadExpr Rat) (pp : Nat → RadExpr Rat → String) : String :=
  match e with
  | .lit r =>
    let n := r.num
    let d := r.den
    if d == 1 then toString n
    else if n < 0 then "-\\frac{" ++ toString (- n) ++ "}{" ++ toString d ++ "}"
    else "\\frac{" ++ toString n ++ "}{" ++ toString d ++ "}"
  | _ => pp 0 e

private def latexBase (e : RadExpr Rat) (pp : Nat → RadExpr Rat → String) : String :=
  match e with
  | .root _ _ | .add _ _ | .mul _ _ | .neg _ | .inv _ =>
    "\\left(" ++ pp 0 e ++ "\\right)"
  | _ => pp precPow e

/-- Render a radical expression as LaTeX at given precedence. -/
partial def latexPrec : Nat → RadExpr Rat → String
  | _, .lit r => latexRat r
  | p, .neg e =>
    match e with
    | .add _ _ =>
      parensIf (p > precAdd)
        (renderTerms (flattenAdd e |>.map fun (s, t) => (!s, t)))
    | .mul (.lit c) rest =>
      latexPrec p (.mul (.lit (-c)) rest)
    | .lit r =>
      latexPrec p (.lit (-r))
    | _ =>
      parensIf (p > precNeg) ("-" ++ latexPrec precNeg e)
  | p, e@(.add _ _) =>
    parensIf (p > precAdd) (renderTerms (flattenAdd e))
  | p, .mul a (.inv b) =>
    parensIf (p > precMul) ("\\frac{" ++ latexPrec 0 a ++ "}{" ++ latexPrec 0 b ++ "}")
  | p, .mul (.inv a) b =>
    parensIf (p > precMul) ("\\frac{" ++ latexPrec 0 b ++ "}{" ++ latexPrec 0 a ++ "}")
  | p, .inv e =>
    parensIf (p > precMul) ("\\frac{1}{" ++ latexPrec 0 e ++ "}")
  | _, .root 2 (.lit r) =>
    if r == -1 then "\\mathrm{i}"
    else "\\sqrt{" ++ latexRadicand (.lit r) latexPrec ++ "}"
  | _, .root 2 e => "\\sqrt{" ++ latexRadicand e latexPrec ++ "}"
  | _, .root n e => "\\sqrt[" ++ toString n ++ "]{" ++ latexRadicand e latexPrec ++ "}"
  | _, .pow _ 0 => "1"
  | p, .pow e n =>
    if n < 0 then latexPrec p (.inv (.pow e (-n)))
    else if n == 1 then latexPrec p e
    else parensIf (p > precPow) (latexBase e latexPrec ++ "^{" ++ toString n ++ "}")
  | p, e@(.mul _ _) =>
    parensIf (p > precMul) (renderFactors (flattenMul e))
where
  renderTerms : List (Bool × RadExpr Rat) → String
    | [] => "0"
    | (s, t) :: rest =>
      let hd := if s then latexPrec precAdd t else "-" ++ latexPrec precMul t
      hd ++ String.join (rest.map fun (s', e) =>
        if s' then " + " ++ latexPrec precAdd e
        else " - " ++ latexPrec precMul e)

  renderFactors : List (RadExpr Rat) → String
    | [] => "1"
    | [x] => latexPrec precMul x
    | .lit c :: rest =>
      if c == 1 then joinMul (rest.map (latexPrec precPow))
      else if c == -1 then "-" ++ joinMul (rest.map (latexPrec precPow))
      else latexRat c ++ " \\cdot " ++ joinMul (rest.map (latexPrec precPow))
    | fs => joinMul (fs.map (latexPrec precPow))

/-- Render a radical expression as a LaTeX math-mode string. -/
def latex (e : RadExpr Rat) : String :=
  latexPrec 0 e

-- --------------------------------------------------------------------------
-- DAG-based rendering
-- --------------------------------------------------------------------------

/-- Generate a LaTeX variable name for a DAG node. -/
private def nodeVar (nid : Nat) : String :=
  "x_{" ++ toString nid ++ "}"

/-- Count how many times each node is referenced by other nodes. -/
private def countRefs (dag : RadDAG Rat) : Array Nat :=
  let n := dag.nodes.size
  let counts := Array.mkArray n 0
  dag.nodes.foldl (fun counts op =>
    let inc (cs : Array Nat) (id : Nat) : Array Nat :=
      if id < cs.size then cs.set! id (cs.get! id + 1) else cs
    match op with
    | .nLit _ => counts
    | .nNeg a => inc counts a
    | .nAdd a b => inc (inc counts a) b
    | .nMul a b => inc (inc counts a) b
    | .nInv a => inc counts a
    | .nRoot _ a => inc counts a
    | .nPow a _ => inc counts a
  ) counts

private def isLitOp : RadNodeOp Rat → Bool
  | .nLit _ => true
  | _ => false

/-- Render a radical expression via its DAG, naming shared subexpressions.
    Returns (definitions, expression) where definitions is a list of
    (name, body) pairs for multiply-referenced subexpressions. -/
partial def latexDAG (e : RadExpr Rat) : List (String × String) × String :=
  let dag := RadDAG.toDAG e
  if RadDAG.dagSize dag ≤ 40 then
    ([], latex e)
  else
    let refs := countRefs dag
    let needsName (nid : Nat) : Bool :=
      nid < refs.size && refs.get! nid > 1 &&
      nid < dag.nodes.size && !isLitOp (dag.nodes.get! nid)
    let renderOp := dagRenderOp dag needsName
    -- Build definitions for multiply-referenced non-trivial nodes
    let defs := Id.run do
      let mut result : List (String × String) := []
      for idx in [:dag.nodes.size] do
        if needsName idx then
          result := result ++ [(nodeVar idx, renderOp idx)]
      return result
    let rootExpr :=
      if dag.root < dag.nodes.size then
        let op := dag.nodes.get! dag.root
        match op with
        | .nLit r => latexRat r
        | _ => if needsName dag.root then nodeVar dag.root else renderOp dag.root
      else "?"
    (defs, rootExpr)
where
  dagRenderOp (dag : RadDAG Rat) (needsName : Nat → Bool) (nid : Nat) : String :=
    if nid ≥ dag.nodes.size then "?" else
    let op := dag.nodes.get! nid
    let ref (id : Nat) : String :=
      if id < dag.nodes.size then
        match dag.nodes.get! id with
        | .nLit r => latexRat r
        | _ => if needsName id then nodeVar id else dagRenderOp dag needsName id
      else "?"
    let refAtom (id : Nat) : String :=
      if id < dag.nodes.size then
        match dag.nodes.get! id with
        | .nLit r => latexRat r
        | .nRoot _ _ =>
          if needsName id then nodeVar id else dagRenderOp dag needsName id
        | _ =>
          if needsName id then nodeVar id
          else "\\left(" ++ dagRenderOp dag needsName id ++ "\\right)"
      else "?"
    match op with
    | .nLit r => latexRat r
    | .nNeg a => "-" ++ refAtom a
    | .nAdd a b =>
      let bStr := ref b
      if bStr.startsWith "-" then ref a ++ " - " ++ bStr.drop 1
      else ref a ++ " + " ++ bStr
    | .nMul a b => refAtom a ++ " \\cdot " ++ refAtom b
    | .nInv a => "\\frac{1}{" ++ ref a ++ "}"
    | .nRoot 2 a =>
      if a < dag.nodes.size then
        match dag.nodes.get! a with
        | .nLit r => if r == -1 then "\\mathrm{i}" else "\\sqrt{" ++ ref a ++ "}"
        | _ => "\\sqrt{" ++ ref a ++ "}"
      else "\\sqrt{" ++ ref a ++ "}"
    | .nRoot n a => "\\sqrt[" ++ toString n ++ "]{" ++ ref a ++ "}"
    | .nPow a n =>
      if n == 0 then "1"
      else if n < 0 then "\\frac{1}{" ++ refAtom a ++ "^{" ++ toString (-n) ++ "}}"
      else refAtom a ++ "^{" ++ toString n ++ "}"

end Surd
