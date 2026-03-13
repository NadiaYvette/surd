/-
  Surd.Radical.Pretty — Pretty-printing radical expressions in human-readable notation.

  Includes common subexpression elimination (CSE) for readability:
  repeated subexpressions are named and shown as `let` bindings.
-/
import Surd.Radical.Expr
import Surd.Rat
import Std.Internal.Rat
import Std.Data.HashMap

open Std.Internal
open Std (HashMap)

namespace Surd

-- --------------------------------------------------------------------------
-- Precedence levels
-- --------------------------------------------------------------------------

private def precAdd : Nat := 1
private def precMul : Nat := 2
private def precNeg : Nat := 3
private def precPow : Nat := 4
private def precAtom : Nat := 5

-- --------------------------------------------------------------------------
-- Shared helpers
-- --------------------------------------------------------------------------

private def parensIf (b : Bool) (s : String) : String :=
  if b then "(" ++ s ++ ")" else s

private def prettyRat (r : Rat) : String :=
  let n := r.num
  let d := r.den
  if d == 1 then toString n
  else if n < 0 then "(-" ++ toString (- n) ++ "/" ++ toString d ++ ")"
  else "(" ++ toString n ++ "/" ++ toString d ++ ")"

private def joinWith (sep : String) : List String → String
  | [] => ""
  | [x] => x
  | x :: xs => x ++ String.join (xs.map (sep ++ ·))

-- --------------------------------------------------------------------------
-- Flatten helpers — flattenMul must come before flattenAdd (forward ref)
-- --------------------------------------------------------------------------

private def flattenMulBasic : RadExpr Rat → List (RadExpr Rat)
  | .mul a b => flattenMulBasic a ++ flattenMulBasic b
  | e => [e]

private def rebuildMul : List (RadExpr Rat) → RadExpr Rat
  | [] => .lit 1
  | [x] => x
  | x :: xs => xs.foldl .mul x

private def flattenAddBasic : RadExpr Rat → List (Bool × RadExpr Rat)
  | .add a b => flattenAddBasic a ++ flattenAddBasic b
  | .neg e => (flattenAddBasic e).map fun (s, t) => (!s, t)
  | .lit r => if r < 0 then [(false, .lit (-r))] else [(true, .lit r)]
  | .mul (.neg a) b => [(false, .mul a b)]
  | e@(.mul _ _) =>
    match flattenMulBasic e with
    | .lit r :: rest =>
      if r < 0 then [(false, rebuildMul (.lit (-r) :: rest))]
      else [(true, e)]
    | _ => [(true, e)]
  | e => [(true, e)]

-- --------------------------------------------------------------------------
-- Basic (non-CSE) pretty-printing
-- --------------------------------------------------------------------------

private def isSimpleBasic : RadExpr Rat → Bool
  | .lit _ => true
  | .root _ _ => true
  | _ => false

private def prettyRadicandBasic (e : RadExpr Rat) (pp : Nat → RadExpr Rat → String) : String :=
  match e with
  | .lit r =>
    if r ≥ 0 && r.den == 1 then toString r.num
    else "(" ++ pp 0 e ++ ")"
  | _ =>
    if isSimpleBasic e then pp precPow e
    else "(" ++ pp 0 e ++ ")"

/-- Render a radical expression at given precedence. -/
partial def prettyPrec : Nat → RadExpr Rat → String
  | _, .lit r => prettyRat r
  | p, .neg e =>
    match e with
    | .add _ _ =>
      parensIf (p > precAdd) (renderTermsBasic (flattenAddBasic e |>.map fun (s, t) => (!s, t)))
    | .mul (.lit c) rest =>
      prettyPrec p (.mul (.lit (-c)) rest)
    | .lit r =>
      prettyPrec p (.lit (-r))
    | _ =>
      parensIf (p > precNeg) ("-" ++ prettyPrec precNeg e)
  | p, e@(.add _ _) =>
    parensIf (p > precAdd) (renderTermsBasic (flattenAddBasic e))
  | p, .mul a (.inv b) =>
    parensIf (p > precMul) (prettyPrec precMul a ++ "/" ++ prettyPrec precPow b)
  | p, e@(.mul _ _) =>
    parensIf (p > precMul) (renderFactorsBasic (flattenMulBasic e))
  | p, .inv e =>
    parensIf (p > precMul) ("1/" ++ prettyPrec precPow e)
  | _, .root 2 (.lit r) =>
    if r == -1 then "i" else "√" ++ prettyRadicandBasic (.lit r) prettyPrec
  | _, .root 2 e => "√" ++ prettyRadicandBasic e prettyPrec
  | _, .root 3 e => "∛" ++ prettyRadicandBasic e prettyPrec
  | _, .root n e => toString n ++ "√" ++ prettyRadicandBasic e prettyPrec
  | _, .pow _ 0 => "1"
  | p, .pow e n =>
    if n < 0 then prettyPrec p (.inv (.pow e (-n)))
    else if n == 1 then prettyPrec p e
    else parensIf (p > precPow) (prettyPrec precPow e ++ "^" ++ toString n)

where
  renderTermsBasic : List (Bool × RadExpr Rat) → String
    | [] => "0"
    | (s, t) :: rest =>
      let hd := if s then prettyPrec precAdd t else "-" ++ prettyPrec precMul t
      hd ++ String.join (rest.map fun (s', e) =>
        if s' then " + " ++ prettyPrec precAdd e
        else " - " ++ prettyPrec precMul e)

  renderFactorsBasic : List (RadExpr Rat) → String
    | [] => "1"
    | [x] => prettyPrec precMul x
    | .lit c :: rest =>
      if c == 1 then joinWith "·" (rest.map (prettyPrec precPow))
      else if c == -1 then "-" ++ joinWith "·" (rest.map (prettyPrec precPow))
      else prettyRat c ++ "·" ++ joinWith "·" (rest.map (prettyPrec precPow))
    | fs => joinWith "·" (fs.map (prettyPrec precPow))

/-- Render a radical expression as a human-readable string. -/
def pretty (e : RadExpr Rat) : String :=
  prettyPrec 0 e

-- --------------------------------------------------------------------------
-- CSE pretty-printing
-- --------------------------------------------------------------------------

/-- Count occurrences of every subexpression. -/
private partial def countSubs (e : RadExpr Rat) : HashMap (RadExpr Rat) Nat :=
  go HashMap.empty e
where
  go (m : HashMap (RadExpr Rat) Nat) (e : RadExpr Rat) : HashMap (RadExpr Rat) Nat :=
    let m' := m.alter e fun
      | some n => some (n + 1)
      | none => some 1
    match e with
    | .lit _ => m'
    | .neg a => go m' a
    | .add a b => go (go m' a) b
    | .mul a b => go (go m' a) b
    | .inv a => go m' a
    | .root _ a => go m' a
    | .pow a _ => go m' a

/-- Is a subexpression complex enough to be worth naming? -/
private def worthNaming : RadExpr Rat → Bool
  | .lit _ => false
  | .neg (.lit _) => false
  | .root _ (.lit _) => false
  | _ => true

/-- Generate variable names: a, b, ..., z, a1, b1, ..., z1, a2, ... -/
private def varName (i : Nat) : String :=
  let c := Char.ofNat (97 + i % 26)  -- 'a' = 97
  if i < 26 then c.toString
  else c.toString ++ toString (i / 26)

/-- Render with CSE: repeated subexpressions are shown as named intermediates. -/
partial def prettyCSE (expr : RadExpr Rat) : String :=
  let counts := countSubs expr
  -- Keep subexpressions that appear 2+ times and are worth naming
  let shared := counts.fold (init := ([] : List (RadExpr Rat))) fun acc e n =>
    if n ≥ 2 && worthNaming e then e :: acc else acc
  -- Sort by size (smaller first)
  let sorted := shared.mergeSort fun a b => RadExpr.size a ≤ RadExpr.size b
  -- Assign names
  let nameMap := sorted.enum.foldl (fun (m : HashMap (RadExpr Rat) String) (i, e) =>
    m.insert e (varName i)
  ) HashMap.empty
  -- Build bindings
  let bindings := sorted.enum.map fun (i, sub) =>
    let name := varName i
    "  " ++ name ++ " = " ++ renderWith nameMap sub
  let body := renderWith nameMap expr
  match bindings with
  | [] => body
  | _ => "let\n" ++ String.intercalate "\n" bindings ++ "\nin " ++ body
where
  renderWith (names : HashMap (RadExpr Rat) String) (e : RadExpr Rat) : String :=
    go names 0 e

  go (names : HashMap (RadExpr Rat) String) (p : Nat) (e : RadExpr Rat) : String :=
    match names.get? e with
    | some name => parensIf (p > precAtom) name
    | none => pp names p e

  pp (names : HashMap (RadExpr Rat) String) : Nat → RadExpr Rat → String
    | _, .lit r => prettyRat r
    | p, .neg e => parensIf (p > precNeg) ("-" ++ go names precNeg e)
    | p, e@(.add _ _) =>
      parensIf (p > precAdd) (renderTermsCSE names (flattenAddBasic e))
    | p, .mul a (.inv b) =>
      parensIf (p > precMul) (go names precMul a ++ "/" ++ go names precPow b)
    | p, e@(.mul _ _) =>
      parensIf (p > precMul) (renderFactorsCSE names (flattenMulBasic e))
    | p, .inv e => parensIf (p > precMul) ("1/" ++ go names precPow e)
    | _, .root 2 (.lit r) =>
      if r == -1 then "i" else "√" ++ radicandCSE names (.lit r)
    | _, .root 2 e => "√" ++ radicandCSE names e
    | _, .root 3 e => "∛" ++ radicandCSE names e
    | _, .root n e => toString n ++ "√" ++ radicandCSE names e
    | _, .pow _ 0 => "1"
    | p, .pow e n =>
      if n < 0 then pp names p (.inv (.pow e (-n)))
      else if n == 1 then go names p e
      else parensIf (p > precPow) (go names precPow e ++ "^" ++ toString n)

  radicandCSE (names : HashMap (RadExpr Rat) String) (e : RadExpr Rat) : String :=
    match e with
    | .lit r =>
      if r ≥ 0 && r.den == 1 then toString r.num
      else "(" ++ go names 0 e ++ ")"
    | _ =>
      let isSimple := match e with
        | .lit _ => true
        | .root _ _ => true
        | _ => (names.get? e).isSome
      if isSimple then go names precPow e
      else "(" ++ go names 0 e ++ ")"

  renderTermsCSE (names : HashMap (RadExpr Rat) String) : List (Bool × RadExpr Rat) → String
    | [] => "0"
    | (s, t) :: rest =>
      let hd := if s then go names precAdd t else "-" ++ go names precMul t
      hd ++ String.join (rest.map fun (s', e) =>
        if s' then " + " ++ go names precAdd e
        else " - " ++ go names precMul e)

  renderFactorsCSE (names : HashMap (RadExpr Rat) String) : List (RadExpr Rat) → String
    | [] => "1"
    | [x] => go names precMul x
    | .lit c :: rest =>
      if c == 1 then joinWith "·" (rest.map (go names precPow))
      else if c == -1 then "-" ++ joinWith "·" (rest.map (go names precPow))
      else prettyRat c ++ "·" ++ joinWith "·" (rest.map (go names precPow))
    | fs => joinWith "·" (fs.map (go names precPow))

end Surd
