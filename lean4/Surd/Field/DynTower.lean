/-
  Surd.Field.DynTower — Dynamically-nested algebraic extension tower.

  Unlike ExtElem which uses static type nesting, TowerElem uses a recursive
  data type that supports towers of arbitrary runtime-determined depth.

  Elements support full field arithmetic and can be converted to
  radical expressions for display.
-/
import Surd.Radical.Expr
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

mutual

/-- One level of the field tower. -/
inductive TowerLevel where
  | mk : Int → Int → Int → TowerElem → TowerLevel

/-- Element of a dynamically-nested field tower. -/
inductive TowerElem where
  | tRat : Rat → TowerElem
  | tExt : List TowerElem → TowerLevel → TowerElem

end

namespace TowerLevel
def tlId : TowerLevel → Int | .mk i _ _ _ => i
def tlDegree : TowerLevel → Int | .mk _ d _ _ => d
def tlRootDeg : TowerLevel → Int | .mk _ _ r _ => r
def tlRadicand : TowerLevel → TowerElem | .mk _ _ _ e => e
end TowerLevel

instance : Inhabited TowerElem where default := .tRat 0
instance : Inhabited TowerLevel where default := .mk 0 1 1 (.tRat 0)
instance : BEq TowerLevel where beq a b := a.tlId == b.tlId

/-- Check if a tower element is zero. -/
partial def TowerElem.tIsZero : TowerElem → Bool
  | .tRat r => r == 0
  | .tExt cs _ => cs.all TowerElem.tIsZero

def TowerElem.tLevel : TowerElem → Option TowerLevel
  | .tRat _ => none
  | .tExt _ l => some l

def TowerElem.levelId : TowerElem → Int
  | .tRat _ => -1
  | .tExt _ l => l.tlId

/-- Remove trailing zero coefficients. -/
private partial def trimTE : List TowerElem → List TowerElem :=
  fun cs => (cs.reverse.dropWhile TowerElem.tIsZero).reverse

/-- Pad a coefficient list to exactly n elements with zeros. -/
private def padTo (n : Int) (cs : List TowerElem) : List TowerElem :=
  let nNat := n.toNat
  if cs.length ≥ nNat then cs.take nNat
  else cs ++ List.replicate (nNat - cs.length) (.tRat 0)

-- All mutually recursive arithmetic in one block
mutual

partial def towerNeg : TowerElem → TowerElem
  | .tRat a => .tRat (-a)
  | .tExt cs l => .tExt (cs.map towerNeg) l

partial def addCoeffs : List TowerElem → List TowerElem → List TowerElem
  | [], bs => bs
  | as, [] => as
  | a :: as, b :: bs => towerAdd a b :: addCoeffs as bs

partial def subCoeffs : List TowerElem → List TowerElem → List TowerElem
  | [], bs => bs.map towerNeg
  | as, [] => as
  | a :: as, b :: bs => towerSub a b :: subCoeffs as bs

partial def polyMulTE : List TowerElem → List TowerElem → List TowerElem
  | [], _ => []
  | _, [] => []
  | as, bs =>
    let rlen := as.length + bs.length - 1
    let zeros := List.replicate rlen (.tRat 0)
    let terms : List (Nat × TowerElem) :=
      as.enum.flatMap fun (i, a) =>
        bs.enum.map fun (j, b) => (i + j, towerMul a b)
    terms.foldl (fun acc (idx, c) => addAt idx c acc) zeros
  where
    addAt : Nat → TowerElem → List TowerElem → List TowerElem
      | _, _, [] => []
      | 0, c, x :: xs => towerAdd x c :: xs
      | n + 1, c, x :: xs => x :: addAt n c xs

partial def reduceCoeffs (l : TowerLevel) (cs : List TowerElem) : List TowerElem :=
  let n := l.tlDegree.toNat
  if cs.length ≤ n then cs
  else
    let lo := cs.take n
    let hi := cs.drop n
    let shifted := hi.map (towerMul · l.tlRadicand)
    reduceCoeffs l (addCoeffs lo shifted)

partial def mkTExt (cs : List TowerElem) (l : TowerLevel) : TowerElem :=
  let cs' := trimTE cs
  match cs' with
  | [] => .tRat 0
  | [.tRat r] => .tRat r
  | _ => .tExt (padTo l.tlDegree cs') l

partial def promoteTo (lvl : TowerLevel) (e : TowerElem) : TowerElem :=
  match e with
  | .tRat _ => .tExt (e :: List.replicate (lvl.tlDegree.toNat - 1) (.tRat 0)) lvl
  | .tExt _ l =>
    if l == lvl then e
    else if l.tlId < lvl.tlId then
      .tExt (e :: List.replicate (lvl.tlDegree.toNat - 1) (.tRat 0)) lvl
    else e

partial def towerAdd : TowerElem → TowerElem → TowerElem
  | .tRat a, .tRat b => .tRat (a + b)
  | .tRat a, .tExt bs l => .tExt (addCoeffs [.tRat a] bs) l
  | .tExt as l, .tRat b => .tExt (addCoeffs as [.tRat b]) l
  | .tExt as l1, .tExt bs l2 =>
    if l1 == l2 then mkTExt (addCoeffs as bs) l1
    else if l1.tlId < l2.tlId then towerAdd (promoteTo l2 (.tExt as l1)) (.tExt bs l2)
    else towerAdd (.tExt as l1) (promoteTo l1 (.tExt bs l2))

partial def towerSub (a b : TowerElem) : TowerElem :=
  towerAdd a (towerNeg b)

partial def towerMul : TowerElem → TowerElem → TowerElem
  | .tRat a, .tRat b => .tRat (a * b)
  | .tRat a, .tExt bs l => mkTExt (bs.map (towerMul (.tRat a) ·)) l
  | .tExt as l, .tRat b => mkTExt (as.map (towerMul · (.tRat b))) l
  | .tExt as l1, .tExt bs l2 =>
    if l1 == l2 then mkTExt (reduceCoeffs l1 (polyMulTE as bs)) l1
    else if l1.tlId < l2.tlId then towerMul (promoteTo l2 (.tExt as l1)) (.tExt bs l2)
    else towerMul (.tExt as l1) (promoteTo l1 (.tExt bs l2))

partial def towerDiv (a b : TowerElem) : TowerElem :=
  towerMul a (towerInv b)

partial def towerInv : TowerElem → TowerElem
  | .tRat a => .tRat (1 / a)
  | .tExt cs l =>
    if cs.all TowerElem.tIsZero then .tRat 0
    else
      let minP := towerNeg l.tlRadicand :: List.replicate (l.tlDegree.toNat - 1) (.tRat 0) ++ [.tRat 1]
      let (_, s, _) := polyExtGcd (trimTE cs) minP
      mkTExt (reduceCoeffs l s) l

partial def polyExtGcd (a b : List TowerElem)
    : List TowerElem × List TowerElem × List TowerElem :=
  go a b [.tRat 1] [] [] [.tRat 1]
where
  lastNZ (xs : List TowerElem) : TowerElem :=
    match trimTE xs with
    | [] => .tRat 1
    | ts => ts.getLast!
  go (r0 r1 s0 s1 t0 t1 : List TowerElem)
      : List TowerElem × List TowerElem × List TowerElem :=
    if r1.all TowerElem.tIsZero then
      let lc := lastNZ r0
      let lcInv := towerInv lc
      (trimTE (r0.map (towerMul · lcInv)),
       trimTE (s0.map (towerMul · lcInv)),
       trimTE (t0.map (towerMul · lcInv)))
    else
      let (q, r) := polyDivMod r0 r1
      go r1 r s1 (subCoeffs s0 (polyMulTE q s1)) t1 (subCoeffs t0 (polyMulTE q t1))

partial def polyDivMod (f g : List TowerElem) : List TowerElem × List TowerElem :=
  let f' := trimTE f
  let g' := trimTE g
  if g'.isEmpty then ([], f')
  else
    let degG := g'.length - 1
    let lcG := g'.getLast!
    go degG lcG g' [] f'
where
  go (degG : Nat) (lcG : TowerElem) (g' q r : List TowerElem)
      : List TowerElem × List TowerElem :=
    let r' := trimTE r
    if r'.length ≤ degG || r'.all TowerElem.tIsZero then (trimTE q, r')
    else
      let lcR := r'.getLast!
      let c := towerDiv lcR lcG
      let d := r'.length - 1 - degG
      let term := List.replicate d (.tRat 0) ++ [c]
      go degG lcG g' (addCoeffs q term) (subCoeffs r' (polyMulTE term g'))

end -- mutual

-- Instances
instance : Add TowerElem where add := towerAdd
instance : Sub TowerElem where sub := towerSub
instance : Mul TowerElem where mul := towerMul
instance : Div TowerElem where div := towerDiv
instance : Neg TowerElem where neg := towerNeg
instance : OfNat TowerElem 0 where ofNat := .tRat 0
instance : OfNat TowerElem 1 where ofNat := .tRat 1

/-- Adjoin an nth root to the tower: create a new level where α^n = r. -/
def adjoinTowerRoot (lvlId : Int) (n : Int) (r : TowerElem)
    : TowerLevel × TowerElem :=
  let level : TowerLevel := .mk lvlId n n r
  let gen := TowerElem.tExt
    (.tRat 0 :: .tRat 1 :: List.replicate (n.toNat - 2) (.tRat 0)) level
  (level, gen)

/-- Convert a tower element to a radical expression. -/
partial def towerToRadExpr : TowerElem → RadExpr Rat
  | .tRat r => .lit r
  | .tExt cs level =>
    let gen : RadExpr Rat := .root level.tlRootDeg (towerToRadExpr level.tlRadicand)
    let terms := cs.enum.filterMap fun (i, c) =>
      if c.tIsZero then none
      else
        let cExpr := towerToRadExpr c
        match i with
        | 0 => some cExpr
        | 1 => some (.mul cExpr gen)
        | _ => some (.mul cExpr (.pow gen (Int.ofNat i)))
    match terms with
    | [] => .lit 0
    | [t] => t
    | t :: ts => ts.foldl .add t

-- Equality
private partial def towerBEq : TowerElem → TowerElem → Bool
  | .tRat a, .tRat b => a == b
  | .tExt cs1 l1, .tExt cs2 l2 =>
    l1 == l2 && eqCoeffs cs1 cs2
  | .tRat a, .tExt cs _ => eqCoeffs [.tRat a] cs
  | .tExt cs _, .tRat a => eqCoeffs cs [.tRat a]
where
  eqCoeffs : List TowerElem → List TowerElem → Bool
    | [], [] => true
    | [], bs => bs.all TowerElem.tIsZero
    | as, [] => as.all TowerElem.tIsZero
    | a :: as, b :: bs => towerBEq a b && eqCoeffs as bs

instance : BEq TowerElem where beq := towerBEq

end Surd
