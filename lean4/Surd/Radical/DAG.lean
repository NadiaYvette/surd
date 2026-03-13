/-
  Surd.Radical.DAG — Explicit DAG representation for radical expressions.

  Hash-consing via structural equality (no StableName equivalent in Lean).
  Nodes are stored in a HashMap with topological ordering (children < parents).
  Evaluation processes each unique node once, avoiding exponential traversal.
-/
import Surd.Radical.Expr
import Surd.Radical.Eval
import Surd.Interval
import Std.Internal.Rat
import Std.Data.HashMap

open Std.Internal
open Std (HashMap)

namespace Surd

/-- Node identifier in the DAG. -/
abbrev NodeId := Nat

/-- A single operation node. Children are referenced by NodeId. -/
inductive RadNodeOp (k : Type) where
  | nLit : k → RadNodeOp k
  | nNeg : NodeId → RadNodeOp k
  | nAdd : NodeId → NodeId → RadNodeOp k
  | nMul : NodeId → NodeId → RadNodeOp k
  | nInv : NodeId → RadNodeOp k
  | nRoot : Int → NodeId → RadNodeOp k
  | nPow : NodeId → Int → RadNodeOp k
  deriving Repr, BEq, Inhabited

instance {k : Type} [Hashable k] : Hashable (RadNodeOp k) where
  hash
    | .nLit r => mixHash 0 (hash r)
    | .nNeg a => mixHash 1 (hash a)
    | .nAdd a b => mixHash 2 (mixHash (hash a) (hash b))
    | .nMul a b => mixHash 3 (mixHash (hash a) (hash b))
    | .nInv a => mixHash 4 (hash a)
    | .nRoot n a => mixHash 5 (mixHash (hash n) (hash a))
    | .nPow a n => mixHash 6 (mixHash (hash a) (hash n))

/-- An explicit DAG of radical operations. -/
structure RadDAG (k : Type) where
  nodes : Array (RadNodeOp k)
  root : NodeId
  deriving Repr

namespace RadDAG

variable {k : Type} [BEq k] [Hashable k]

/-- State during DAG construction. -/
private structure BuildState (k : Type) [BEq k] [Hashable k] where
  nodes : Array (RadNodeOp k)
  cache : HashMap (RadNodeOp k) NodeId

/-- Allocate a node, reusing existing if structurally equal. -/
private def allocNode (op : RadNodeOp k) (s : BuildState k) : NodeId × BuildState k :=
  match s.cache.get? op with
  | some id => (id, s)
  | none =>
    let id := s.nodes.size
    (id, { nodes := s.nodes.push op, cache := s.cache.insert op id })

/-- Build a DAG from a RadExpr via structural hash-consing. -/
partial def toDAG (expr : RadExpr k) : RadDAG k :=
  let init : BuildState k := { nodes := #[], cache := HashMap.empty }
  let (rootId, state) := go expr init
  { nodes := state.nodes, root := rootId }
where
  go (e : RadExpr k) (s : BuildState k) : NodeId × BuildState k :=
    match e with
    | .lit r => allocNode (.nLit r) s
    | .neg a =>
      let (na, s1) := go a s
      allocNode (.nNeg na) s1
    | .add a b =>
      let (na, s1) := go a s
      let (nb, s2) := go b s1
      allocNode (.nAdd na nb) s2
    | .mul a b =>
      let (na, s1) := go a s
      let (nb, s2) := go b s1
      allocNode (.nMul na nb) s2
    | .inv a =>
      let (na, s1) := go a s
      allocNode (.nInv na) s1
    | .root n a =>
      let (na, s1) := go a s
      allocNode (.nRoot n na) s1
    | .pow a n =>
      let (na, s1) := go a s
      allocNode (.nPow na n) s1

/-- Convert DAG back to RadExpr. -/
def fromDAG [Inhabited k] (dag : RadDAG k) : RadExpr k :=
  let memo := buildMemo dag.nodes
  if h : dag.root < memo.size then memo[dag.root]
  else .lit default
where
  buildMemo (nodes : Array (RadNodeOp k)) : Array (RadExpr k) :=
    nodes.foldl (fun (memo : Array (RadExpr k)) op =>
      let expr := match op with
        | .nLit r => .lit r
        | .nNeg a => .neg (memo.getD a (.lit default))
        | .nAdd a b => .add (memo.getD a (.lit default)) (memo.getD b (.lit default))
        | .nMul a b => .mul (memo.getD a (.lit default)) (memo.getD b (.lit default))
        | .nInv a => .inv (memo.getD a (.lit default))
        | .nRoot n a => .root n (memo.getD a (.lit default))
        | .nPow a n => .pow (memo.getD a (.lit default)) n
      memo.push expr
    ) #[]

/-- Number of unique nodes. -/
def dagSize (dag : RadDAG k) : Nat :=
  dag.nodes.size

/-- Depth of the DAG (longest root-to-leaf path). -/
def dagDepth (dag : RadDAG k) : Nat :=
  let depths := dag.nodes.foldl (fun (memo : Array Nat) op =>
    let d := match op with
      | .nLit _ => 0
      | .nNeg a => 1 + memo.getD a 0
      | .nAdd a b => 1 + max (memo.getD a 0) (memo.getD b 0)
      | .nMul a b => 1 + max (memo.getD a 0) (memo.getD b 0)
      | .nInv a => 1 + memo.getD a 0
      | .nRoot _ a => 1 + memo.getD a 0
      | .nPow a _ => 1 + memo.getD a 0
    memo.push d
  ) #[]
  if dag.nodes.isEmpty then 0
  else depths.getD dag.root 0

end RadDAG

/-- Evaluate a DAG to Complex Float. Each node computed once. -/
def dagEvalComplex (dag : RadDAG Rat) : Complex Float :=
  let vals := dag.nodes.foldl (fun (memo : Array (Complex Float)) op =>
    let v := match op with
      | .nLit r => Complex.ofReal (ratToFloat r)
      | .nNeg a => Complex.neg (memo.getD a Complex.zero)
      | .nAdd a b => Complex.add (memo.getD a Complex.zero) (memo.getD b Complex.zero)
      | .nMul a b => Complex.mul (memo.getD a Complex.zero) (memo.getD b Complex.zero)
      | .nInv a => Complex.inv (memo.getD a Complex.zero)
      | .nRoot n a => complexNthRoot n (memo.getD a Complex.zero)
      | .nPow a n => complexPow (memo.getD a Complex.zero) n
    memo.push v
  ) #[]
  vals.getD dag.root Complex.zero

/-- Evaluate a DAG to rational interval. Each node computed once. -/
def dagEvalInterval (dag : RadDAG Rat) : Interval :=
  let zeroIv := Interval.fromRat 0
  let vals := dag.nodes.foldl (fun (memo : Array Interval) op =>
    let v := match op with
      | .nLit r => Interval.fromRat r
      | .nNeg a => Interval.ineg (memo.getD a zeroIv)
      | .nAdd a b => Interval.iadd (memo.getD a zeroIv) (memo.getD b zeroIv)
      | .nMul a b => Interval.imul (memo.getD a zeroIv) (memo.getD b zeroIv)
      | .nInv a => Interval.iinv (memo.getD a zeroIv)
      | .nRoot n a =>
        let iv := memo.getD a zeroIv
        if n == 2 then Interval.isqrt iv
        else Interval.inth n.toNat iv
      | .nPow a n =>
        if n ≥ 0 then Interval.ipow (memo.getD a zeroIv) n.toNat
        else Interval.iinv (Interval.ipow (memo.getD a zeroIv) (-n).toNat)
    memo.push v
  ) #[]
  vals.getD dag.root zeroIv

end Surd
