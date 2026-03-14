module Surd.DAG

import Surd.Rational
import Surd.Types
import Surd.Positive
import Surd.PrimeFactors
import Surd.Eval

import Data.Nat
import Data.SortedMap
import Data.List

%default covering

------------------------------------------------------------------------
-- DAG node operations
------------------------------------------------------------------------

||| Node identifier in the DAG.
public export
NodeId : Type
NodeId = Int

||| A node operation in the DAG. Children referenced by NodeId.
public export
data RadNodeOp : Type -> Type where
  NLit  : k -> RadNodeOp k
  NNeg  : NodeId -> RadNodeOp k
  NAdd  : NodeId -> NodeId -> RadNodeOp k
  NMul  : NodeId -> NodeId -> RadNodeOp k
  NInv  : NodeId -> RadNodeOp k
  NRoot : Nat -> NodeId -> RadNodeOp k
  NPow  : NodeId -> Int -> RadNodeOp k

export
(Show k) => Show (RadNodeOp k) where
  show (NLit c) = "NLit " ++ show c
  show (NNeg a) = "NNeg " ++ show a
  show (NAdd a b) = "NAdd " ++ show a ++ " " ++ show b
  show (NMul a b) = "NMul " ++ show a ++ " " ++ show b
  show (NInv a) = "NInv " ++ show a
  show (NRoot n a) = "NRoot " ++ show n ++ " " ++ show a
  show (NPow a n) = "NPow " ++ show a ++ " " ++ show n

------------------------------------------------------------------------
-- DAG type
------------------------------------------------------------------------

||| An explicit DAG of radical expression nodes.
||| Nodes stored in a SortedMap indexed by NodeId.
||| Children always have lower NodeIds than parents (topological order).
public export
record RadDAG k where
  constructor MkRadDAG
  dagNodes : SortedMap NodeId (RadNodeOp k)
  dagRoot  : NodeId

export
(Show k) => Show (RadDAG k) where
  show dag = "RadDAG(root=" ++ show (dagRoot dag) ++ ", nodes=" ++ show (Data.SortedMap.toList (dagNodes dag)) ++ ")"

------------------------------------------------------------------------
-- Convert RadExpr to DAG (structural, no StableName)
------------------------------------------------------------------------

||| Convert a RadExpr to an explicit DAG.
||| Without StableName (unavailable in Idris 2), this does structural
||| deduplication via a SortedMap on expression structure.
export
toDAG : (Ord k) => RadExpr k -> RadDAG k
toDAG expr = let (dag, _, rootId) = buildDAG expr empty 0 in MkRadDAG dag rootId
  where
    buildDAG : RadExpr k -> SortedMap NodeId (RadNodeOp k) -> NodeId
             -> (SortedMap NodeId (RadNodeOp k), NodeId, NodeId)
    buildDAG (Lit c) nodes nextId =
      (insert nextId (NLit c) nodes, nextId + 1, nextId)
    buildDAG (Neg a) nodes nextId =
      let (nodes1, next1, aId) = buildDAG a nodes nextId
      in (insert next1 (NNeg aId) nodes1, next1 + 1, next1)
    buildDAG (Add a b) nodes nextId =
      let (nodes1, next1, aId) = buildDAG a nodes nextId
          (nodes2, next2, bId) = buildDAG b nodes1 next1
      in (insert next2 (NAdd aId bId) nodes2, next2 + 1, next2)
    buildDAG (Mul a b) nodes nextId =
      let (nodes1, next1, aId) = buildDAG a nodes nextId
          (nodes2, next2, bId) = buildDAG b nodes1 next1
      in (insert next2 (NMul aId bId) nodes2, next2 + 1, next2)
    buildDAG (Inv a) nodes nextId =
      let (nodes1, next1, aId) = buildDAG a nodes nextId
      in (insert next1 (NInv aId) nodes1, next1 + 1, next1)
    buildDAG (Root n a) nodes nextId =
      let (nodes1, next1, aId) = buildDAG a nodes nextId
      in (insert next1 (NRoot n aId) nodes1, next1 + 1, next1)
    buildDAG (Pow a n) nodes nextId =
      let (nodes1, next1, aId) = buildDAG a nodes nextId
      in (insert next1 (NPow aId n) nodes1, next1 + 1, next1)

------------------------------------------------------------------------
-- Convert DAG back to RadExpr
------------------------------------------------------------------------

||| Convert a DAG back to a RadExpr.
||| Uses a map to share reconstructed subexpressions.
export
fromDAG : RadDAG k -> RadExpr k
fromDAG dag =
  let nodeList = Data.SortedMap.toList (dagNodes dag)
      -- Process nodes in topological order (ascending NodeId)
      sorted = sortBy (\a, b => compare (fst a) (fst b)) nodeList
      exprMap = foldl addNode empty sorted
  in case lookup (dagRoot dag) exprMap of
       Just e => e
       Nothing => Lit (believe_me 0)  -- should not happen
  where
    addNode : SortedMap NodeId (RadExpr k) -> (NodeId, RadNodeOp k) -> SortedMap NodeId (RadExpr k)
    addNode m (nid, NLit c) = insert nid (Lit c) m
    addNode m (nid, NNeg a) = case lookup a m of
      Just ea => insert nid (Neg ea) m
      Nothing => m
    addNode m (nid, NAdd a b) = case (lookup a m, lookup b m) of
      (Just ea, Just eb) => insert nid (Add ea eb) m
      _ => m
    addNode m (nid, NMul a b) = case (lookup a m, lookup b m) of
      (Just ea, Just eb) => insert nid (Mul ea eb) m
      _ => m
    addNode m (nid, NInv a) = case lookup a m of
      Just ea => insert nid (Inv ea) m
      Nothing => m
    addNode m (nid, NRoot n a) = case lookup a m of
      Just ea => insert nid (Root n @{believe_me (the (LTE 0 0) LTEZero)} ea) m
      Nothing => m
    addNode m (nid, NPow a n) = case lookup a m of
      Just ea => insert nid (Pow ea n) m
      Nothing => m

------------------------------------------------------------------------
-- DAG metrics
------------------------------------------------------------------------

||| Number of unique nodes in the DAG.
export
dagSize : RadDAG k -> Nat
dagSize dag = length (Data.SortedMap.toList (dagNodes dag))

||| Maximum depth of the DAG.
export
dagDepth : (Ord k) => RadDAG k -> Nat
dagDepth dag =
  let nodeList = Data.SortedMap.toList (dagNodes dag)
      sorted = sortBy (\a, b => compare (fst a) (fst b)) nodeList
      depthMap = foldl computeDepth empty sorted
  in case lookup (dagRoot dag) depthMap of
       Just d => d
       Nothing => 0
  where
    childDepth : SortedMap NodeId Nat -> NodeId -> Nat
    childDepth m nid = case lookup nid m of Just d => d; Nothing => 0

    computeDepth : SortedMap NodeId Nat -> (NodeId, RadNodeOp k) -> SortedMap NodeId Nat
    computeDepth m (nid, NLit _) = insert nid 0 m
    computeDepth m (nid, NNeg a) = insert nid (1 + childDepth m a) m
    computeDepth m (nid, NAdd a b) = insert nid (1 + max (childDepth m a) (childDepth m b)) m
    computeDepth m (nid, NMul a b) = insert nid (1 + max (childDepth m a) (childDepth m b)) m
    computeDepth m (nid, NInv a) = insert nid (1 + childDepth m a) m
    computeDepth m (nid, NRoot _ a) = insert nid (1 + childDepth m a) m
    computeDepth m (nid, NPow a _) = insert nid (1 + childDepth m a) m

------------------------------------------------------------------------
-- DAG fold constants
------------------------------------------------------------------------

||| Fold constants in a DAG: evaluate pure-literal subtrees.
||| O(n) in the number of unique nodes.
export
dagFoldConstants : RadDAG Rational -> RadDAG Rational
dagFoldConstants dag =
  let nodeList = Data.SortedMap.toList (dagNodes dag)
      sorted = sortBy (\a, b => compare (fst a) (fst b)) nodeList
      newNodes = foldl foldNode empty sorted
  in MkRadDAG newNodes (dagRoot dag)
  where
    getLit : SortedMap NodeId (RadNodeOp Rational) -> NodeId -> Maybe Rational
    getLit m nid = case lookup nid m of
      Just (NLit r) => Just r
      _ => Nothing

    foldNode : SortedMap NodeId (RadNodeOp Rational) -> (NodeId, RadNodeOp Rational) -> SortedMap NodeId (RadNodeOp Rational)
    foldNode m (nid, op@(NLit _)) = insert nid op m
    foldNode m (nid, NNeg a) = case getLit m a of
      Just r => insert nid (NLit (negate r)) m
      Nothing => insert nid (NNeg a) m
    foldNode m (nid, NAdd a b) = case (getLit m a, getLit m b) of
      (Just r, Just s) => insert nid (NLit (r + s)) m
      _ => insert nid (NAdd a b) m
    foldNode m (nid, NMul a b) = case (getLit m a, getLit m b) of
      (Just r, Just s) => insert nid (NLit (r * s)) m
      _ => insert nid (NMul a b) m
    foldNode m (nid, NInv a) = case getLit m a of
      Just r => if not (Surd.Rational.isZero r)
                  then insert nid (NLit (recip r)) m
                  else insert nid (NInv a) m
      Nothing => insert nid (NInv a) m
    foldNode m (nid, NRoot n a) = case getLit m a of
      Just r => if Surd.Rational.isZero r then insert nid (NLit Rational.zero) m
                else if r == Rational.one then insert nid (NLit Rational.one) m
                else insert nid (NRoot n a) m
      Nothing => insert nid (NRoot n a) m
    foldNode m (nid, NPow a n) = case getLit m a of
      Just r => insert nid (NLit (powRatInt r (cast n))) m
      Nothing => insert nid (NPow a n) m

------------------------------------------------------------------------
-- DAG evaluation (Complex Double)
------------------------------------------------------------------------

||| Evaluate a DAG to a complex number. O(n) in unique nodes.
export
dagEvalComplex : RadDAG Rational -> Complex
dagEvalComplex dag =
  let nodeList = Data.SortedMap.toList (dagNodes dag)
      sorted = sortBy (\a, b => compare (fst a) (fst b)) nodeList
      valMap = foldl evalNode empty sorted
  in case lookup (dagRoot dag) valMap of
       Just v => v
       Nothing => (0.0, 0.0)
  where
    ratToDouble : Rational -> Double
    ratToDouble r = cast (numer r) / cast (denom r)

    getVal : SortedMap NodeId Complex -> NodeId -> Complex
    getVal m nid = case lookup nid m of Just v => v; Nothing => (0.0, 0.0)

    cadd : Complex -> Complex -> Complex
    cadd (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

    cmul : Complex -> Complex -> Complex
    cmul (r1, i1) (r2, i2) = (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)

    cinv : Complex -> Complex
    cinv (r, i) = let d = r * r + i * i in (r / d, negate i / d)

    cpowNat : Complex -> Nat -> Complex
    cpowNat _ Z = (1.0, 0.0)
    cpowNat z (S k) = cmul z (cpowNat z k)

    evalNode : SortedMap NodeId Complex -> (NodeId, RadNodeOp Rational) -> SortedMap NodeId Complex
    evalNode m (nid, NLit r) = insert nid (ratToDouble r, 0.0) m
    evalNode m (nid, NNeg a) = let (r, i) = getVal m a in insert nid (negate r, negate i) m
    evalNode m (nid, NAdd a b) = insert nid (cadd (getVal m a) (getVal m b)) m
    evalNode m (nid, NMul a b) = insert nid (cmul (getVal m a) (getVal m b)) m
    evalNode m (nid, NInv a) = insert nid (cinv (getVal m a)) m
    evalNode m (nid, NRoot n a) = insert nid (complexNthRoot (cast n) (getVal m a)) m
    evalNode m (nid, NPow a n) =
      let v = getVal m a
      in if n >= 0 then insert nid (cpowNat v (cast n)) m
         else insert nid (cinv (cpowNat v (cast (negate n)))) m
