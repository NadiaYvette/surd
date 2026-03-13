implementation module DAG

import StdEnv
import RadExpr
import Rational
import Eval
from Data.Map import :: Map(..), newMap, get, put, fromList, foldrWithKey

// Convert RadExpr to DAG via structural equality CSE.
// Uses a Map from RadExpr to NodeId for deduplication.
toDAG :: !(RadExpr Rational) -> RadDAG
toDAG expr
    # (rootId, nodes, _) = buildDAG expr 0 newMap
    = { dagNodes = nodes, dagRootId = rootId }

buildDAG :: !(RadExpr Rational) !Int !(Map (RadExpr Rational) Int) -> (Int, [(NodeId, RadNodeOp)], (Int, Map (RadExpr Rational) Int))
buildDAG expr nextId cache
    = case get expr cache of
        ?Just nid -> (nid, [], (nextId, cache))
        ?None -> buildNew expr nextId cache

buildNew :: !(RadExpr Rational) !Int !(Map (RadExpr Rational) Int) -> (Int, [(NodeId, RadNodeOp)], (Int, Map (RadExpr Rational) Int))
buildNew (Lit r) nextId cache
    # nid = nextId
    # cache` = put (Lit r) nid cache
    = (nid, [(nid, NLit r)], (nextId + 1, cache`))
buildNew (Neg a) nextId cache
    # (aId, aNodes, (nextId`, cache`)) = buildDAG a nextId cache
    # nid = nextId`
    # cache`` = put (Neg a) nid cache`
    = (nid, aNodes ++ [(nid, NNeg aId)], (nextId` + 1, cache``))
buildNew (Add a b) nextId cache
    # (aId, aNodes, (nextId`, cache`)) = buildDAG a nextId cache
    # (bId, bNodes, (nextId``, cache``)) = buildDAG b nextId` cache`
    # nid = nextId``
    # cache``` = put (Add a b) nid cache``
    = (nid, aNodes ++ bNodes ++ [(nid, NAdd aId bId)], (nextId`` + 1, cache```))
buildNew (Mul a b) nextId cache
    # (aId, aNodes, (nextId`, cache`)) = buildDAG a nextId cache
    # (bId, bNodes, (nextId``, cache``)) = buildDAG b nextId` cache`
    # nid = nextId``
    # cache``` = put (Mul a b) nid cache``
    = (nid, aNodes ++ bNodes ++ [(nid, NMul aId bId)], (nextId`` + 1, cache```))
buildNew (Inv a) nextId cache
    # (aId, aNodes, (nextId`, cache`)) = buildDAG a nextId cache
    # nid = nextId`
    # cache`` = put (Inv a) nid cache`
    = (nid, aNodes ++ [(nid, NInv aId)], (nextId` + 1, cache``))
buildNew (Root n a) nextId cache
    # (aId, aNodes, (nextId`, cache`)) = buildDAG a nextId cache
    # nid = nextId`
    # cache`` = put (Root n a) nid cache`
    = (nid, aNodes ++ [(nid, NRoot n aId)], (nextId` + 1, cache``))
buildNew (Pow a n) nextId cache
    # (aId, aNodes, (nextId`, cache`)) = buildDAG a nextId cache
    # nid = nextId`
    # cache`` = put (Pow a n) nid cache`
    = (nid, aNodes ++ [(nid, NPow aId n)], (nextId` + 1, cache``))

// Rebuild RadExpr from DAG
fromDAG :: !RadDAG -> RadExpr Rational
fromDAG dag
    # nodeMap = fromList dag.dagNodes
    # exprMap = foldl buildExpr newMap (sortBy (\(a,_) (b,_) -> a < b) dag.dagNodes)
    = case get dag.dagRootId exprMap of
        ?Just e -> e
        ?None -> abort "fromDAG: root node not found"
where
    buildExpr :: !(Map Int (RadExpr Rational)) !(Int, RadNodeOp) -> Map Int (RadExpr Rational)
    buildExpr m (nid, NLit r) = put nid (Lit r) m
    buildExpr m (nid, NNeg aId) = put nid (Neg (lookupE m aId)) m
    buildExpr m (nid, NAdd aId bId) = put nid (Add (lookupE m aId) (lookupE m bId)) m
    buildExpr m (nid, NMul aId bId) = put nid (Mul (lookupE m aId) (lookupE m bId)) m
    buildExpr m (nid, NInv aId) = put nid (Inv (lookupE m aId)) m
    buildExpr m (nid, NRoot n aId) = put nid (Root n (lookupE m aId)) m
    buildExpr m (nid, NPow aId n) = put nid (Pow (lookupE m aId) n) m

    lookupE :: !(Map Int (RadExpr Rational)) !Int -> RadExpr Rational
    lookupE m nid = case get nid m of
        ?Just e -> e
        ?None -> abort "fromDAG: node not found"

dagSize :: !RadDAG -> Int
dagSize dag = length dag.dagNodes

dagDepth :: !RadDAG -> Int
dagDepth dag
    # depthMap = foldl computeDepth newMap (sortBy (\(a,_) (b,_) -> a < b) dag.dagNodes)
    = case get dag.dagRootId depthMap of
        ?Just d -> d
        ?None -> 0
where
    computeDepth :: !(Map Int Int) !(Int, RadNodeOp) -> Map Int Int
    computeDepth m (nid, NLit _) = put nid 0 m
    computeDepth m (nid, NNeg a) = put nid (1 + lookupD m a) m
    computeDepth m (nid, NAdd a b) = put nid (1 + max (lookupD m a) (lookupD m b)) m
    computeDepth m (nid, NMul a b) = put nid (1 + max (lookupD m a) (lookupD m b)) m
    computeDepth m (nid, NInv a) = put nid (1 + lookupD m a) m
    computeDepth m (nid, NRoot _ a) = put nid (1 + lookupD m a) m
    computeDepth m (nid, NPow a _) = put nid (1 + lookupD m a) m

    lookupD :: !(Map Int Int) !Int -> Int
    lookupD m nid = case get nid m of
        ?Just d -> d
        ?None -> 0

// DAG-based constant folding
dagFoldConstants :: !RadDAG -> RadDAG
dagFoldConstants dag
    // For now, just convert to tree, fold, convert back
    # expr = fromDAG dag
    # folded = foldConstantsTree expr
    = toDAG folded

foldConstantsTree :: !(RadExpr Rational) -> RadExpr Rational
foldConstantsTree (Lit r) = Lit r
foldConstantsTree (Neg a) = case foldConstantsTree a of
    Lit r -> Lit (~ r)
    a` -> Neg a`
foldConstantsTree (Add a b) = case (foldConstantsTree a, foldConstantsTree b) of
    (Lit r, Lit s) -> Lit (r + s)
    (Lit r, b`) | r == zero -> b`
    (a`, Lit s) | s == zero -> a`
    (a`, b`) -> Add a` b`
foldConstantsTree (Mul a b) = case (foldConstantsTree a, foldConstantsTree b) of
    (Lit r, Lit s) -> Lit (r * s)
    (Lit r, _) | r == zero -> Lit zero
    (_, Lit s) | s == zero -> Lit zero
    (Lit r, b`) | r == one -> b`
    (a`, Lit s) | s == one -> a`
    (a`, b`) -> Mul a` b`
foldConstantsTree (Inv a) = case foldConstantsTree a of
    Lit r | not (r == zero) -> Lit (one / r)
    a` -> Inv a`
foldConstantsTree (Root n a) = case foldConstantsTree a of
    Lit r | r == zero -> Lit zero
    Lit r | r == one -> Lit one
    a` -> Root n a`
foldConstantsTree (Pow a n) = case foldConstantsTree a of
    Lit r -> Lit (ratPow r n)
    a` | n == 0 -> Lit one
    a` | n == 1 -> a`
    a` -> Pow a` n

// DAG-based complex evaluation (delegates to tree eval for simplicity)
dagEvalComplex :: !RadDAG -> (Real, Real)
dagEvalComplex dag = evalComplex (fromDAG dag)
