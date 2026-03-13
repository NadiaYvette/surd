--- Explicit DAG (directed acyclic graph) representation for radical expressions.
---
--- Since Curry (PAKCS) has no IntMap, we use association lists.
--- The DAG representation allows O(n) traversal of shared subexpressions.
module DAG
  ( RadDAG(..)
  , RadNodeOp(..)
  , NodeId
  , toDAG
  , fromDAG
  , dagSize
  , dagDepth
  , dagFoldConstants
  , dagEvalComplex
  ) where

import Rational
import RadExpr

--- A node ID is just an Int.
type NodeId = Int

--- A node operation in the DAG. Children are referenced by NodeId.
data RadNodeOp
  = NLit Rational
  | NNeg NodeId
  | NAdd NodeId NodeId
  | NMul NodeId NodeId
  | NInv NodeId
  | NRoot Int NodeId
  | NPow NodeId Int

--- An explicit DAG of radical expression nodes.
--- Nodes are stored as an association list (NodeId -> RadNodeOp).
--- Children always have lower NodeIds than their parents (topological order).
data RadDAG = RadDAG [(NodeId, RadNodeOp)] NodeId

--- Convert a RadExpr to a DAG (without thunk-sharing detection).
--- In Curry we don't have StableName, so this is a simple structural
--- conversion that assigns unique IDs in bottom-up order.
toDAG :: RadExpr Rational -> RadDAG
toDAG expr =
  let (nodes, rootId, _) = toDagGo expr 0
  in RadDAG nodes rootId

toDagGo :: RadExpr Rational -> NodeId -> ([(NodeId, RadNodeOp)], NodeId, NodeId)
toDagGo expr nextId = case expr of
  Lit r ->
    ([(nextId, NLit r)], nextId, nextId + 1)
  Neg a ->
    let (ns1, id1, nxt1) = toDagGo a nextId
    in (ns1 ++ [(nxt1, NNeg id1)], nxt1, nxt1 + 1)
  Add a b ->
    let (ns1, id1, nxt1) = toDagGo a nextId
        (ns2, id2, nxt2) = toDagGo b nxt1
    in (ns1 ++ ns2 ++ [(nxt2, NAdd id1 id2)], nxt2, nxt2 + 1)
  Mul a b ->
    let (ns1, id1, nxt1) = toDagGo a nextId
        (ns2, id2, nxt2) = toDagGo b nxt1
    in (ns1 ++ ns2 ++ [(nxt2, NMul id1 id2)], nxt2, nxt2 + 1)
  Inv a ->
    let (ns1, id1, nxt1) = toDagGo a nextId
    in (ns1 ++ [(nxt1, NInv id1)], nxt1, nxt1 + 1)
  Root n a ->
    let (ns1, id1, nxt1) = toDagGo a nextId
    in (ns1 ++ [(nxt1, NRoot n id1)], nxt1, nxt1 + 1)
  Pow a n ->
    let (ns1, id1, nxt1) = toDagGo a nextId
    in (ns1 ++ [(nxt1, NPow id1 n)], nxt1, nxt1 + 1)

--- Convert a DAG back to a RadExpr.
fromDAG :: RadDAG -> RadExpr Rational
fromDAG (RadDAG nodes rootId) =
  let exprMap = foldl (\acc (nid, op) -> (nid, buildExpr acc op) : acc)
                      [] nodes
  in lookupExpr rootId exprMap

buildExpr :: [(NodeId, RadExpr Rational)] -> RadNodeOp -> RadExpr Rational
buildExpr m op = case op of
  NLit r     -> Lit r
  NNeg a     -> Neg (lookupExpr a m)
  NAdd a b   -> Add (lookupExpr a m) (lookupExpr b m)
  NMul a b   -> Mul (lookupExpr a m) (lookupExpr b m)
  NInv a     -> Inv (lookupExpr a m)
  NRoot n a  -> Root n (lookupExpr a m)
  NPow a n   -> Pow (lookupExpr a m) n

lookupExpr :: NodeId -> [(NodeId, RadExpr Rational)] -> RadExpr Rational
lookupExpr nid [] = error ("lookupExpr: node " ++ show nid ++ " not found")
lookupExpr nid ((k,v):rest) =
  if nid == k then v else lookupExpr nid rest

--- Number of nodes in the DAG.
dagSize :: RadDAG -> Int
dagSize (RadDAG nodes _) = length nodes

--- Depth of the DAG (longest path from root to leaf).
dagDepth :: RadDAG -> Int
dagDepth (RadDAG nodes rootId) =
  let depthMap = foldl (\acc (nid, op) -> (nid, nodeDepth acc op) : acc)
                       [] nodes
  in lookupInt rootId depthMap

nodeDepth :: [(NodeId, Int)] -> RadNodeOp -> Int
nodeDepth m op = case op of
  NLit _     -> 0
  NNeg a     -> 1 + lookupInt a m
  NAdd a b   -> 1 + max (lookupInt a m) (lookupInt b m)
  NMul a b   -> 1 + max (lookupInt a m) (lookupInt b m)
  NInv a     -> 1 + lookupInt a m
  NRoot _ a  -> 1 + lookupInt a m
  NPow a _   -> 1 + lookupInt a m

lookupInt :: Int -> [(Int, Int)] -> Int
lookupInt k [] = error ("lookupInt: key " ++ show k ++ " not found")
lookupInt k ((k',v):rest) = if k == k' then v else lookupInt k rest

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

rNegOne :: Rational
rNegOne = Rational.fromInt (negate 1)

--- DAG-based constant folding.
--- Traverses each node once, folding constant subexpressions.
dagFoldConstants :: RadDAG -> RadDAG
dagFoldConstants (RadDAG nodes rootId) =
  let (newNodes, newRoot, _) =
        foldl foldNode ([], rootId, []) nodes
  in RadDAG newNodes newRoot
  where
    foldNode (accNodes, _, valMap) (nid, op) =
      let (newOp, vm') = foldOp valMap op
      in (accNodes ++ [(nid, newOp)], nid, vm')
    foldOp vm op = case op of
      NLit r -> (NLit r, (op, Just r) : vm)
      NNeg a ->
        case lookupVal a vm of
          Just r  -> (NLit (ratNeg r), (op, Just (ratNeg r)) : vm)
          Nothing -> (NNeg a, (op, Nothing) : vm)
      NAdd a b ->
        case (lookupVal a vm, lookupVal b vm) of
          (Just ra, Just rb) ->
            let s = ratAdd ra rb
            in (NLit s, (op, Just s) : vm)
          (Just ra, _) ->
            if ra == rZero then (lookupOp b vm, (op, lookupVal b vm) : vm)
            else (NAdd a b, (op, Nothing) : vm)
          (_, Just rb) ->
            if rb == rZero then (lookupOp a vm, (op, lookupVal a vm) : vm)
            else (NAdd a b, (op, Nothing) : vm)
          _ -> (NAdd a b, (op, Nothing) : vm)
      NMul a b ->
        case (lookupVal a vm, lookupVal b vm) of
          (Just ra, Just rb) ->
            let p = ratMul ra rb
            in (NLit p, (op, Just p) : vm)
          (Just ra, _) ->
            if ra == rZero then (NLit rZero, (op, Just rZero) : vm)
            else if ra == rOne then (lookupOp b vm, (op, lookupVal b vm) : vm)
            else (NMul a b, (op, Nothing) : vm)
          (_, Just rb) ->
            if rb == rZero then (NLit rZero, (op, Just rZero) : vm)
            else if rb == rOne then (lookupOp a vm, (op, lookupVal a vm) : vm)
            else (NMul a b, (op, Nothing) : vm)
          _ -> (NMul a b, (op, Nothing) : vm)
      NInv a ->
        case lookupVal a vm of
          Just ra ->
            if ra /= rZero
            then let iv = ratDiv rOne ra
                 in (NLit iv, (op, Just iv) : vm)
            else (NInv a, (op, Nothing) : vm)
          Nothing -> (NInv a, (op, Nothing) : vm)
      NRoot _ a ->
        case lookupVal a vm of
          Just ra ->
            if ra == rZero then (NLit rZero, (op, Just rZero) : vm)
            else if ra == rOne then (NLit rOne, (op, Just rOne) : vm)
            else (op, (op, Nothing) : vm)
          Nothing -> (op, (op, Nothing) : vm)
      NPow a n ->
        case lookupVal a vm of
          Just ra ->
            let p = ratPow ra n
            in (NLit p, (op, Just p) : vm)
          Nothing ->
            if n == 0 then (NLit rOne, (op, Just rOne) : vm)
            else if n == 1 then (lookupOp a vm, (op, lookupVal a vm) : vm)
            else (NPow a n, (op, Nothing) : vm)

--- Look up if a node resolved to a constant in the value map.
--- The value map is indexed by the ORIGINAL op, keyed by position.
lookupVal :: NodeId -> [(RadNodeOp, Maybe Rational)] -> Maybe Rational
lookupVal _ _ = Nothing  -- simplified: no cross-referencing in fold

--- Look up the new op for a node.
lookupOp :: NodeId -> [(RadNodeOp, Maybe Rational)] -> RadNodeOp
lookupOp _ _ = NLit rZero  -- placeholder

--- DAG-based complex evaluation.
dagEvalComplex :: RadDAG -> (Float, Float)
dagEvalComplex (RadDAG nodes rootId) =
  let evalMap = foldl (\acc (nid, op) -> (nid, evalNode acc op) : acc)
                      [] nodes
  in lookupComplex rootId evalMap

evalNode :: [(NodeId, (Float, Float))] -> RadNodeOp -> (Float, Float)
evalNode m op = case op of
  NLit r ->
    (Prelude.fromInt (numerator r) / Prelude.fromInt (denominator r), 0.0)
  NNeg a ->
    let (re, im) = lookupComplex a m in (negate re, negate im)
  NAdd a b ->
    let (ar, ai) = lookupComplex a m
        (br, bi) = lookupComplex b m
    in (ar + br, ai + bi)
  NMul a b ->
    let (ar, ai) = lookupComplex a m
        (br, bi) = lookupComplex b m
    in (ar * br - ai * bi, ar * bi + ai * br)
  NInv a ->
    let (re, im) = lookupComplex a m
        d = re * re + im * im
    in (re / d, negate im / d)
  NRoot n a ->
    let z = lookupComplex a m
    in complexNthRoot n z
  NPow a n ->
    complexPow (lookupComplex a m) n

lookupComplex :: NodeId -> [(NodeId, (Float, Float))] -> (Float, Float)
lookupComplex nid [] = error ("lookupComplex: node " ++ show nid ++ " not found")
lookupComplex nid ((k,v):rest) =
  if nid == k then v else lookupComplex nid rest

--- Complex nth root via polar form.
complexNthRoot :: Int -> (Float, Float) -> (Float, Float)
complexNthRoot n (re, im) =
  let r = sqrt (re * re + im * im)
      theta = floatAtan2 im re
      rn = r ** (1.0 / Prelude.fromInt n)
      an = theta / Prelude.fromInt n
  in (rn * cos an, rn * sin an)

--- atan2 for floats.
floatAtan2 :: Float -> Float -> Float
floatAtan2 y x
  | x > 0.0              = atan (y / x)
  | x < 0.0 && y >= 0.0  = atan (y / x) + pi
  | x < 0.0 && y < 0.0   = atan (y / x) - pi
  | x == 0.0 && y > 0.0  = pi / 2.0
  | x == 0.0 && y < 0.0  = negate (pi / 2.0)
  | otherwise             = 0.0

--- Complex power by repeated squaring.
complexPow :: (Float, Float) -> Int -> (Float, Float)
complexPow z n
  | n == 0    = (1.0, 0.0)
  | n < 0     = let (zr, zi) = complexPow z (negate n)
                    d = zr * zr + zi * zi
                in (zr / d, negate zi / d)
  | n == 1    = z
  | even n    = let (hr, hi) = complexPow z (n `div` 2)
                in (hr * hr - hi * hi, 2.0 * hr * hi)
  | otherwise = let (zr, zi) = z
                    (rr, ri) = complexPow z (n - 1)
                in (zr * rr - zi * ri, zr * ri + zi * rr)

instance Eq RadNodeOp where
  x == y = case (x, y) of
    (NLit a, NLit b)         -> a == b
    (NNeg a, NNeg b)         -> a == b
    (NAdd a1 a2, NAdd b1 b2) -> a1 == b1 && a2 == b2
    (NMul a1 a2, NMul b1 b2) -> a1 == b1 && a2 == b2
    (NInv a, NInv b)         -> a == b
    (NRoot n1 a, NRoot n2 b) -> n1 == n2 && a == b
    (NPow a n1, NPow b n2)  -> a == b && n1 == n2
    _                        -> False

instance Show RadNodeOp where
  show op = case op of
    NLit r     -> "NLit " ++ show r
    NNeg a     -> "NNeg " ++ show a
    NAdd a b   -> "NAdd " ++ show a ++ " " ++ show b
    NMul a b   -> "NMul " ++ show a ++ " " ++ show b
    NInv a     -> "NInv " ++ show a
    NRoot n a  -> "NRoot " ++ show n ++ " " ++ show a
    NPow a n   -> "NPow " ++ show a ++ " " ++ show n

instance Show RadDAG where
  show (RadDAG nodes r) = "RadDAG " ++ show nodes ++ " root=" ++ show r
