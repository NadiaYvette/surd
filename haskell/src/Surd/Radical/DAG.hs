-- | Explicit DAG (directed acyclic graph) representation for radical expressions.
--
-- RadExpr is a tree ADT, but Haskell's lazy evaluation creates DAG-shaped
-- expressions through thunk sharing (e.g., Gauss period descent reuses
-- sub-expressions across Chebyshev polynomials and period equations).
-- Tree-walking functions (foldConstants, normalize, exprMetrics) visit
-- shared nodes multiple times, causing exponential blowup and OOM.
--
-- This module converts between RadExpr (with invisible thunk sharing)
-- and RadDAG (with explicit sharing via IntMap). Algorithms on RadDAG
-- process each unique node exactly once, in O(n) time where n is the
-- number of unique nodes (not the exponentially larger tree size).
--
-- The conversion 'toDAG' uses StableName to detect Haskell thunk sharing.
-- The conversion 'fromDAG' reconstructs RadExpr with sharing preserved
-- via IntMap lookup (same key → same thunk).
module Surd.Radical.DAG
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

import Data.Complex (Complex(..), magnitude, mkPolar)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Lazy as IntMapL
import Data.IntMap.Strict (IntMap)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName, hashStableName, eqStableName)
import Surd.Types

type NodeId = Int

-- | A node operation in the DAG. Children are referenced by NodeId.
data RadNodeOp k
  = NLit !k
  | NNeg !NodeId
  | NAdd !NodeId !NodeId
  | NMul !NodeId !NodeId
  | NInv !NodeId
  | NRoot !Int !NodeId
  | NPow !NodeId !Int
  deriving (Eq, Ord, Show)

-- | An explicit DAG of radical expression nodes.
-- Nodes are stored in an IntMap indexed by NodeId.
-- Children always have lower NodeIds than their parents (topological order).
data RadDAG k = RadDAG
  { dagNodes :: !(IntMap (RadNodeOp k))
  , dagRoot  :: !NodeId
  } deriving (Show)

-- | Convert a RadExpr (with thunk sharing) to an explicit DAG.
--
-- Uses StableName-based traversal: when the same thunk is encountered
-- twice, it maps to the same NodeId. This detects sharing that is
-- invisible at the type level.
--
-- Each node is forced (via pattern matching in 'case') during traversal,
-- giving it a stable heap address for StableName. Children are processed
-- before parents, so NodeIds are in topological order.
toDAG :: RadExpr k -> RadDAG k
toDAG expr = unsafePerformIO $ do
  nextRef  <- newIORef (0 :: NodeId)
  -- StableName cache: hash → [(StableName, NodeId)]
  cacheRef <- newIORef (IntMap.empty :: IntMap [(StableName (RadExpr k), NodeId)])
  nodesRef <- newIORef (IntMap.empty :: IntMap (RadNodeOp k))

  let alloc op = do
        nid <- readIORef nextRef
        writeIORef nextRef (nid + 1)
        nodes <- readIORef nodesRef
        writeIORef nodesRef (IntMap.insert nid op nodes)
        return nid

  let go e = do
        sn <- makeStableName e
        let h = hashStableName sn
        cache <- readIORef cacheRef
        let bucket = maybe [] id (IntMap.lookup h cache)
        case lookupSN sn bucket of
          Just nid -> return nid
          Nothing -> do
            nid <- case e of
              Lit k    -> alloc (NLit k)
              Neg a    -> do na <- go a; alloc (NNeg na)
              Add a b  -> do na <- go a; nb <- go b; alloc (NAdd na nb)
              Mul a b  -> do na <- go a; nb <- go b; alloc (NMul na nb)
              Inv a    -> do na <- go a; alloc (NInv na)
              Root n a -> do na <- go a; alloc (NRoot n na)
              Pow a n  -> do na <- go a; alloc (NPow na n)
            -- Re-read cache to get latest state after recursive calls
            cache' <- readIORef cacheRef
            let bucket' = maybe [] id (IntMap.lookup h cache')
            writeIORef cacheRef (IntMap.insert h ((sn, nid) : bucket') cache')
            return nid

  rootId <- go expr
  nodes  <- readIORef nodesRef
  return $ RadDAG nodes rootId

  where
    lookupSN _ [] = Nothing
    lookupSN sn ((sn', v) : rest)
      | eqStableName sn sn' = Just v
      | otherwise           = lookupSN sn rest

-- | Convert a DAG back to RadExpr, preserving sharing.
--
-- Uses IntMap.map to build all nodes lazily. Looking up the same NodeId
-- always returns the same Haskell thunk, so sub-expression sharing is
-- preserved in the resulting RadExpr.
fromDAG :: RadDAG k -> RadExpr k
fromDAG (RadDAG nodes rootId) = memo IntMap.! rootId
  where
    -- MUST use IntMap.Lazy.map here: the lazy knot-tying means each node's
    -- value references other entries in the same map. Strict map would force
    -- all values during construction, causing an infinite loop.
    memo = IntMapL.map build nodes
    build (NLit k)    = Lit k
    build (NNeg a)    = Neg (memo IntMap.! a)
    build (NAdd a b)  = Add (memo IntMap.! a) (memo IntMap.! b)
    build (NMul a b)  = Mul (memo IntMap.! a) (memo IntMap.! b)
    build (NInv a)    = Inv (memo IntMap.! a)
    build (NRoot n a) = Root n (memo IntMap.! a)
    build (NPow a n)  = Pow (memo IntMap.! a) n

-- | Number of unique nodes in the DAG.
dagSize :: RadDAG k -> Int
dagSize = IntMap.size . dagNodes

-- | Depth of the DAG (longest path from root to leaf).
-- O(n) since each node is visited once.
dagDepth :: RadDAG k -> Int
dagDepth (RadDAG nodes rootId) = depths IntMap.! rootId
  where
    -- Use lazy map for knot-tying (each depth references other entries).
    depths = IntMapL.mapWithKey computeDepth nodes
    computeDepth _ (NLit _)    = 0
    computeDepth _ (NNeg a)    = 1 + depths IntMap.! a
    computeDepth _ (NAdd a b)  = 1 + max (depths IntMap.! a) (depths IntMap.! b)
    computeDepth _ (NMul a b)  = 1 + max (depths IntMap.! a) (depths IntMap.! b)
    computeDepth _ (NInv a)    = 1 + depths IntMap.! a
    computeDepth _ (NRoot _ a) = 1 + depths IntMap.! a
    computeDepth _ (NPow a _)  = 1 + depths IntMap.! a

-- | Constant folding on the DAG. Each node is processed exactly once.
--
-- Simplifications:
-- - Lit r op Lit s → Lit (r op s)
-- - Add (Lit 0) x, Add x (Lit 0) → x
-- - Mul (Lit 0) _, Mul _ (Lit 0) → Lit 0
-- - Mul (Lit 1) x, Mul x (Lit 1) → x
-- - Neg (Neg x) → x
-- - Inv (Inv x) → x
-- - Neg (Lit r) → Lit (negate r)
-- - Inv (Lit r) → Lit (1/r) when r /= 0
--
-- Returns a new DAG with the same or fewer nodes.
dagFoldConstants :: RadDAG Rational -> RadDAG Rational
dagFoldConstants (RadDAG nodes rootId) =
  let -- Process nodes in ascending NodeId order (topological order).
      -- For each node, compute: (newNodeId, newNodes, nextFreeId)
      -- where newNodeId might be a pre-existing node (if the node
      -- was eliminated by folding).
      (remap, finalNodes, _) =
        IntMap.foldlWithKey' step (IntMap.empty, IntMap.empty, 0) nodes

      step (rm, ns, nxt) oldId op =
        let -- Remap children to their simplified versions
            r x = rm IntMap.! x
            op' = remapOp r op
        in case simplify ns op' of
             Left existingId ->
               -- This node simplifies to an existing node
               (IntMap.insert oldId existingId rm, ns, nxt)
             Right newOp ->
               -- This node becomes a new (simplified) node
               (IntMap.insert oldId nxt rm, IntMap.insert nxt newOp ns, nxt + 1)

  in RadDAG finalNodes (remap IntMap.! rootId)
  where
    remapOp _ (NLit k)    = NLit k
    remapOp r (NNeg a)    = NNeg (r a)
    remapOp r (NAdd a b)  = NAdd (r a) (r b)
    remapOp r (NMul a b)  = NMul (r a) (r b)
    remapOp r (NInv a)    = NInv (r a)
    remapOp r (NRoot n a) = NRoot n (r a)
    remapOp r (NPow a n)  = NPow (r a) n

    -- Try to simplify a node given the current set of built nodes.
    -- Left nid  = this node is equivalent to existing node nid
    -- Right op  = this node should be created with this op
    simplify ns (NNeg a) = case ns IntMap.! a of
      NLit r  -> Right (NLit (negate r))
      NNeg a' -> Left a'   -- double negation
      _       -> Right (NNeg a)

    simplify ns (NAdd a b) = case (ns IntMap.! a, ns IntMap.! b) of
      (NLit r, NLit s) -> Right (NLit (r + s))
      (NLit 0, _)      -> Left b
      (_, NLit 0)      -> Left a
      _                -> Right (NAdd a b)

    simplify ns (NMul a b) = case (ns IntMap.! a, ns IntMap.! b) of
      (NLit r, NLit s) -> Right (NLit (r * s))
      (NLit 0, _)      -> Right (NLit 0)
      (_, NLit 0)      -> Right (NLit 0)
      (NLit 1, _)      -> Left b
      (_, NLit 1)      -> Left a
      (NLit (-1), _)   -> Right (NNeg b)
      (_, NLit (-1))   -> Right (NNeg a)
      _                -> Right (NMul a b)

    simplify ns (NInv a) = case ns IntMap.! a of
      NLit r | r /= 0 -> Right (NLit (1 / r))
      NInv a'          -> Left a'   -- double inverse
      _                -> Right (NInv a)

    simplify ns (NRoot n a) = case ns IntMap.! a of
      NLit 0 -> Right (NLit 0)
      NLit 1 -> Right (NLit 1)
      _      -> Right (NRoot n a)

    simplify _ op@(NLit _)  = Right op
    simplify _ op@(NPow _ _) = Right op

-- | Evaluate a DAG to Complex Double. Each node evaluated exactly once.
dagEvalComplex :: RadDAG Rational -> Complex Double
dagEvalComplex (RadDAG nodes rootId) = vals IntMap.! rootId
  where
    -- Use lazy map for knot-tying (each value references other entries).
    vals = IntMapL.map ev nodes
    ev (NLit r)    = fromRational r :+ 0
    ev (NNeg a)    = negate (vals IntMap.! a)
    ev (NAdd a b)  = (vals IntMap.! a) + (vals IntMap.! b)
    ev (NMul a b)  = (vals IntMap.! a) * (vals IntMap.! b)
    ev (NInv a)    = 1 / (vals IntMap.! a)
    ev (NRoot n a) = complexNthRoot n (vals IntMap.! a)
    ev (NPow a n)
      | n >= 0    = (vals IntMap.! a) ^ n
      | otherwise = 1 / ((vals IntMap.! a) ^ negate n)

    complexNthRoot n z =
      let r     = magnitude z
          theta = atan2 (imagPart z) (realPart z)
      in mkPolar (r ** (1 / fromIntegral n)) (theta / fromIntegral n)
      where
        realPart (x :+ _) = x
        imagPart (_ :+ y) = y
