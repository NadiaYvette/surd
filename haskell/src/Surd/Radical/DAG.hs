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
import Data.Ratio (numerator, denominator)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName, hashStableName, eqStableName)
import Surd.Types
import Surd.Internal.Positive (Positive)
import Surd.Internal.PrimeFactors (factorise)

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

-- | DAG simplification pass. Each node is processed exactly once in O(n).
--
-- Performs constant folding, power simplification, and perfect power extraction:
-- - Lit r op Lit s → Lit (r op s)
-- - Add (Lit 0) x, Add x (Lit 0) → x
-- - Mul (Lit 0) _, Mul _ (Lit 0) → Lit 0
-- - Mul (Lit 1) x, Mul x (Lit 1) → x
-- - Neg (Neg x) → x, Inv (Inv x) → x
-- - Neg (Lit r) → Lit (negate r), Inv (Lit r) → Lit (1/r)
-- - Pow (Root n a) n → a, Root n (Pow a n) → a
-- - Pow (Pow a m) n → Pow a (m*n)
-- - Root m (Root n a) → Root (m*n) a
-- - Mul (Root 2 a) (Root 2 a) → a  (same node: √x·√x = x)
-- - Root n (Lit r) → Lit coeff * Root n (Lit inner) (perfect power extraction)
--
-- Returns a new DAG with the same or fewer nodes.
dagFoldConstants :: RadDAG Rational -> RadDAG Rational
dagFoldConstants (RadDAG nodes rootId) =
  let (remap, finalNodes, _) =
        IntMap.foldlWithKey' step (IntMap.empty, IntMap.empty, 0) nodes

      step (rm, ns, nxt) oldId op =
        let r x = rm IntMap.! x
            op' = remapOp r op
        in case simplify ns nxt op' of
             Left existingId ->
               (IntMap.insert oldId existingId rm, ns, nxt)
             Right newOps ->
               let (finalId, ns', nxt') = allocChain ns nxt newOps
               in (IntMap.insert oldId finalId rm, ns', nxt')

      allocChain ns nxt [] = (nxt - 1, ns, nxt)
      allocChain ns nxt [op] = (nxt, IntMap.insert nxt op ns, nxt + 1)
      allocChain ns nxt (op:ops) =
        allocChain (IntMap.insert nxt op ns) (nxt + 1) ops

  in RadDAG finalNodes (remap IntMap.! rootId)
  where
    remapOp _ (NLit k)    = NLit k
    remapOp r (NNeg a)    = NNeg (r a)
    remapOp r (NAdd a b)  = NAdd (r a) (r b)
    remapOp r (NMul a b)  = NMul (r a) (r b)
    remapOp r (NInv a)    = NInv (r a)
    remapOp r (NRoot n a) = NRoot n (r a)
    remapOp r (NPow a n)  = NPow (r a) n

    -- Try to simplify a node. Takes the current node map and next free ID.
    -- Left nid   = reuse existing node
    -- Right ops  = allocate these nodes in order (last is result)
    simplify :: IntMap (RadNodeOp Rational) -> NodeId -> RadNodeOp Rational
             -> Either NodeId [RadNodeOp Rational]
    simplify ns _ (NNeg a) = case ns IntMap.! a of
      NLit r  -> Right [NLit (negate r)]
      NNeg a' -> Left a'
      _       -> Right [NNeg a]

    simplify ns _ (NAdd a b) = case (ns IntMap.! a, ns IntMap.! b) of
      (NLit r, NLit s) -> Right [NLit (r + s)]
      (NLit 0, _)      -> Left b
      (_, NLit 0)      -> Left a
      _                -> Right [NAdd a b]

    simplify ns _ (NMul a b) = case (ns IntMap.! a, ns IntMap.! b) of
      (NLit r, NLit s)         -> Right [NLit (r * s)]
      (NLit 0, _)              -> Right [NLit 0]
      (_, NLit 0)              -> Right [NLit 0]
      (NLit 1, _)              -> Left b
      (_, NLit 1)              -> Left a
      (NLit (-1), _)           -> Right [NNeg b]
      (_, NLit (-1))           -> Right [NNeg a]
      -- √a · √a = a (same node)
      (NRoot 2 ra, NRoot 2 rb) | ra == rb -> Left ra
      _                        -> Right [NMul a b]

    simplify ns _ (NInv a) = case ns IntMap.! a of
      NLit r | r /= 0 -> Right [NLit (1 / r)]
      NInv a'          -> Left a'
      _                -> Right [NInv a]

    simplify ns nxt (NRoot n a) = case ns IntMap.! a of
      NLit 0             -> Right [NLit 0]
      NLit 1             -> Right [NLit 1]
      NLit r | r > 0     -> extractPerfectPowerDAG nxt n r
      NRoot m a'         -> Right [NRoot (m * n) a']
      NPow a' m | m == n -> Left a'
      _                  -> Right [NRoot n a]

    simplify ns _ (NPow a n) = case ns IntMap.! a of
      _ | n == 0            -> Right [NLit 1]
      _ | n == 1            -> Left a
      NLit r                -> Right [NLit (r ^^ n)]
      NPow a' m             -> Right [NPow a' (m * n)]
      NRoot nr a' | nr == n -> Left a'
      _                     -> Right [NPow a n]

    simplify _ _ op@(NLit _) = Right [op]

    -- Extract perfect nth powers from Root n (Lit r) where r > 0.
    -- Uses nxt to assign correct NodeIds for multi-node chains.
    extractPerfectPowerDAG :: NodeId -> Int -> Rational
                           -> Either NodeId [RadNodeOp Rational]
    extractPerfectPowerDAG nxt n r =
      let num = numerator r
          den = denominator r
          (numOut, numIn) = extractNthPower n num
          (denOut, denIn) = extractNthPower n den
          (outerCoeff, innerRat)
            | denIn == 1 = (numOut / denOut, numIn)
            | otherwise  =
                let newInner = numIn * denIn ^ (n - 1 :: Int)
                    newOuter = numOut / (denOut * denIn)
                    (numOut2, numIn2) = extractNthPower n (numerator newInner)
                in (newOuter * numOut2, numIn2)
      in case (outerCoeff == 1, innerRat == 1) of
           (True, True)  -> Right [NLit 1]
           (True, False) -> Right [NLit innerRat, NRoot n nxt]
           (_, True)     -> Right [NLit outerCoeff]
           _             -> Right [ NLit innerRat               -- nxt
                                  , NRoot n nxt                  -- nxt+1
                                  , NLit outerCoeff              -- nxt+2
                                  , NMul (nxt + 2) (nxt + 1)    -- nxt+3
                                  ]

-- | Given n and a positive integer m, extract the largest perfect nth power
-- that divides m. Returns (extracted, remainder) so m = extracted^n * remainder.
extractNthPower :: Int -> Integer -> (Rational, Rational)
extractNthPower n m =
  let fs = factorise (fromInteger (abs m) :: Positive)
      extracted = product [ p ^ (e `div` n) | (p, e) <- fs ]
      remainder = product [ p ^ (e `mod` n) | (p, e) <- fs ]
  in (fromInteger extracted, fromInteger remainder)

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
