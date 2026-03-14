-- |
-- Module      : Surd.Radical.DAG
-- Description : Explicit DAG representation for radical expressions with sharing detection
-- Stability   : experimental
--
-- 'RadExpr' is a tree ADT, but Haskell's lazy evaluation creates DAG-shaped
-- expressions through thunk sharing (e.g., Gauss period descent reuses
-- sub-expressions across Chebyshev polynomials and period equations).
-- Tree-walking functions ('foldConstants', 'normalize', metrics) visit
-- shared nodes multiple times, causing exponential blowup and OOM.
--
-- This module converts between 'RadExpr' (with invisible thunk sharing)
-- and 'RadDAG' (with explicit sharing via 'IntMap'). Algorithms on 'RadDAG'
-- process each unique node exactly once, in O(n) time where n is the
-- number of unique nodes (not the exponentially larger tree size).
--
-- === StableName technique
--
-- The conversion 'toDAG' uses 'System.Mem.StableName.StableName' to detect
-- Haskell thunk sharing: when the same thunk is encountered twice during
-- traversal, it maps to the same 'NodeId'. Each node is forced (via pattern
-- matching) during traversal, giving it a stable heap address. Children are
-- processed before parents, so 'NodeId's are in topological order.
--
-- The reconstruction 'fromDAG' uses lazy 'IntMap' lookup to preserve sharing:
-- the same 'NodeId' always returns the same Haskell thunk.
module Surd.Radical.DAG
  ( -- * Types
    RadDAG (..),
    RadNodeOp (..),
    NodeId,

    -- * Conversion
    toDAG,
    fromDAG,

    -- * Metrics
    dagSize,
    dagDepth,

    -- * DAG-native simplification
    dagFoldConstants,
    dagNormalize,

    -- * Evaluation
    dagEvalComplex,
    dagEvalComplexInterval,
  )
where

import Data.Bifunctor (first)
import Data.Complex (Complex (..), magnitude, mkPolar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Lazy qualified as IntMapL
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Math.Internal.Interval (ComplexInterval (..), Interval (..))
import Math.Internal.Interval qualified as I
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)

-- | Unique identifier for a node in the DAG.
type NodeId = Int

-- | A node operation in the DAG. Children are referenced by 'NodeId'
-- rather than by direct pointers, making sharing explicit.
data RadNodeOp k
  = -- | Literal coefficient.
    NLit !k
  | -- | Negation of node @a@.
    NNeg !NodeId
  | -- | Sum of nodes @a@ and @b@.
    NAdd !NodeId !NodeId
  | -- | Product of nodes @a@ and @b@.
    NMul !NodeId !NodeId
  | -- | Multiplicative inverse of node @a@.
    NInv !NodeId
  | -- | Principal @n@th root of node @a@.
    NRoot !Int !NodeId
  | -- | Integer power of node @a@ raised to @n@.
    NPow !NodeId !Int
  deriving (Eq, Ord, Show)

-- | An explicit DAG (directed acyclic graph) of radical expression nodes.
--
-- Nodes are stored in an 'IntMap' indexed by 'NodeId'. Children always
-- have lower 'NodeId's than their parents (topological order), enabling
-- single-pass bottom-up algorithms.
data RadDAG k = RadDAG
  { -- | All nodes in the DAG, indexed by 'NodeId'.
    dagNodes :: !(IntMap (RadNodeOp k)),
    -- | The 'NodeId' of the root (top-level) expression.
    dagRoot :: !NodeId
  }
  deriving (Show)

-- | Convert a 'RadExpr' (with thunk sharing) to an explicit 'RadDAG'.
--
-- Uses 'StableName'-based traversal: when the same thunk is encountered
-- twice, it maps to the same 'NodeId'. This detects sharing that is
-- invisible at the type level.
--
-- Each node is forced (via pattern matching in @case@) during traversal,
-- giving it a stable heap address for 'StableName'. Children are processed
-- before parents, so 'NodeId's are in topological order.
toDAG :: RadExpr k -> RadDAG k
toDAG expr = unsafePerformIO $ do
  nextRef <- newIORef (0 :: NodeId)
  -- StableName cache: hash -> [(StableName, NodeId)]
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
        let bucket = fromMaybe [] (IntMap.lookup h cache)
        case lookupSN sn bucket of
          Just nid -> return nid
          Nothing -> do
            nid <- case e of
              Lit k -> alloc (NLit k)
              Neg a -> do na <- go a; alloc (NNeg na)
              Add a b -> do na <- go a; nb <- go b; alloc (NAdd na nb)
              Mul a b -> do na <- go a; nb <- go b; alloc (NMul na nb)
              Inv a -> do na <- go a; alloc (NInv na)
              Root n a -> do na <- go a; alloc (NRoot n na)
              Pow a n -> do na <- go a; alloc (NPow na n)
            -- Re-read cache to get latest state after recursive calls
            cache' <- readIORef cacheRef
            let bucket' = fromMaybe [] (IntMap.lookup h cache')
            writeIORef cacheRef (IntMap.insert h ((sn, nid) : bucket') cache')
            return nid

  rootId <- go expr
  nodes <- readIORef nodesRef
  return $ RadDAG nodes rootId
  where
    lookupSN _ [] = Nothing
    lookupSN sn ((sn', v) : rest)
      | eqStableName sn sn' = Just v
      | otherwise = lookupSN sn rest

-- | Convert a 'RadDAG' back to 'RadExpr', preserving sharing.
--
-- Uses lazy 'IntMap.Lazy.map' to build all nodes. Looking up the same
-- 'NodeId' always returns the same Haskell thunk, so sub-expression
-- sharing is preserved in the resulting 'RadExpr'.
fromDAG :: RadDAG k -> RadExpr k
fromDAG (RadDAG nodes rootId) = memo IntMap.! rootId
  where
    -- MUST use IntMap.Lazy.map here: the lazy knot-tying means each node's
    -- value references other entries in the same map. Strict map would force
    -- all values during construction, causing an infinite loop.
    memo = IntMapL.map build nodes
    build (NLit k) = Lit k
    build (NNeg a) = Neg (memo IntMap.! a)
    build (NAdd a b) = Add (memo IntMap.! a) (memo IntMap.! b)
    build (NMul a b) = Mul (memo IntMap.! a) (memo IntMap.! b)
    build (NInv a) = Inv (memo IntMap.! a)
    build (NRoot n a) = Root n (memo IntMap.! a)
    build (NPow a n) = Pow (memo IntMap.! a) n

-- | Number of unique nodes in the DAG. O(1).
dagSize :: RadDAG k -> Int
dagSize = IntMap.size . dagNodes

-- | Depth of the DAG (longest path from root to any leaf).
--
-- O(n) since each node is visited once via lazy knot-tied 'IntMap'.
dagDepth :: RadDAG k -> Int
dagDepth (RadDAG nodes rootId) = depths IntMap.! rootId
  where
    -- Use lazy map for knot-tying (each depth references other entries).
    depths = IntMapL.mapWithKey computeDepth nodes
    computeDepth _ (NLit _) = 0
    computeDepth _ (NNeg a) = 1 + depths IntMap.! a
    computeDepth _ (NAdd a b) = 1 + max (depths IntMap.! a) (depths IntMap.! b)
    computeDepth _ (NMul a b) = 1 + max (depths IntMap.! a) (depths IntMap.! b)
    computeDepth _ (NInv a) = 1 + depths IntMap.! a
    computeDepth _ (NRoot _ a) = 1 + depths IntMap.! a
    computeDepth _ (NPow a _) = 1 + depths IntMap.! a

-- | DAG-native constant folding and simplification pass.
--
-- Each node is processed exactly once in O(n). Performs:
--
-- * Constant folding: @Lit r `op` Lit s@ evaluates to @Lit (r `op` s)@
-- * Identity elimination: @Add (Lit 0) x@, @Mul (Lit 1) x@, etc.
-- * Double-negation/inversion cancellation: @Neg (Neg x)@, @Inv (Inv x)@
-- * Power simplification: @Pow (Root n a) n@ cancels, @Root m (Root n a)@ composes
-- * Perfect power extraction: @Root n (Lit r)@ extracts nth power factors
-- * Same-node root squaring: @Mul (Root 2 a) (Root 2 a)@ when @a@ is the same node
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
      allocChain ns nxt (op : ops) =
        allocChain (IntMap.insert nxt op ns) (nxt + 1) ops
   in RadDAG finalNodes (remap IntMap.! rootId)
  where
    remapOp _ (NLit k) = NLit k
    remapOp r (NNeg a) = NNeg (r a)
    remapOp r (NAdd a b) = NAdd (r a) (r b)
    remapOp r (NMul a b) = NMul (r a) (r b)
    remapOp r (NInv a) = NInv (r a)
    remapOp r (NRoot n a) = NRoot n (r a)
    remapOp r (NPow a n) = NPow (r a) n

    -- Try to simplify a node. Takes the current node map and next free ID.
    -- Left nid   = reuse existing node
    -- Right ops  = allocate these nodes in order (last is result)
    simplify ::
      IntMap (RadNodeOp Rational) ->
      NodeId ->
      RadNodeOp Rational ->
      Either NodeId [RadNodeOp Rational]
    simplify ns _ (NNeg a) = case ns IntMap.! a of
      NLit r -> Right [NLit (negate r)]
      NNeg a' -> Left a'
      _ -> Right [NNeg a]
    simplify ns _ (NAdd a b) = case (ns IntMap.! a, ns IntMap.! b) of
      (NLit r, NLit s) -> Right [NLit (r + s)]
      (NLit 0, _) -> Left b
      (_, NLit 0) -> Left a
      _ -> Right [NAdd a b]
    simplify ns _ (NMul a b) = case (ns IntMap.! a, ns IntMap.! b) of
      (NLit r, NLit s) -> Right [NLit (r * s)]
      (NLit 0, _) -> Right [NLit 0]
      (_, NLit 0) -> Right [NLit 0]
      (NLit 1, _) -> Left b
      (_, NLit 1) -> Left a
      (NLit (-1), _) -> Right [NNeg b]
      (_, NLit (-1)) -> Right [NNeg a]
      -- sqrt(a) * sqrt(a) = a (same node)
      (NRoot 2 ra, NRoot 2 rb) | ra == rb -> Left ra
      _ -> Right [NMul a b]
    simplify ns _ (NInv a) = case ns IntMap.! a of
      NLit r | r /= 0 -> Right [NLit (1 / r)]
      NInv a' -> Left a'
      _ -> Right [NInv a]
    simplify ns nxt (NRoot n a) = case ns IntMap.! a of
      NLit 0 -> Right [NLit 0]
      NLit 1 -> Right [NLit 1]
      NLit r | r > 0 -> extractPerfectPowerDAG nxt n r
      NRoot m a' -> Right [NRoot (m * n) a']
      NPow a' m | m == n -> Left a'
      _ -> Right [NRoot n a]
    simplify ns _ (NPow a n) = case ns IntMap.! a of
      _ | n == 0 -> Right [NLit 1]
      _ | n == 1 -> Left a
      NLit r -> Right [NLit (r ^^ n)]
      NPow a' m -> Right [NPow a' (m * n)]
      NRoot nr a' | nr == n -> Left a'
      _ -> Right [NPow a n]
    simplify _ _ op@(NLit _) = Right [op]

    -- Extract perfect nth powers from Root n (Lit r) where r > 0.
    -- Uses nxt to assign correct NodeIds for multi-node chains.
    extractPerfectPowerDAG ::
      NodeId ->
      Int ->
      Rational ->
      Either NodeId [RadNodeOp Rational]
    extractPerfectPowerDAG nxt n r =
      let num = numerator r
          den = denominator r
          (numOut, numIn) = extractNthPower n num
          (denOut, denIn) = extractNthPower n den
          (outerCoeff, innerRat)
            | denIn == 1 = (numOut / denOut, numIn)
            | otherwise =
                let newInner = numIn * denIn ^ (n - 1 :: Int)
                    newOuter = numOut / (denOut * denIn)
                    (numOut2, numIn2) = extractNthPower n (numerator newInner)
                 in (newOuter * numOut2, numIn2)
       in case (outerCoeff == 1, innerRat == 1) of
            (True, True) -> Right [NLit 1]
            (True, False) -> Right [NLit innerRat, NRoot n nxt]
            (_, True) -> Right [NLit outerCoeff]
            _ ->
              Right
                [ NLit innerRat, -- nxt
                  NRoot n nxt, -- nxt+1
                  NLit outerCoeff, -- nxt+2
                  NMul (nxt + 2) (nxt + 1) -- nxt+3
                ]

-- | Given n and a positive integer m, extract the largest perfect nth power
-- that divides m. Returns (extracted, remainder) so m = extracted^n * remainder.
extractNthPower :: Int -> Integer -> (Rational, Rational)
extractNthPower n m =
  let fs = factorise (fromInteger (abs m) :: Positive)
      extracted = product [p ^ (e `div` n) | (p, e) <- fs]
      remainder = product [p ^ (e `mod` n) | (p, e) <- fs]
   in (fromInteger extracted, fromInteger remainder)

-- | DAG-native normalization pass: algebraic simplifications beyond constant folding.
--
-- Performs like-term collection in 'Add' chains and coefficient merging
-- in 'Mul' chains -- the DAG equivalents of 'collectTerms' and
-- 'collectCoefficients' from "Surd.Radical.Normalize".
--
-- Runs in O(n*k) where n = unique nodes and k = max chain length.
-- Unlike tree-based normalize, never breaks DAG sharing.
dagNormalize :: RadDAG Rational -> RadDAG Rational
dagNormalize (RadDAG nodes rootId) =
  let (remap, finalNodes, _) =
        IntMap.foldlWithKey' step (IntMap.empty, IntMap.empty, 0) nodes

      step (rm, ns, nxt) oldId op =
        let r x = rm IntMap.! x
            op' = remapOp r op
         in case normStep ns nxt op' of
              Left existingId ->
                (IntMap.insert oldId existingId rm, ns, nxt)
              Right newOps ->
                let (finalId, ns', nxt') = allocChain ns nxt newOps
                 in (IntMap.insert oldId finalId rm, ns', nxt')

      allocChain ns nxt [] = (nxt - 1, ns, nxt)
      allocChain ns nxt [op] = (nxt, IntMap.insert nxt op ns, nxt + 1)
      allocChain ns nxt (op : ops) =
        allocChain (IntMap.insert nxt op ns) (nxt + 1) ops
   in RadDAG finalNodes (remap IntMap.! rootId)
  where
    remapOp _ (NLit k) = NLit k
    remapOp r (NNeg a) = NNeg (r a)
    remapOp r (NAdd a b) = NAdd (r a) (r b)
    remapOp r (NMul a b) = NMul (r a) (r b)
    remapOp r (NInv a) = NInv (r a)
    remapOp r (NRoot n a) = NRoot n (r a)
    remapOp r (NPow a n) = NPow (r a) n

    normStep ::
      IntMap (RadNodeOp Rational) ->
      NodeId ->
      RadNodeOp Rational ->
      Either NodeId [RadNodeOp Rational]

    -- Collect like terms in Add chains: flatten, group by base, sum coefficients.
    normStep ns nxt (NAdd a b) =
      let terms = flattenAdd ns a ++ flattenAdd ns b
          grouped =
            Map.toAscList $
              foldl
                ( \m (coeff, base) ->
                    Map.insertWith (+) base coeff m
                )
                Map.empty
                terms
          nonzero = [(c, base) | (base, c) <- grouped, c /= 0]
       in case nonzero of
            [] -> Right [NLit 0]
            [(c, base)] -> buildScaled nxt c base
            _ -> buildSum nxt nonzero
    -- Collect Lit factors in Mul chains: flatten, multiply lits, keep rest.
    normStep ns nxt (NMul a b) =
      let factors = flattenMul ns a ++ flattenMul ns b
          (lits, rest) =
            foldr
              ( \f (ls, rs) -> case f of
                  Left r -> (r * ls, rs)
                  Right n -> (ls, n : rs)
              )
              (1, [])
              factors
       in case (lits == 0, rest) of
            (True, _) -> Right [NLit 0]
            (_, []) -> Right [NLit lits]
            (_, _)
              | lits == 1 -> buildProduct nxt rest
              | lits == -1 -> case buildProduct nxt rest of
                  Left nid -> Right [NNeg nid]
                  Right ops ->
                    let lastId = nxt + length ops - 1
                     in Right (ops ++ [NNeg lastId])
              | otherwise -> case buildProduct nxt rest of
                  Left nid -> Right [NLit lits, NMul nxt nid]
                  Right ops ->
                    let litOp = NLit lits
                     in Right (ops ++ [litOp, NMul (nxt + length ops) (nxt + length ops - 1)])
    -- Distribute: Lit * (a + b) -> Lit*a + Lit*b
    -- Check after Mul normalization
    normStep ns _ op@(NNeg a) = case ns IntMap.! a of
      NLit r -> Right [NLit (negate r)]
      NNeg a' -> Left a'
      _ -> Right [op]
    normStep _ _ op = Right [op]

    -- Flatten an Add chain into [(coefficient, base NodeId)].
    -- Lit r -> (r, Nothing) represented as (r, -1) sentinel
    -- Neg x -> negate coefficient
    -- Mul (Lit c) x -> (c, x)
    flattenAdd :: IntMap (RadNodeOp Rational) -> NodeId -> [(Rational, NodeId)]
    flattenAdd ns nid = case ns IntMap.! nid of
      NAdd a b -> flattenAdd ns a ++ flattenAdd ns b
      NLit r -> [(r, -1)] -- sentinel: pure literal
      NNeg a -> map (first negate) (flattenAdd ns a)
      NMul a b -> case (ns IntMap.! a, ns IntMap.! b) of
        (NLit c, _) -> [(c, b)]
        (_, NLit c) -> [(c, a)]
        _ -> [(1, nid)]
      _ -> [(1, nid)]

    -- Flatten a Mul chain into [Either Rational NodeId].
    -- Left r = literal factor, Right nid = non-literal factor.
    flattenMul :: IntMap (RadNodeOp Rational) -> NodeId -> [Either Rational NodeId]
    flattenMul ns nid = case ns IntMap.! nid of
      NMul a b -> flattenMul ns a ++ flattenMul ns b
      NLit r -> [Left r]
      NInv a -> case ns IntMap.! a of
        NLit r | r /= 0 -> [Left (1 / r)]
        _ -> [Right nid]
      _ -> [Right nid]

    -- Build a single scaled term: c * base (or just Lit c, or just base)
    buildScaled ::
      NodeId ->
      Rational ->
      NodeId ->
      Either NodeId [RadNodeOp Rational]
    buildScaled _ c (-1) = Right [NLit c] -- pure literal
    buildScaled _ 1 base = Left base
    buildScaled _ (-1) base = Right [NNeg base]
    buildScaled nxt c base = Right [NLit c, NMul nxt base]

    -- Build a sum from [(coefficient, base NodeId)]
    buildSum :: NodeId -> [(Rational, NodeId)] -> Either NodeId [RadNodeOp Rational]
    buildSum nxt terms =
      let -- Build each scaled term, collecting ops and tracking IDs
          (ops, ids, nextId) =
            foldl
              ( \(accOps, accIds, nid) (c, base) ->
                  case buildScaled nid c base of
                    Left existId -> (accOps, existId : accIds, nid)
                    Right newOps ->
                      let termId = nid + length newOps - 1
                       in (accOps ++ newOps, termId : accIds, nid + length newOps)
              )
              ([], [], nxt)
              terms
       in case reverse ids of
            [] -> Right [NLit 0]
            [x] -> if null ops then Left x else Right ops
            (hd : rest) ->
              let addChain =
                    foldl
                      ( \(prevId, acc) y ->
                          let newId = nextId + length acc
                           in (newId, acc ++ [NAdd prevId y])
                      )
                      (hd, [])
                      rest
               in Right (ops ++ snd addChain)

    -- Build a product from [NodeId] (non-literal factors)
    buildProduct :: NodeId -> [NodeId] -> Either NodeId [RadNodeOp Rational]
    buildProduct _ [] = Right [NLit 1]
    buildProduct _ [x] = Left x
    buildProduct nxt (x : xs) =
      let chain =
            foldl
              ( \(prevId, acc) y ->
                  let newId = nxt + length acc
                   in (newId, acc ++ [NMul prevId y])
              )
              (x, [])
              xs
       in Right (snd chain)

-- | Evaluate a DAG to 'Complex Double'. Each node is evaluated exactly once.
--
-- Uses lazy 'IntMap' for knot-tying: each node's value references other
-- entries in the same map. Total work is O(n) in DAG size.
dagEvalComplex :: RadDAG Rational -> Complex Double
dagEvalComplex (RadDAG nodes rootId) = vals IntMap.! rootId
  where
    -- Use lazy map for knot-tying (each value references other entries).
    vals = IntMapL.map ev nodes
    ev (NLit r) = fromRational r :+ 0
    ev (NNeg a) = negate (vals IntMap.! a)
    ev (NAdd a b) = (vals IntMap.! a) + (vals IntMap.! b)
    ev (NMul a b) = (vals IntMap.! a) * (vals IntMap.! b)
    ev (NInv a) = 1 / (vals IntMap.! a)
    ev (NRoot n a) = complexNthRoot n (vals IntMap.! a)
    ev (NPow a n)
      | n >= 0 = (vals IntMap.! a) ^ n
      | otherwise = 1 / ((vals IntMap.! a) ^ negate n)

    complexNthRoot n z =
      -- Normalize +/-0 in imaginary part to avoid IEEE 754 sign-of-zero
      -- issues: sqrt(-45e6 + (-0.0)i) would give -6708i instead of +6708i
      -- because atan2(-0.0, -45e6) = -pi instead of +pi.
      let re = realPart z
          im = imagPart z
          im' = if im == 0 then 0 else im  -- canonicalise -0.0 -> +0.0
          r = magnitude z
          theta = atan2 im' re
       in mkPolar (r ** (1 / fromIntegral n)) (theta / fromIntegral n)
      where
        realPart (x :+ _) = x
        imagPart (_ :+ y) = y

-- | Evaluate a DAG to a rigorous 'ComplexInterval' enclosure.
--
-- Each node is evaluated exactly once. Uses rational interval arithmetic
-- for rigorous bounds, avoiding the Double precision loss that
-- 'dagEvalComplex' suffers at depth > 50. Fast for constructible angles
-- (~5ms, width ~1e-18). Slower for complex intermediates due to
-- rigorous complex nth root computation.
dagEvalComplexInterval :: RadDAG Rational -> ComplexInterval
dagEvalComplexInterval (RadDAG nodes rootId) = vals IntMap.! rootId
  where
    vals = IntMapL.map ev nodes

    ev (NLit r) = I.ciFromRational r
    ev (NNeg a) = I.cineg (vals IntMap.! a)
    ev (NAdd a b) = I.ciadd (vals IntMap.! a) (vals IntMap.! b)
    ev (NMul a b) = I.cimul (vals IntMap.! a) (vals IntMap.! b)
    ev (NInv a) = I.ciinv (vals IntMap.! a)
    ev (NPow a n) = I.cipow (vals IntMap.! a) n
    ev (NRoot n a) =
      let ci = vals IntMap.! a
          rePart = I.ciReal ci
          imPart = I.ciImag ci
       in if lo imPart >= 0 && hi imPart <= 0 && lo rePart >= 0
            then -- Non-negative real: use real nth root
              I.ciFromReal (I.inth n rePart)
            else
              if lo imPart >= 0 && hi imPart <= 0 && hi rePart <= 0 && odd n
                then -- Negative real, odd root
                  let pos = I.inth n (Interval (negate (hi rePart)) (negate (lo rePart)))
                   in ComplexInterval (Interval (negate (hi pos)) (negate (lo pos))) (I.fromRational' 0)
                else
                  if lo imPart >= 0 && hi imPart <= 0 && hi rePart <= 0 && n == 2
                    then -- sqrt(negative) = i*sqrt(|x|)
                      let pos = I.isqrt (Interval (negate (hi rePart)) (negate (lo rePart)))
                       in ComplexInterval (I.fromRational' 0) pos
                    else -- General complex root
                      I.cinthroot n ci
