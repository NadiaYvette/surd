-- | LaTeX rendering of radical expressions.
module Surd.Radical.LaTeX
  ( latex
  , latexPrec
  , latexDAG
  ) where

import Data.Ratio (numerator, denominator)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Surd.Radical.DAG (RadDAG(..), RadNodeOp(..), NodeId, toDAG, dagSize)
import Surd.Types

-- | Render a radical expression as a LaTeX math-mode string.
latex :: RadExpr Rational -> String
latex = latexPrec 0

precAdd, precMul, precNeg, precPow :: Int
precAdd = 1
precMul = 2
precNeg = 3
precPow = 4

latexPrec :: Int -> RadExpr Rational -> String
latexPrec _ (Lit r) = latexRat r
latexPrec p (Neg e) = case e of
  -- Neg of a sum: distribute the sign into terms
  Add _ _ ->
    parensIf (p > precAdd) $ renderTerms (map negTerm (flattenAdd e))
  -- Neg of a product with literal coefficient: absorb sign
  Mul (Lit c) rest ->
    latexPrec p (Mul (Lit (negate c)) rest)
  -- Neg of a literal: just negate
  Lit r ->
    latexPrec p (Lit (negate r))
  -- Otherwise: prefix minus
  _ ->
    parensIf (p > precNeg) $ "-" ++ latexPrec precNeg e
  where
    negTerm (s, t) = (not s, t)
latexPrec p e@(Add _ _) =
  parensIf (p > precAdd) $ renderTerms (flattenAdd e)
-- a / b
latexPrec p (Mul a (Inv b)) =
  parensIf (p > precMul) $ "\\frac{" ++ latexPrec 0 a ++ "}{" ++ latexPrec 0 b ++ "}"
-- (1/a) * b  →  b/a
latexPrec p (Mul (Inv a) b) =
  parensIf (p > precMul) $ "\\frac{" ++ latexPrec 0 b ++ "}{" ++ latexPrec 0 a ++ "}"
latexPrec p (Inv e) =
  parensIf (p > precMul) $ "\\frac{1}{" ++ latexPrec 0 e ++ "}"
latexPrec _ (Root 2 (Lit (-1))) = "\\mathrm{i}"
latexPrec _ (Root 2 e) = "\\sqrt{" ++ latexRadicand e ++ "}"
latexPrec _ (Root n e) = "\\sqrt[" ++ show n ++ "]{" ++ latexRadicand e ++ "}"
latexPrec _ (Pow _ 0) = "1"
latexPrec p (Pow e n)
  | n < 0     = latexPrec p (Inv (Pow e (negate n)))
  | n == 1    = latexPrec p e
  | otherwise = parensIf (p > precPow) $ latexBase e ++ "^{" ++ show n ++ "}"
latexPrec p e@(Mul _ _) =
  parensIf (p > precMul) $ renderFactors (flattenMul e)

-- | Render the base of a power expression.
-- Roots and other compound expressions need grouping for the exponent.
latexBase :: RadExpr Rational -> String
latexBase e@(Root _ _) = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Add _ _)  = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Mul _ _)  = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Neg _)    = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e@(Inv _)    = "\\left(" ++ latexPrec 0 e ++ "\\right)"
latexBase e            = latexPrec precPow e

-- | Render a radicand (inside \\sqrt{...}).
-- No outer parens needed since braces provide grouping.
latexRadicand :: RadExpr Rational -> String
latexRadicand (Lit r)
  | d == 1    = show n'  -- no parens needed inside braces, even for negatives
  | n' < 0   = "-\\frac{" ++ show (abs n') ++ "}{" ++ show d ++ "}"
  | otherwise = "\\frac{" ++ show n' ++ "}{" ++ show d ++ "}"
  where n' = numerator r; d = denominator r
latexRadicand e = latexPrec 0 e

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

parensIf :: Bool -> String -> String
parensIf True  s = "\\left(" ++ s ++ "\\right)"
parensIf False s = s

latexRat :: Rational -> String
latexRat r
  | d == 1    = show n'
  | n' < 0   = "-\\frac{" ++ show (abs n') ++ "}{" ++ show d ++ "}"
  | otherwise = "\\frac{" ++ show n' ++ "}{" ++ show d ++ "}"
  where
    n' = numerator r
    d  = denominator r

flattenAdd :: RadExpr Rational -> [(Bool, RadExpr Rational)]
flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
flattenAdd (Neg e)   = map (\(s, t) -> (not s, t)) (flattenAdd e)
flattenAdd (Lit r) | r < 0 = [(False, Lit (negate r))]
flattenAdd (Mul (Neg a) b) = [(False, Mul a b)]
flattenAdd e@(Mul _ _) = case flattenMul e of
  (Lit r : rest) | r < 0 -> [(False, rebuildMul (Lit (negate r) : rest))]
  _ -> [(True, e)]
flattenAdd e = [(True, e)]

rebuildMul :: [RadExpr Rational] -> RadExpr Rational
rebuildMul []     = Lit 1
rebuildMul [x]    = x
rebuildMul (x:xs) = foldl Mul x xs

renderTerms :: [(Bool, RadExpr Rational)] -> String
renderTerms [] = "0"
renderTerms ((s, t):rest) =
  let first = if s then latexPrec precAdd t else "-" ++ latexMulOrAtom t
  in first ++ concatMap rr rest
  where
    rr (True,  e) = " + " ++ latexPrec precAdd e
    rr (False, e) = " - " ++ latexMulOrAtom e
    -- Render at multiplication level without adding parens for products/atoms
    latexMulOrAtom = latexPrec precMul

flattenMul :: RadExpr Rational -> [RadExpr Rational]
flattenMul (Mul a b) = flattenMul a ++ flattenMul b
flattenMul e         = [e]

renderFactors :: [RadExpr Rational] -> String
renderFactors [] = "1"
renderFactors [x] = latexPrec precMul x
renderFactors (Lit c : rest)
  | c == 1    = joinMul (map (latexPrec precPow) rest)
  | c == -1   = "-" ++ joinMul (map (latexPrec precPow) rest)
  | otherwise = latexRat c ++ " \\cdot " ++ joinMul (map (latexPrec precPow) rest)
renderFactors fs = joinMul (map (latexPrec precPow) fs)

joinMul :: [String] -> String
joinMul []     = ""
joinMul [x]    = x
joinMul (x:xs) = x ++ concatMap (" \\cdot " ++) xs

-- --------------------------------------------------------------------------
-- DAG-based rendering
-- --------------------------------------------------------------------------

-- | Render a radical expression via its DAG, naming shared subexpressions.
-- For small expressions (DAG size ≤ threshold), falls back to tree rendering.
-- For large expressions, returns @(definitions, expression)@ where definitions
-- is a list of @(name, body)@ pairs for multiply-referenced subexpressions,
-- and expression is the root formula referencing those names.
latexDAG :: RadExpr Rational -> ([(String, String)], String)
latexDAG e =
  let dag = toDAG e
  in if dagSize dag <= 40
     then ([], latex e)
     else renderDAG dag

renderDAG :: RadDAG Rational -> ([(String, String)], String)
renderDAG dag =
  let nodes = IntMap.toAscList (dagNodes dag)
      -- Count references to identify which nodes need names
      refCounts = countRefs dag
      needsName nid = IntSet.member nid multiRef
      multiRef = IntSet.fromList
        [ nid | (nid, _) <- nodes
        , IntMap.findWithDefault 0 nid refCounts > 1
        , not (isLit (dagNodes dag IntMap.! nid))
        ]
      -- Build definitions for multiply-referenced non-trivial nodes
      defs = [ (nodeVar nid, renderOp dag needsName nid op)
             | (nid, op) <- nodes
             , needsName nid
             ]
      rootExpr = case dagNodes dag IntMap.! dagRoot dag of
        NLit r -> latexRat r
        op | needsName (dagRoot dag) -> nodeVar (dagRoot dag)
           | otherwise -> renderOp dag needsName (dagRoot dag) op
  in (defs, rootExpr)

-- | Render a single DAG node operation.
renderOp :: RadDAG Rational -> (NodeId -> Bool) -> NodeId -> RadNodeOp Rational -> String
renderOp dag needsName _ op =
  let ref nid = case dagNodes dag IntMap.! nid of
        NLit r -> latexRat r
        _ | needsName nid -> nodeVar nid
          | otherwise     -> renderOp dag needsName nid (dagNodes dag IntMap.! nid)
      -- Reference with parens for contexts needing an atom
      refAtom nid = case dagNodes dag IntMap.! nid of
        NLit r  -> latexRat r
        NRoot{} | needsName nid -> nodeVar nid
                | otherwise -> renderOp dag needsName nid (dagNodes dag IntMap.! nid)
        _ | needsName nid -> nodeVar nid
          | otherwise ->
              let s = renderOp dag needsName nid (dagNodes dag IntMap.! nid)
              in "\\left(" ++ s ++ "\\right)"
  in case op of
    NLit r     -> latexRat r
    NNeg a     -> "-" ++ refAtom a
    NAdd a b   ->
      let bStr = ref b
      in case bStr of
        ('-':rest) -> ref a ++ " - " ++ rest
        _          -> ref a ++ " + " ++ bStr
    NMul a b   -> refAtom a ++ " \\cdot " ++ refAtom b
    NInv a     -> "\\frac{1}{" ++ ref a ++ "}"
    NRoot 2 a  -> case dagNodes dag IntMap.! a of
                    NLit (-1) -> "\\mathrm{i}"
                    _         -> "\\sqrt{" ++ ref a ++ "}"
    NRoot n a  -> "\\sqrt[" ++ show n ++ "]{" ++ ref a ++ "}"
    NPow a n
      | n == 0    -> "1"
      | n < 0     -> "\\frac{1}{" ++ refAtom a ++ "^{" ++ show (negate n) ++ "}}"
      | otherwise -> refAtom a ++ "^{" ++ show n ++ "}"

-- | Generate a variable name for a DAG node: x_1, x_2, etc.
nodeVar :: NodeId -> String
nodeVar nid = "x_{" ++ show nid ++ "}"

-- | Count how many times each node is referenced by other nodes.
countRefs :: RadDAG Rational -> IntMap.IntMap Int
countRefs dag = IntMap.foldl' addRefs IntMap.empty (dagNodes dag)
  where
    addRefs m op = foldl (\m' nid -> IntMap.insertWith (+) nid 1 m') m (children op)
    children (NLit _)    = []
    children (NNeg a)    = [a]
    children (NAdd a b)  = [a, b]
    children (NMul a b)  = [a, b]
    children (NInv a)    = [a]
    children (NRoot _ a) = [a]
    children (NPow a _)  = [a]

isLit :: RadNodeOp k -> Bool
isLit (NLit _) = True
isLit _        = False
