--- Structural queries on radical expressions.
---
--- These do NOT normalize — they just inspect the AST.
module Expr
  ( exprDepth
  , exprSize
  , freeOf
  , collectRadicals
  , topoSortRadicals
  , allRootsResolved
  ) where

import Data.List (nub)
import RadExpr

--- Nesting depth of the expression tree.
exprDepth :: RadExpr k -> Int
exprDepth expr = case expr of
  Lit _     -> 0
  Neg a     -> exprDepth a
  Add a b   -> 1 + max (exprDepth a) (exprDepth b)
  Mul a b   -> 1 + max (exprDepth a) (exprDepth b)
  Inv a     -> 1 + exprDepth a
  Root _ a  -> 1 + exprDepth a
  Pow a _   -> 1 + exprDepth a

--- Number of nodes in the expression tree.
exprSize :: RadExpr k -> Int
exprSize expr = case expr of
  Lit _     -> 1
  Neg a     -> 1 + exprSize a
  Add a b   -> 1 + exprSize a + exprSize b
  Mul a b   -> 1 + exprSize a + exprSize b
  Inv a     -> 1 + exprSize a
  Root _ a  -> 1 + exprSize a
  Pow a _   -> 1 + exprSize a

--- Check if a predicate holds for every coefficient in the expression.
freeOf :: (k -> Bool) -> RadExpr k -> Bool
freeOf p expr = case expr of
  Lit k     -> p k
  Neg a     -> freeOf p a
  Add a b   -> freeOf p a && freeOf p b
  Mul a b   -> freeOf p a && freeOf p b
  Inv a     -> freeOf p a
  Root _ a  -> freeOf p a
  Pow a _   -> freeOf p a

--- Collect distinct (rootIndex, radicand) pairs from an expression.
--- Uses nub for deduplication (preserves order).
collectRadicals :: Eq k => RadExpr k -> [(Int, RadExpr k)]
collectRadicals expr = nub (goCollect expr)

goCollect :: RadExpr k -> [(Int, RadExpr k)]
goCollect expr = case expr of
  Lit _     -> []
  Neg a     -> goCollect a
  Add a b   -> goCollect a ++ goCollect b
  Mul a b   -> goCollect a ++ goCollect b
  Inv a     -> goCollect a
  Pow a _   -> goCollect a
  Root n a  -> goCollect a ++ [(n, a)]

--- Topologically sort radicals so that radicals with rational radicands
--- come first, followed by radicals whose radicands depend only on
--- earlier radicals. Unresolvable radicals are appended at the end.
topoSortRadicals :: Eq k => [(Int, RadExpr k)] -> [(Int, RadExpr k)]
topoSortRadicals rads = goTopo [] rads

goTopo :: Eq k => [(Int, RadExpr k)] -> [(Int, RadExpr k)]
       -> [(Int, RadExpr k)]
goTopo sorted remaining
  | null remaining = sorted
  | otherwise =
      let ready = filter (\r -> allRootsResolved sorted (snd r)) remaining
          remaining' = filter (\r -> not (elem r ready)) remaining
      in if null ready
         then sorted ++ remaining
         else goTopo (sorted ++ ready) remaining'

--- Check whether all Root subexpressions in a radicand are present
--- in the resolved set.
allRootsResolved :: Eq k => [(Int, RadExpr k)] -> RadExpr k -> Bool
allRootsResolved resolved expr = case expr of
  Lit _     -> True
  Neg a     -> allRootsResolved resolved a
  Add a b   -> allRootsResolved resolved a && allRootsResolved resolved b
  Mul a b   -> allRootsResolved resolved a && allRootsResolved resolved b
  Inv a     -> allRootsResolved resolved a
  Pow a _   -> allRootsResolved resolved a
  Root n a  -> elem (n, a) resolved
