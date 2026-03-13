module Surd.Expr

import Surd.Types

import Data.List

%default covering

------------------------------------------------------------------------
-- Structural queries
------------------------------------------------------------------------

||| Nesting depth of the expression tree.
export
depth : RadExpr k -> Nat
depth (Lit _)    = 0
depth (Neg a)    = depth a
depth (Add a b)  = 1 + max (depth a) (depth b)
depth (Mul a b)  = 1 + max (depth a) (depth b)
depth (Inv a)    = 1 + depth a
depth (Root _ a) = 1 + depth a
depth (Pow a _)  = 1 + depth a

||| Number of nodes in the expression tree.
export
exprSize : RadExpr k -> Nat
exprSize (Lit _)    = 1
exprSize (Neg a)    = 1 + exprSize a
exprSize (Add a b)  = 1 + exprSize a + exprSize b
exprSize (Mul a b)  = 1 + exprSize a + exprSize b
exprSize (Inv a)    = 1 + exprSize a
exprSize (Root _ a) = 1 + exprSize a
exprSize (Pow a _)  = 1 + exprSize a

||| Check whether all leaves satisfy a predicate.
||| (Named `freeOf` in the Haskell version — checks if all coefficients
||| satisfy the predicate.)
export
freeOf : (k -> Bool) -> RadExpr k -> Bool
freeOf p (Lit k)    = p k
freeOf p (Neg a)    = freeOf p a
freeOf p (Add a b)  = freeOf p a && freeOf p b
freeOf p (Mul a b)  = freeOf p a && freeOf p b
freeOf p (Inv a)    = freeOf p a
freeOf p (Root _ a) = freeOf p a
freeOf p (Pow a _)  = freeOf p a

------------------------------------------------------------------------
-- Radical collection
------------------------------------------------------------------------

||| Collect distinct (rootIndex, radicand) pairs from an expression.
||| Uses nub to deduplicate while preserving order.
export
collectRadicals : Eq k => RadExpr k -> List (Int, RadExpr k)
collectRadicals = nub . go
  where
    go : RadExpr k -> List (Int, RadExpr k)
    go (Lit _)    = []
    go (Neg a)    = go a
    go (Add a b)  = go a ++ go b
    go (Mul a b)  = go a ++ go b
    go (Inv a)    = go a
    go (Pow a _)  = go a
    go (Root n a) = go a ++ [(n, a)]

------------------------------------------------------------------------
-- Topological sort of radicals
------------------------------------------------------------------------

||| Check whether all Root subexpressions in a radicand are present
||| in the resolved set.
export
allRootsResolved : Eq k => List (Int, RadExpr k) -> RadExpr k -> Bool
allRootsResolved _        (Lit _)    = True
allRootsResolved resolved (Neg a)    = allRootsResolved resolved a
allRootsResolved resolved (Add a b)  = allRootsResolved resolved a && allRootsResolved resolved b
allRootsResolved resolved (Mul a b)  = allRootsResolved resolved a && allRootsResolved resolved b
allRootsResolved resolved (Inv a)    = allRootsResolved resolved a
allRootsResolved resolved (Pow a _)  = allRootsResolved resolved a
allRootsResolved resolved (Root n a) = elem (n, a) resolved

||| Topologically sort radicals so that radicals with rational radicands
||| come first, followed by radicals whose radicands depend only on
||| earlier radicals. Unresolvable radicals (cyclic dependencies) are
||| appended at the end.
export
topoSortRadicals : Eq k => List (Int, RadExpr k) -> List (Int, RadExpr k)
topoSortRadicals = go []
  where
    go : List (Int, RadExpr k) -> List (Int, RadExpr k) -> List (Int, RadExpr k)
    go sorted [] = sorted
    go sorted remaining =
      let ready = filter (\r => allRootsResolved sorted (snd r)) remaining
          remaining' = filter (\r => not (elem r ready)) remaining
      in if isNil ready
           then sorted ++ remaining
           else go (sorted ++ ready) remaining'
