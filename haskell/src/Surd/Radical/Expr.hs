-- |
-- Module      : Surd.Radical.Expr
-- Description : Smart constructors and structural queries for radical expressions
-- Stability   : experimental
--
-- Convenience operations for building and inspecting 'RadExpr' trees.
-- These functions do /not/ normalize -- they just construct the AST nodes.
-- Use "Surd.Radical.Normalize" for simplification.
module Surd.Radical.Expr
  ( -- * Construction helpers
    lit,
    neg,
    add,
    sub,
    mul,
    div',
    inv,
    root,
    sqrt',
    pow,
    fromInteger',
    fromRational',

    -- * Structural queries
    depth,
    size,
    freeOf,
    mapCoeffs,

    -- * Radical collection and dependency ordering
    collectRadicals,
    topoSortRadicals,
    allRootsResolved,
  )
where

import Data.List (nub)
import Surd.Types
import Prelude hiding (div, sqrt)

-- | Build a coefficient literal node.
lit :: k -> RadExpr k
lit = Lit

-- | Build a negation node.
neg :: RadExpr k -> RadExpr k
neg = Neg

-- | Build an addition node.
add :: RadExpr k -> RadExpr k -> RadExpr k
add = Add

-- | Build a subtraction node: @sub a b = Add a (Neg b)@.
sub :: RadExpr k -> RadExpr k -> RadExpr k
sub a b = Add a (Neg b)

-- | Build a multiplication node.
mul :: RadExpr k -> RadExpr k -> RadExpr k
mul = Mul

-- | Build a division node: @div' a b = Mul a (Inv b)@.
--
-- Named @div'@ to avoid clashing with 'Prelude.div'.
div' :: RadExpr k -> RadExpr k -> RadExpr k
div' a b = Mul a (Inv b)

-- | Build a multiplicative inverse node.
inv :: RadExpr k -> RadExpr k
inv = Inv

-- | Build an @n@th root node: @root n x@ represents @x^(1/n)@.
root :: Int -> RadExpr k -> RadExpr k
root = Root

-- | Build a square root node: @sqrt' x = Root 2 x@.
--
-- Named @sqrt'@ to avoid clashing with 'Prelude.sqrt'.
sqrt' :: RadExpr k -> RadExpr k
sqrt' = Root 2

-- | Build an integer power node: @pow e n@ represents @e^n@.
pow :: RadExpr k -> Int -> RadExpr k
pow = Pow

-- | Lift an 'Integer' into a @RadExpr Rational@.
fromInteger' :: Integer -> RadExpr Rational
fromInteger' = Lit . fromInteger

-- | Lift a 'Rational' into a @RadExpr Rational@.
fromRational' :: Rational -> RadExpr Rational
fromRational' = Lit

-- | Nesting depth of the expression tree.
--
-- Leaves ('Lit') have depth 0. Each unary or binary node adds 1.
depth :: RadExpr k -> Int
depth (Lit _) = 0
depth (Neg a) = depth a
depth (Add a b) = 1 + max (depth a) (depth b)
depth (Mul a b) = 1 + max (depth a) (depth b)
depth (Inv a) = 1 + depth a
depth (Root _ a) = 1 + depth a
depth (Pow a _) = 1 + depth a

-- | Number of nodes in the expression tree (including leaves).
size :: RadExpr k -> Int
size (Lit _) = 1
size (Neg a) = 1 + size a
size (Add a b) = 1 + size a + size b
size (Mul a b) = 1 + size a + size b
size (Inv a) = 1 + size a
size (Root _ a) = 1 + size a
size (Pow a _) = 1 + size a

-- | Test whether all coefficients in the expression satisfy a predicate.
--
-- This traverses the entire tree, returning 'True' only if the predicate
-- holds for every 'Lit' coefficient. Despite the name, it does not
-- specifically check for 'Root' nodes.
freeOf :: (k -> Bool) -> RadExpr k -> Bool
freeOf p (Lit k) = p k
freeOf p (Neg a) = freeOf p a
freeOf p (Add a b) = freeOf p a && freeOf p b
freeOf p (Mul a b) = freeOf p a && freeOf p b
freeOf p (Inv a) = freeOf p a
freeOf p (Root _ a) = freeOf p a
freeOf p (Pow a _) = freeOf p a

-- | Map a function over the coefficients. Same as 'fmap' but with
-- a more descriptive name for the radical expression domain.
mapCoeffs :: (a -> b) -> RadExpr a -> RadExpr b
mapCoeffs = fmap

-- | Collect distinct @(rootIndex, radicand)@ pairs from an expression.
--
-- Traverses the tree, collecting every 'Root' occurrence as a pair
-- @(n, radicand)@. Duplicates are removed (preserving first-occurrence
-- order via 'nub').
collectRadicals :: (Eq k) => RadExpr k -> [(Int, RadExpr k)]
collectRadicals = nub . go
  where
    go (Lit _) = []
    go (Neg a) = go a
    go (Add a b) = go a ++ go b
    go (Mul a b) = go a ++ go b
    go (Inv a) = go a
    go (Pow a _) = go a
    go (Root n a) = go a ++ [(n, a)]

-- | Topologically sort radicals so that radicals with rational radicands
-- come first, followed by radicals whose radicands depend only on
-- earlier radicals.
--
-- If cyclic dependencies exist (which should not happen for well-formed
-- radical expressions), unresolvable radicals are appended at the end.
topoSortRadicals :: (Eq k) => [(Int, RadExpr k)] -> [(Int, RadExpr k)]
topoSortRadicals = go []
  where
    go sorted [] = sorted
    go sorted remaining =
      let ready = [r | r <- remaining, allRootsResolved sorted (snd r)]
          remaining' = filter (`notElem` ready) remaining
       in if null ready
            then sorted ++ remaining -- can't resolve more; append as-is
            else go (sorted ++ ready) remaining'

-- | Check whether all 'Root' subexpressions in a radicand are present
-- in the resolved set. Used by 'topoSortRadicals' to determine which
-- radicals are ready to be processed next.
allRootsResolved :: (Eq k) => [(Int, RadExpr k)] -> RadExpr k -> Bool
allRootsResolved _ (Lit _) = True
allRootsResolved resolved (Neg a) = allRootsResolved resolved a
allRootsResolved resolved (Add a b) = allRootsResolved resolved a && allRootsResolved resolved b
allRootsResolved resolved (Mul a b) = allRootsResolved resolved a && allRootsResolved resolved b
allRootsResolved resolved (Inv a) = allRootsResolved resolved a
allRootsResolved resolved (Pow a _) = allRootsResolved resolved a
allRootsResolved resolved (Root n a) = (n, a) `elem` resolved
