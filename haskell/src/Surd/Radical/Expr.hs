-- | Convenience operations on radical expressions.
--
-- These do NOT normalise — they just build the AST.
-- Use "Surd.Radical.Normalize" for simplification.
module Surd.Radical.Expr
  ( -- * Construction helpers
    lit
  , neg
  , add
  , sub
  , mul
  , div'
  , inv
  , root
  , sqrt'
  , pow
  , fromInteger'
  , fromRational'
    -- * Structural queries
  , depth
  , size
  , freeOf
  , mapCoeffs
  ) where

import Surd.Types
import Prelude hiding (div, sqrt)

-- | Literal coefficient.
lit :: k -> RadExpr k
lit = Lit

neg :: RadExpr k -> RadExpr k
neg = Neg

add :: RadExpr k -> RadExpr k -> RadExpr k
add = Add

sub :: RadExpr k -> RadExpr k -> RadExpr k
sub a b = Add a (Neg b)

mul :: RadExpr k -> RadExpr k -> RadExpr k
mul = Mul

div' :: RadExpr k -> RadExpr k -> RadExpr k
div' a b = Mul a (Inv b)

inv :: RadExpr k -> RadExpr k
inv = Inv

root :: Int -> RadExpr k -> RadExpr k
root = Root

sqrt' :: RadExpr k -> RadExpr k
sqrt' = Root 2

pow :: RadExpr k -> Int -> RadExpr k
pow = Pow

fromInteger' :: Integer -> RadExpr Rational
fromInteger' = Lit . fromInteger

fromRational' :: Rational -> RadExpr Rational
fromRational' = Lit

-- | Nesting depth of the expression tree.
depth :: RadExpr k -> Int
depth (Lit _)    = 0
depth (Neg a)    = depth a
depth (Add a b)  = 1 + max (depth a) (depth b)
depth (Mul a b)  = 1 + max (depth a) (depth b)
depth (Inv a)    = 1 + depth a
depth (Root _ a) = 1 + depth a
depth (Pow a _)  = 1 + depth a

-- | Number of nodes in the expression tree.
size :: RadExpr k -> Int
size (Lit _)    = 1
size (Neg a)    = 1 + size a
size (Add a b)  = 1 + size a + size b
size (Mul a b)  = 1 + size a + size b
size (Inv a)    = 1 + size a
size (Root _ a) = 1 + size a
size (Pow a _)  = 1 + size a

-- | Check if the expression contains no occurrences of 'Root'.
freeOf :: (k -> Bool) -> RadExpr k -> Bool
freeOf p (Lit k)    = p k
freeOf p (Neg a)    = freeOf p a
freeOf p (Add a b)  = freeOf p a && freeOf p b
freeOf p (Mul a b)  = freeOf p a && freeOf p b
freeOf p (Inv a)    = freeOf p a
freeOf p (Root _ a) = freeOf p a
freeOf p (Pow a _)  = freeOf p a

-- | Map a function over the coefficients (same as 'fmap' but with
-- a more descriptive name for the domain).
mapCoeffs :: (a -> b) -> RadExpr a -> RadExpr b
mapCoeffs = fmap
