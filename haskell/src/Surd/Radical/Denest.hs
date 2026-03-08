-- | Top-level radical denesting dispatcher.
--
-- Applies the appropriate denesting algorithm based on the
-- root index and expression structure.
module Surd.Radical.Denest
  ( denest
  , denestFull
  ) where

import Surd.Types
import Surd.Radical.Denest.Sqrt (denestSqrt)
import Surd.Radical.Denest.NthRoot (denestNthRoot)
import Surd.Radical.Normalize (normalize)

-- | Denest a radical expression: apply all available denesting
-- algorithms recursively.
denest :: RadExpr Rational -> RadExpr Rational
denest = denestFull . normalize

-- | Full denesting pass without pre-normalization.
denestFull :: RadExpr Rational -> RadExpr Rational
denestFull expr = case expr of
  Root 2 _ -> denestSqrt expr
  Root _ _ -> denestNthRoot expr
  Neg a    -> Neg (denestFull a)
  Add a b  -> Add (denestFull a) (denestFull b)
  Mul a b  -> Mul (denestFull a) (denestFull b)
  Inv a    -> Inv (denestFull a)
  Pow a n  -> Pow (denestFull a) n
  e        -> e
