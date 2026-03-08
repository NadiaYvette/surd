-- | Ordering of real radical expressions.
--
-- Uses numerical evaluation with sufficient precision to determine
-- the ordering between two expressions.
module Surd.Radical.Order
  ( radicalCompare
  , radicalLt
  , radicalGt
  , radicalLeq
  , radicalGeq
  ) where

import Surd.Types
import Surd.Radical.Eval (eval)
import Surd.Radical.Equality (radicalEq)

-- | Compare two radical expressions.
--
-- First checks equality (via minimal polynomials), then uses
-- numerical evaluation to determine ordering.
--
-- For rigorous comparison, we would refine interval arithmetic
-- until the intervals are disjoint. The current implementation
-- uses double-precision evaluation which is correct for
-- expressions whose values are separated by more than ~10^-15.
radicalCompare :: RadExpr Rational -> RadExpr Rational -> Ordering
radicalCompare a b
  | radicalEq a b = EQ
  | otherwise =
      let va = eval a :: Double
          vb = eval b :: Double
      in compare va vb

radicalLt :: RadExpr Rational -> RadExpr Rational -> Bool
radicalLt a b = radicalCompare a b == LT

radicalGt :: RadExpr Rational -> RadExpr Rational -> Bool
radicalGt a b = radicalCompare a b == GT

radicalLeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalLeq a b = radicalCompare a b /= GT

radicalGeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalGeq a b = radicalCompare a b /= LT
