-- | Ordering of real radical expressions.
--
-- Uses algebraic number comparison with Sturm-based interval
-- refinement for rigorous results.
module Surd.Radical.Order
  ( radicalCompare
  , radicalLt
  , radicalGt
  , radicalLeq
  , radicalGeq
  ) where

import Surd.Types
import Surd.Algebraic.Convert (radExprToAlgNum)
import Surd.Algebraic.Number (algCompare)

-- | Compare two radical expressions.
--
-- Converts both to algebraic numbers and compares using
-- Sturm-based interval refinement. This is rigorous and
-- correct for all real radical expressions.
radicalCompare :: RadExpr Rational -> RadExpr Rational -> Ordering
radicalCompare a b = algCompare (radExprToAlgNum a) (radExprToAlgNum b)

radicalLt :: RadExpr Rational -> RadExpr Rational -> Bool
radicalLt a b = radicalCompare a b == LT

radicalGt :: RadExpr Rational -> RadExpr Rational -> Bool
radicalGt a b = radicalCompare a b == GT

radicalLeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalLeq a b = radicalCompare a b /= GT

radicalGeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalGeq a b = radicalCompare a b /= LT
