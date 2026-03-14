-- |
-- Module      : Surd.Radical.Order
-- Description : Rigorous ordering of real radical expressions
-- Stability   : experimental
--
-- Compares real radical expressions by converting both sides to
-- algebraic numbers and using Sturm-based interval refinement for
-- a rigorous, exact result. No floating-point tolerance is involved.
--
-- All functions in this module assume the expressions represent
-- real algebraic numbers. For expressions with complex values,
-- the behaviour is undefined.
module Surd.Radical.Order
  ( -- * Comparison
    radicalCompare,

    -- * Convenience predicates
    radicalLt,
    radicalGt,
    radicalLeq,
    radicalGeq,
  )
where

import Surd.Algebraic.Convert (radExprToAlgNum)
import Surd.Algebraic.Number (algCompare)
import Surd.Types

-- | Compare two radical expressions, returning an 'Ordering'.
--
-- Converts both to algebraic numbers and compares using
-- Sturm-based interval refinement. This is rigorous and
-- correct for all real radical expressions.
--
-- >>> radicalCompare (Root 2 (Lit 2)) (Lit 1)
-- GT
radicalCompare :: RadExpr Rational -> RadExpr Rational -> Ordering
radicalCompare a b = algCompare (radExprToAlgNum a) (radExprToAlgNum b)

-- | Strict less-than comparison of two radical expressions.
radicalLt :: RadExpr Rational -> RadExpr Rational -> Bool
radicalLt a b = radicalCompare a b == LT

-- | Strict greater-than comparison of two radical expressions.
radicalGt :: RadExpr Rational -> RadExpr Rational -> Bool
radicalGt a b = radicalCompare a b == GT

-- | Less-than-or-equal comparison of two radical expressions.
radicalLeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalLeq a b = radicalCompare a b /= GT

-- | Greater-than-or-equal comparison of two radical expressions.
radicalGeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalGeq a b = radicalCompare a b /= LT
