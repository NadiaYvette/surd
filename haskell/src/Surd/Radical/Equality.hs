-- |
-- Module      : Surd.Radical.Equality
-- Description : Rigorous equality testing for radical expressions
-- Stability   : experimental
--
-- Tests equality of radical expressions by converting both sides to
-- algebraic numbers (minimal polynomial + isolating interval) and
-- comparing via Sturm-based interval refinement.
--
-- This is __rigorous__: no floating-point tolerance is involved in the
-- final decision. Two expressions are equal if and only if they share
-- the same minimal polynomial and their isolating intervals overlap
-- (which is refined until the answer is definitive).
module Surd.Radical.Equality
  ( radicalEq,
    radicalNeq,
  )
where

import Surd.Algebraic.Convert (radExprToAlgNum)
import Surd.Algebraic.Number (algEq)
import Surd.Types

-- | Test equality of two radical expressions over Q.
--
-- Converts both to algebraic numbers (minimal polynomial + isolating
-- interval) and checks equality using Sturm-based interval refinement.
-- This is rigorous: no floating-point tolerance is involved in the
-- final decision.
--
-- >>> radicalEq (Root 2 (Lit 8)) (Mul (Lit 2) (Root 2 (Lit 2)))
-- True
radicalEq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalEq a b = algEq (radExprToAlgNum a) (radExprToAlgNum b)

-- | Test inequality of two radical expressions over Q.
--
-- @radicalNeq a b = not (radicalEq a b)@.
radicalNeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalNeq a b = not (radicalEq a b)
