-- | Equality testing for radical expressions.
--
-- Two radical expressions are equal iff they represent the same
-- algebraic number: same minimal polynomial and same root
-- (verified by isolating interval overlap).
module Surd.Radical.Equality
  ( radicalEq
  , radicalNeq
  ) where

import Surd.Types
import Surd.Algebraic.Convert (radExprToAlgNum)
import Surd.Algebraic.Number (algEq)

-- | Test equality of two radical expressions over Q.
--
-- Converts both to algebraic numbers (minimal polynomial + isolating
-- interval) and checks equality using Sturm-based interval refinement.
-- This is rigorous: no floating-point tolerance is involved in the
-- final decision.
radicalEq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalEq a b = algEq (radExprToAlgNum a) (radExprToAlgNum b)

-- | Inequality test.
radicalNeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalNeq a b = not (radicalEq a b)
