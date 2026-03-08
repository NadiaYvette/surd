-- | Equality testing for radical expressions.
--
-- Two radical expressions are equal iff they share the same minimal
-- polynomial AND their numerical evaluations agree to sufficient precision.
module Surd.Radical.Equality
  ( radicalEq
  , radicalNeq
  ) where

import Surd.Types
import Surd.Polynomial.Univariate (degree)
import Surd.Polynomial.MinimalPoly (minimalPoly)
import Surd.Radical.Eval (eval)

-- | Test equality of two radical expressions over Q.
--
-- Algorithm:
-- 1. Compute minimal polynomials of both expressions.
-- 2. If they differ, the expressions are definitely not equal.
-- 3. If they match, compare numerical evaluations to sufficient precision
--    to distinguish roots of the minimal polynomial.
radicalEq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalEq a b =
  let pa = minimalPoly a
      pb = minimalPoly b
  in if pa /= pb
     then False
     else
       -- Same minimal polynomial. Check if they're the same root
       -- by evaluating the difference and checking if it's near zero.
       -- A rigorous approach would use interval arithmetic to verify;
       -- for now, use high-precision float comparison.
       let va = eval a :: Double
           vb = eval b :: Double
           diff = abs (va - vb)
           -- If the minimal poly has degree 1, there's only one root
       in if degree pa == 1
          then True
          else diff < 1e-10 * max 1 (max (abs va) (abs vb))

-- | Inequality test.
radicalNeq :: RadExpr Rational -> RadExpr Rational -> Bool
radicalNeq a b = not (radicalEq a b)
