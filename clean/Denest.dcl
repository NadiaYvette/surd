definition module Denest

// Radical denesting dispatcher.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Try to denest a radical expression.
// Returns a simpler expression if denesting succeeds, otherwise the original.
denest :: !(RadExpr Rational) -> RadExpr Rational

// Sqrt denesting (Borodin et al.)
denestSqrt :: !(RadExpr Rational) -> RadExpr Rational

// Cube root denesting
denestCubeRoot :: !(RadExpr Rational) -> RadExpr Rational
