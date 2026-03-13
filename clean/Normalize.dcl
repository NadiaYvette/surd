definition module Normalize

// Normalization passes for radical expressions.
// These are explicit transformations the user applies as needed.
// They do NOT denest.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Apply all normalization passes, iterated to a fixed point (max 10 times).
normalize :: !(RadExpr Rational) -> RadExpr Rational

// A single normalization pass (all sub-passes composed once).
normalizeOnce :: !(RadExpr Rational) -> RadExpr Rational

// Individual passes:
flattenArith :: !(RadExpr k) -> RadExpr k
foldConstants :: !(RadExpr Rational) -> RadExpr Rational
simplifyPowers :: !(RadExpr Rational) -> RadExpr Rational
extractPerfectPowers :: !(RadExpr Rational) -> RadExpr Rational
collectCoefficients :: !(RadExpr Rational) -> RadExpr Rational
collectTerms :: !(RadExpr Rational) -> RadExpr Rational
sortCommutative :: !(RadExpr Rational) -> RadExpr Rational
distribute :: !(RadExpr Rational) -> RadExpr Rational
