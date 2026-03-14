definition module Normalize

// Normalization passes for radical expressions.
//
// These are explicit transformations the user applies as needed.
// They do NOT denest -- see Denest for radical denesting.
//
// The pipeline is:
//   normalize = fixN 10 (collectTerms . collectCoefficients . distribute .
//     sortCommutative . extractPerfectPowers . simplifyPowers .
//     foldConstants . flattenArith)

from RadExpr import :: RadExpr
from Rational import :: Rational

// Apply all normalization passes, iterated to a fixed point (max 10 times).
normalize :: !(RadExpr Rational) -> RadExpr Rational

// A single normalization pass (all sub-passes composed once).
normalizeOnce :: !(RadExpr Rational) -> RadExpr Rational

// Individual passes (exported for selective use):

// Flatten nested Add/Mul and cancel double negations/inversions.
flattenArith :: !(RadExpr k) -> RadExpr k

// Evaluate pure-literal subtrees to rational constants.
foldConstants :: !(RadExpr Rational) -> RadExpr Rational

// Simplify power expressions: (sqrt(a))^2 -> a, nested roots, etc.
simplifyPowers :: !(RadExpr Rational) -> RadExpr Rational

// Extract perfect nth powers from under radicals: sqrt(12) -> 2*sqrt(3).
extractPerfectPowers :: !(RadExpr Rational) -> RadExpr Rational

// Merge all literal factors in products into a single coefficient.
collectCoefficients :: !(RadExpr Rational) -> RadExpr Rational

// Group like terms in sums and combine their rational coefficients.
collectTerms :: !(RadExpr Rational) -> RadExpr Rational

// Sort children of commutative operators into canonical order.
sortCommutative :: !(RadExpr Rational) -> RadExpr Rational

// Distribute scalar multiplication over addition: c*(a+b) -> c*a + c*b.
distribute :: !(RadExpr Rational) -> RadExpr Rational
