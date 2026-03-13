definition module NormalForm

// Monomial normal form for radical expressions.
// Provides canonical representation enabling like-term collection,
// i² = -1 reduction, and (ⁿ√r)ⁿ = r reduction.

from RadExpr import :: RadExpr
from Rational import :: Rational

// Convert RadExpr to NormalForm and back
toNormExpr :: !(RadExpr Rational) -> RadExpr Rational
fromNormExpr :: !(RadExpr Rational) -> RadExpr Rational

// NF round-trip: toNormExpr then fromNormExpr
normalFormRoundTrip :: !(RadExpr Rational) -> RadExpr Rational
