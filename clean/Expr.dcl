definition module Expr

// Convenience constructors and structural queries for radical expressions.
// These do NOT normalise — they just build the AST.

import StdOverloaded
from RadExpr import :: RadExpr
from Rational import :: Rational

// Predicate-based structural query: test whether all coefficients satisfy p.
freeOf :: (k -> Bool) !(RadExpr k) -> Bool

// Collect distinct (rootIndex, radicand) pairs from an expression.
collectRadicals :: !(RadExpr k) -> [(Int, RadExpr k)] | == k

// Topologically sort radicals: rational radicands first, then those depending
// only on already-resolved radicals.
topoSortRadicals :: [(Int, RadExpr k)] -> [(Int, RadExpr k)] | == k

// Check whether all Root subexpressions in a radicand are present in the
// resolved set.
allRootsResolved :: [(Int, RadExpr k)] !(RadExpr k) -> Bool | == k
