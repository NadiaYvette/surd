definition module Expr

// Convenience constructors and structural queries for radical expressions.
//
// These functions operate on the RadExpr AST without normalising.
// Use them to inspect expression structure, collect radicals for
// dependency analysis, or check invariants on coefficients.

import StdOverloaded
from RadExpr import :: RadExpr
from Rational import :: Rational

// Test whether ALL coefficients in the expression satisfy predicate p.
// Returns True if the expression is "free of" any coefficient violating p.
freeOf :: (k -> Bool) !(RadExpr k) -> Bool

// Collect distinct (rootIndex, radicand) pairs from an expression.
// Uses structural equality on radicands to deduplicate, preserving
// order of first occurrence. Useful for dependency analysis and
// radical counting.
collectRadicals :: !(RadExpr k) -> [(Int, RadExpr k)] | == k

// Topologically sort a list of radicals so that radicals with purely
// rational radicands come first, followed by radicals whose radicands
// depend only on earlier (already-resolved) radicals. Unresolvable
// radicals are appended at the end.
topoSortRadicals :: [(Int, RadExpr k)] -> [(Int, RadExpr k)] | == k

// Check whether all Root subexpressions appearing in a radicand are
// present in the resolved set. Used by topoSortRadicals to determine
// when a radical's dependencies have been satisfied.
allRootsResolved :: [(Int, RadExpr k)] !(RadExpr k) -> Bool | == k
