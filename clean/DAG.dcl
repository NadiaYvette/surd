definition module DAG

// Explicit DAG representation for radical expressions.
// Unlike Haskell, Clean doesn't have thunk sharing via StableName,
// so we use structural hashing to detect shared subexpressions.
//
// PERFORMANCE NOTE: The buildDAG function threads a Map through sequential
// calls for CSE deduplication. With a unique-capable map (e.g. a custom
// hash-array implementation marked *Map), the put operations could be
// destructive updates. Data.Map is a persistent tree and does not support
// uniqueness, so this optimisation requires either:
//   (a) a *{!entry} array-based hash map with uniqueness, or
//   (b) Data.Map gaining uniqueness support upstream.
// The threading pattern in buildDAG already uses single-use discipline
// (each cache` is used exactly once), so switching to a unique map would
// require only type annotation changes, not restructuring.

from RadExpr import :: RadExpr
from Rational import :: Rational

:: NodeId :== Int

:: RadNodeOp
    = NLit !Rational
    | NNeg !NodeId
    | NAdd !NodeId !NodeId
    | NMul !NodeId !NodeId
    | NInv !NodeId
    | NRoot !Int !NodeId
    | NPow !NodeId !Int

:: RadDAG = { dagNodes :: ![(NodeId, RadNodeOp)], dagRootId :: !NodeId }

// Convert between RadExpr and DAG (uses structural equality for CSE)
toDAG :: !(RadExpr Rational) -> RadDAG
fromDAG :: !RadDAG -> RadExpr Rational

// DAG metrics
dagSize :: !RadDAG -> Int
dagDepth :: !RadDAG -> Int

// DAG-based constant folding (O(n) in unique nodes)
dagFoldConstants :: !RadDAG -> RadDAG

// DAG-based complex evaluation
dagEvalComplex :: !RadDAG -> (Real, Real)
