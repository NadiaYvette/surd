definition module DAG

// Explicit DAG representation for radical expressions.
// Unlike Haskell, Clean doesn't have thunk sharing via StableName,
// so we use structural hashing to detect shared subexpressions.

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
