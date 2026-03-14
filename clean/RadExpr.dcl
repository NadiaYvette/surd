definition module RadExpr

// Core algebraic data type for radical expressions,
// parameterised by the coefficient type.
// Normalization is explicit: these constructors are "dumb".

import StdOverloaded
from Rational import :: Rational

:: RadExpr k
    = Lit !k
    | Neg !(RadExpr k)
    | Add !(RadExpr k) !(RadExpr k)
    | Mul !(RadExpr k) !(RadExpr k)
    | Inv !(RadExpr k)
    | Root !Int !(RadExpr k)
    | Pow !(RadExpr k) !Int

// Construction helpers
rSub :: !(RadExpr k) !(RadExpr k) -> RadExpr k
rDiv :: !(RadExpr k) !(RadExpr k) -> RadExpr k
rSqrt :: !(RadExpr k) -> RadExpr k
ratE :: !Rational -> RadExpr Rational
intE :: !Int -> RadExpr Rational

// Structural queries
depth :: !(RadExpr k) -> Int
exprSize :: !(RadExpr k) -> Int
mapCoeffs :: (a -> b) !(RadExpr a) -> RadExpr b

instance == (RadExpr k) | == k
instance < (RadExpr k) | < k & == k
instance toString (RadExpr k) | toString k

// FUTURE: With StdGeneric, the == and < instances could be derived:
//   import StdGeneric
//   derive gEq RadExpr
//   derive gLexOrd RadExpr
// The manual instances above are retained for now because:
// (a) the codebase does not yet import StdGeneric anywhere,
// (b) generic derive for parameterised types requires GenEq/GenLexOrd
//     instances on the type parameter, which may need library changes,
// (c) the manual instances have well-defined constructor ordering (conTag)
//     that generic derive may not replicate identically.
