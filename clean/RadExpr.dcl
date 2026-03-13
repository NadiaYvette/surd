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
