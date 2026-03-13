implementation module RadExpr

import StdEnv
import Rational

rSub :: !(RadExpr k) !(RadExpr k) -> RadExpr k
rSub a b = Add a (Neg b)

rDiv :: !(RadExpr k) !(RadExpr k) -> RadExpr k
rDiv a b = Mul a (Inv b)

rSqrt :: !(RadExpr k) -> RadExpr k
rSqrt x = Root 2 x

ratE :: !Rational -> RadExpr Rational
ratE r = Lit r

intE :: !Int -> RadExpr Rational
intE n = Lit (ratFromInt n)

depth :: !(RadExpr k) -> Int
depth (Lit _) = 0
depth (Neg a) = depth a
depth (Add a b) = 1 + max (depth a) (depth b)
depth (Mul a b) = 1 + max (depth a) (depth b)
depth (Inv a) = 1 + depth a
depth (Root _ a) = 1 + depth a
depth (Pow a _) = 1 + depth a

exprSize :: !(RadExpr k) -> Int
exprSize (Lit _) = 1
exprSize (Neg a) = 1 + exprSize a
exprSize (Add a b) = 1 + exprSize a + exprSize b
exprSize (Mul a b) = 1 + exprSize a + exprSize b
exprSize (Inv a) = 1 + exprSize a
exprSize (Root _ a) = 1 + exprSize a
exprSize (Pow a _) = 1 + exprSize a

mapCoeffs :: (a -> b) !(RadExpr a) -> RadExpr b
mapCoeffs f (Lit k) = Lit (f k)
mapCoeffs f (Neg a) = Neg (mapCoeffs f a)
mapCoeffs f (Add a b) = Add (mapCoeffs f a) (mapCoeffs f b)
mapCoeffs f (Mul a b) = Mul (mapCoeffs f a) (mapCoeffs f b)
mapCoeffs f (Inv a) = Inv (mapCoeffs f a)
mapCoeffs f (Root n a) = Root n (mapCoeffs f a)
mapCoeffs f (Pow a n) = Pow (mapCoeffs f a) n

// Constructor tag for ordering: Lit=0, Neg=1, Add=2, Mul=3, Inv=4, Root=5, Pow=6
conTag :: !(RadExpr k) -> Int
conTag (Lit _)    = 0
conTag (Neg _)    = 1
conTag (Add _ _)  = 2
conTag (Mul _ _)  = 3
conTag (Inv _)    = 4
conTag (Root _ _) = 5
conTag (Pow _ _)  = 6

instance < (RadExpr k) | < k & == k where
    (<) (Lit a) (Lit b) = a < b
    (<) (Neg a) (Neg b) = a < b
    (<) (Add a1 a2) (Add b1 b2)
        | a1 == b1 = a2 < b2
        = a1 < b1
    (<) (Mul a1 a2) (Mul b1 b2)
        | a1 == b1 = a2 < b2
        = a1 < b1
    (<) (Inv a) (Inv b) = a < b
    (<) (Root n1 a) (Root n2 b)
        | n1 == n2 = a < b
        = n1 < n2
    (<) (Pow a n1) (Pow b n2)
        | a == b = n1 < n2
        = a < b
    (<) x y = conTag x < conTag y

instance == (RadExpr k) | == k where
    (==) (Lit a) (Lit b) = a == b
    (==) (Neg a) (Neg b) = a == b
    (==) (Add a1 a2) (Add b1 b2) = a1 == b1 && a2 == b2
    (==) (Mul a1 a2) (Mul b1 b2) = a1 == b1 && a2 == b2
    (==) (Inv a) (Inv b) = a == b
    (==) (Root n1 a) (Root n2 b) = n1 == n2 && a == b
    (==) (Pow a n1) (Pow b n2) = a == b && n1 == n2
    (==) _ _ = False

instance toString (RadExpr k) | toString k where
    toString (Lit k) = toString k
    toString (Neg a) = "(- " +++ toString a +++ ")"
    toString (Add a b) = "(" +++ toString a +++ " + " +++ toString b +++ ")"
    toString (Mul a b) = "(" +++ toString a +++ " * " +++ toString b +++ ")"
    toString (Inv a) = "(1/" +++ toString a +++ ")"
    toString (Root n a) = "Root(" +++ toString n +++ ", " +++ toString a +++ ")"
    toString (Pow a n) = "(" +++ toString a +++ "^" +++ toString n +++ ")"
