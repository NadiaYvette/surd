module Surd.Types

import Surd.Rational

%default total

||| A radical expression over a coefficient field k.
|||
||| Normalization is explicit: these constructors are "dumb".
||| Call simplification functions separately.
public export
data RadExpr : Type -> Type where
  ||| Coefficient literal
  Lit  : k -> RadExpr k
  ||| Negation
  Neg  : RadExpr k -> RadExpr k
  ||| Sum
  Add  : RadExpr k -> RadExpr k -> RadExpr k
  ||| Product
  Mul  : RadExpr k -> RadExpr k -> RadExpr k
  ||| Multiplicative inverse
  Inv  : RadExpr k -> RadExpr k
  ||| The principal nth root of x, where n >= 2
  Root : (n : Int) -> RadExpr k -> RadExpr k
  ||| Integer power (may be negative)
  Pow  : RadExpr k -> (e : Int) -> RadExpr k

||| Subtraction: a - b = a + (-b)
export
sub : RadExpr k -> RadExpr k -> RadExpr k
sub a b = Add a (Neg b)

||| Division: a / b = a * (1/b)
export
rdiv : RadExpr k -> RadExpr k -> RadExpr k
rdiv a b = Mul a (Inv b)

||| Square root shorthand.
export
sqrt : RadExpr k -> RadExpr k
sqrt = Root 2

||| Lift a Rational into a radical expression.
export
ratE : Rational -> RadExpr Rational
ratE = Lit

||| Lift an integer into a radical expression over Rational.
export
intE : Integer -> RadExpr Rational
intE n = Lit (Rational.fromInteger n)

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

export
Eq k => Eq (RadExpr k) where
  (Lit a)    == (Lit b)    = a == b
  (Neg a)    == (Neg b)    = a == b
  (Add a b)  == (Add c d)  = a == c && b == d
  (Mul a b)  == (Mul c d)  = a == c && b == d
  (Inv a)    == (Inv b)    = a == b
  (Root n a) == (Root m b) = n == m && a == b
  (Pow a n)  == (Pow b m)  = n == m && a == b
  _ == _ = False

-- Tag for Ord comparison
tagOf : RadExpr k -> Nat
tagOf (Lit _)    = 0
tagOf (Neg _)    = 1
tagOf (Add _ _)  = 2
tagOf (Mul _ _)  = 3
tagOf (Inv _)    = 4
tagOf (Root _ _) = 5
tagOf (Pow _ _)  = 6

export
(Ord k) => Ord (RadExpr k) where
  compare (Lit a)    (Lit b)    = compare a b
  compare (Neg a)    (Neg b)    = compare a b
  compare (Add a b)  (Add c d)  = case compare a c of EQ => compare b d; r => r
  compare (Mul a b)  (Mul c d)  = case compare a c of EQ => compare b d; r => r
  compare (Inv a)    (Inv b)    = compare a b
  compare (Root n a) (Root m b) = case compare n m of EQ => compare a b; r => r
  compare (Pow a n)  (Pow b m)  = case compare a b of EQ => compare n m; r => r
  compare x y = compare (tagOf x) (tagOf y)

showPrec' : Show k => Prec -> RadExpr k -> String
showPrec' _ (Lit k) = show k
showPrec' d (Neg a) = showParens (d >= PrefixMinus) ("Neg " ++ showPrec' App a)
showPrec' d (Add a b) = showParens (d >= App) ("Add " ++ showPrec' App a ++ " " ++ showPrec' App b)
showPrec' d (Mul a b) = showParens (d >= App) ("Mul " ++ showPrec' App a ++ " " ++ showPrec' App b)
showPrec' d (Inv a) = showParens (d >= App) ("Inv " ++ showPrec' App a)
showPrec' d (Root n a) = showParens (d >= App) ("Root " ++ show n ++ " " ++ showPrec' App a)
showPrec' d (Pow a n) = showParens (d >= App) ("Pow " ++ showPrec' App a ++ " " ++ show n)

export
Show k => Show (RadExpr k) where
  show = showPrec' Open

||| Functor-like map over the coefficient type.
export
mapExpr : (a -> b) -> RadExpr a -> RadExpr b
mapExpr f (Lit k)    = Lit (f k)
mapExpr f (Neg a)    = Neg (mapExpr f a)
mapExpr f (Add a b)  = Add (mapExpr f a) (mapExpr f b)
mapExpr f (Mul a b)  = Mul (mapExpr f a) (mapExpr f b)
mapExpr f (Inv a)    = Inv (mapExpr f a)
mapExpr f (Root n a) = Root n (mapExpr f a)
mapExpr f (Pow a n)  = Pow (mapExpr f a) n

export
Functor RadExpr where
  map = mapExpr
