--- Core AST for radical expressions, parameterised by the coefficient type.
---
--- Normalization is explicit: these constructors are "dumb".
--- Call normalization/simplification functions separately.
module RadExpr
  ( RadExpr(..)
  , subE, divE, sqrtE
  , ratE, intE
  , showRadExpr
  ) where

import Rational

--- A radical expression over a coefficient field k.
data RadExpr k
  = Lit k                            -- Coefficient literal
  | Neg (RadExpr k)                  -- Negation
  | Add (RadExpr k) (RadExpr k)      -- Sum
  | Mul (RadExpr k) (RadExpr k)      -- Product
  | Inv (RadExpr k)                  -- Multiplicative inverse
  | Root Int (RadExpr k)             -- Principal nth root (n >= 2)
  | Pow (RadExpr k) Int              -- Integer power

--- Subtraction: a - b = a + (-b).
subE :: RadExpr k -> RadExpr k -> RadExpr k
subE a b = Add a (Neg b)

--- Division: a / b = a * (1/b).
divE :: RadExpr k -> RadExpr k -> RadExpr k
divE a b = Mul a (Inv b)

--- Square root shorthand.
sqrtE :: RadExpr k -> RadExpr k
sqrtE = Root 2

--- Lift a Rational into a radical expression.
ratE :: Rational -> RadExpr Rational
ratE = Lit

--- Lift an integer into a radical expression over Rational.
intE :: Int -> RadExpr Rational
intE n = Lit (Rational.fromInt n)

--- Show a radical expression (assuming the coefficient type is showable).
showRadExpr :: (k -> String) -> RadExpr k -> String
showRadExpr s expr = case expr of
  Lit k      -> s k
  Neg e      -> "(-" ++ showRadExpr s e ++ ")"
  Add a b    -> "(" ++ showRadExpr s a ++ " + " ++ showRadExpr s b ++ ")"
  Mul a b    -> "(" ++ showRadExpr s a ++ " * " ++ showRadExpr s b ++ ")"
  Inv e      -> "(1/" ++ showRadExpr s e ++ ")"
  Root n e   -> "root(" ++ show n ++ ", " ++ showRadExpr s e ++ ")"
  Pow e n    -> "(" ++ showRadExpr s e ++ ")^" ++ show n

--- Tag number for constructor ordering (matches Haskell's derived Ord).
radTag :: RadExpr k -> Int
radTag (Lit _)    = 0
radTag (Neg _)    = 1
radTag (Add _ _)  = 2
radTag (Mul _ _)  = 3
radTag (Inv _)    = 4
radTag (Root _ _) = 5
radTag (Pow _ _)  = 6

--- Eq instance (structural equality, no overlapping rules).
instance Eq k => Eq (RadExpr k) where
  x == y = case (x, y) of
    (Lit a,      Lit b)      -> a == b
    (Neg a,      Neg b)      -> a == b
    (Add a1 a2,  Add b1 b2)  -> a1 == b1 && a2 == b2
    (Mul a1 a2,  Mul b1 b2)  -> a1 == b1 && a2 == b2
    (Inv a,      Inv b)      -> a == b
    (Root n1 a,  Root n2 b)  -> n1 == n2 && a == b
    (Pow a n1,   Pow b n2)   -> a == b && n1 == n2
    _                        -> False

--- Ord instance (structural ordering, matching Haskell's derived Ord).
instance Ord k => Ord (RadExpr k) where
  compare x y =
    let tx = radTag x
        ty = radTag y
    in if tx /= ty then compare tx ty
       else case (x, y) of
              (Lit a,      Lit b)      -> compare a b
              (Neg a,      Neg b)      -> compare a b
              (Add a1 a2,  Add b1 b2)  -> case compare a1 b1 of
                                             EQ -> compare a2 b2
                                             o  -> o
              (Mul a1 a2,  Mul b1 b2)  -> case compare a1 b1 of
                                             EQ -> compare a2 b2
                                             o  -> o
              (Inv a,      Inv b)      -> compare a b
              (Root n1 a,  Root n2 b)  -> case compare n1 n2 of
                                             EQ -> compare a b
                                             o  -> o
              (Pow a n1,   Pow b n2)   -> case compare a b of
                                             EQ -> compare n1 n2
                                             o  -> o
              _                        -> EQ  -- unreachable

instance Show k => Show (RadExpr k) where
  show expr = case expr of
    Lit k      -> "Lit " ++ show k
    Neg e      -> "Neg (" ++ show e ++ ")"
    Add a b    -> "Add (" ++ show a ++ ") (" ++ show b ++ ")"
    Mul a b    -> "Mul (" ++ show a ++ ") (" ++ show b ++ ")"
    Inv e      -> "Inv (" ++ show e ++ ")"
    Root n e   -> "Root " ++ show n ++ " (" ++ show e ++ ")"
    Pow e n    -> "Pow (" ++ show e ++ ") " ++ show n
