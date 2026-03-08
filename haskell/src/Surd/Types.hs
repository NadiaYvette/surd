{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Core AST for radical expressions, parameterised by the coefficient type.
module Surd.Types
  ( RadExpr(..)
  , data Sub
  , data Div
  , data Sqrt
  , ratE
  , intE
  ) where



-- | A radical expression over a coefficient field @k@.
--
-- Normalization is explicit: these constructors are "dumb".
-- Call functions in "Surd.Radical.Normalize" or "Surd.Radical.Denest"
-- to simplify.
data RadExpr k
  = Lit !k
    -- ^ Coefficient literal
  | Neg !(RadExpr k)
    -- ^ Negation
  | Add !(RadExpr k) !(RadExpr k)
    -- ^ Sum
  | Mul !(RadExpr k) !(RadExpr k)
    -- ^ Product
  | Inv !(RadExpr k)
    -- ^ Multiplicative inverse
  | Root !Int !(RadExpr k)
    -- ^ @Root n x@: the principal @n@th root of @x@, where @n >= 2@.
    --
    -- Convention: for real positive radicands, the positive real root.
    -- For negative radicands under odd @n@, the real root.
    -- Complex radicands use the principal branch.
  | Pow !(RadExpr k) !Int
    -- ^ Integer power (may be negative)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Subtraction as @Add a (Neg b)@.
pattern Sub :: RadExpr k -> RadExpr k -> RadExpr k
pattern Sub a b = Add a (Neg b)

-- | Division as @Mul a (Inv b)@.
pattern Div :: RadExpr k -> RadExpr k -> RadExpr k
pattern Div a b = Mul a (Inv b)

-- | Square root shorthand.
pattern Sqrt :: RadExpr k -> RadExpr k
pattern Sqrt x = Root 2 x

-- | Lift a rational number into a radical expression.
ratE :: Rational -> RadExpr Rational
ratE = Lit

-- | Lift an integer into a radical expression.
intE :: Integral a => a -> RadExpr Rational
intE = Lit . fromIntegral
