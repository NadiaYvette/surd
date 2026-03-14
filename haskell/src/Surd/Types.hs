{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Surd.Types
-- Description : Core AST for radical expressions
-- Stability   : experimental
--
-- The 'RadExpr' type represents radical expressions parameterised by a
-- coefficient type @k@ (typically 'Rational'). It is a "dumb" AST:
-- constructors do not perform any simplification or normalization.
-- Use functions in "Surd.Radical.Normalize" or "Surd.Radical.Denest"
-- to simplify.
--
-- === Examples
--
-- @
-- -- The expression 3 + 2*sqrt(5):
-- Add (Lit 3) (Mul (Lit 2) (Root 2 (Lit 5)))
--
-- -- Using pattern synonyms:
-- Sub (Lit 7) (Sqrt (Lit 3))  -- 7 - sqrt(3)
-- Div (Lit 1) (Sqrt (Lit 2))  -- 1 / sqrt(2)
-- @
module Surd.Types
  ( RadExpr (..),
    pattern Sub,
    pattern Div,
    pattern Sqrt,
    ratE,
    intE,
  )
where

-- | A radical expression over a coefficient field @k@.
--
-- The seven constructors correspond to the basic algebraic operations
-- on radical expressions:
--
-- * 'Lit' -- coefficient literal from @k@
-- * 'Neg' -- additive negation
-- * 'Add' -- addition
-- * 'Mul' -- multiplication
-- * 'Inv' -- multiplicative inverse (reciprocal)
-- * 'Root' -- principal @n@th root
-- * 'Pow' -- integer power (positive or negative)
--
-- Normalization is explicit: these constructors are "dumb".
-- Call functions in "Surd.Radical.Normalize" or "Surd.Radical.Denest"
-- to simplify.
data RadExpr k
  = -- | Coefficient literal. Embeds a value from the coefficient field @k@
    -- into a radical expression.
    Lit !k
  | -- | Additive negation: @Neg e@ represents @-e@.
    Neg !(RadExpr k)
  | -- | Addition: @Add a b@ represents @a + b@.
    Add !(RadExpr k) !(RadExpr k)
  | -- | Multiplication: @Mul a b@ represents @a * b@.
    Mul !(RadExpr k) !(RadExpr k)
  | -- | Multiplicative inverse: @Inv e@ represents @1/e@.
    Inv !(RadExpr k)
  | -- | Principal @n@th root: @Root n x@ represents @x^(1/n)@ where @n >= 2@.
    --
    -- Convention: for real positive radicands, the positive real root.
    -- For negative radicands under odd @n@, the real root.
    -- Complex radicands use the principal branch (smallest positive argument).
    Root !Int !(RadExpr k)
  | -- | Integer power: @Pow e n@ represents @e^n@ (may be negative).
    Pow !(RadExpr k) !Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Subtraction as @Add a (Neg b)@.
--
-- This is a bidirectional pattern synonym: matching on @Sub a b@ will
-- match @Add a (Neg b)@, and constructing @Sub a b@ builds that form.
pattern Sub :: RadExpr k -> RadExpr k -> RadExpr k
pattern Sub a b = Add a (Neg b)

-- | Division as @Mul a (Inv b)@.
--
-- This is a bidirectional pattern synonym: matching on @Div a b@ will
-- match @Mul a (Inv b)@, and constructing @Div a b@ builds that form.
pattern Div :: RadExpr k -> RadExpr k -> RadExpr k
pattern Div a b = Mul a (Inv b)

-- | Square root shorthand: @Sqrt x = Root 2 x@.
pattern Sqrt :: RadExpr k -> RadExpr k
pattern Sqrt x = Root 2 x

-- | Lift a rational number into a radical expression.
--
-- > ratE (3 % 4) == Lit (3 % 4)
ratE :: Rational -> RadExpr Rational
ratE = Lit

-- | Lift an integer into a radical expression (via 'Rational').
--
-- > intE 5 == Lit (5 % 1)
intE :: (Integral a) => a -> RadExpr Rational
intE = Lit . fromIntegral
