-- |
-- Module      : Surd.Radical.Denest.Sqrt
-- Description : Square root denesting via Borodin's algorithm
-- Stability   : experimental
--
-- Implements denesting of expressions of the form @sqrt(a + b*sqrt r)@
-- where @a, b, r@ are rational. The algorithm determines whether the
-- expression can be written as @sqrt x +/- sqrt y@ for rational @x, y@.
--
-- === Mathematical criterion
--
-- Denesting succeeds when the discriminant @a^2 - b^2*r@ is a perfect
-- square of a rational number. In that case:
--
-- @
-- sqrt(a + b*sqrt r) = sqrt x + sign(b) * sqrt y
-- @
--
-- where @x = (a + d) / 2@, @y = (a - d) / 2@, and @d = sqrt(a^2 - b^2*r)@.
--
-- Reference: Borodin, Fagin, Hopcroft, Tompa (1985)
module Surd.Radical.Denest.Sqrt
  ( -- * Top-level denesting
    denestSqrt,

    -- * Pattern matching and core algorithm
    denestSqrtExpr,
    trySqrtDenest,
  )
where

import Data.Ratio (denominator, numerator)
import Math.NumberTheory.Roots (exactSquareRoot)
import Surd.Types

-- | Try to denest @sqrt(a + b*sqrt r)@ into @sqrt x +/- sqrt y@.
--
-- Returns @Just (sign, x, y)@ where the result is @sqrt x + sign*sqrt y@,
-- or @Nothing@ if denesting is not possible over Q.
--
-- Denesting requires @a^2 - b^2*r@ to be a perfect rational square,
-- and the resulting @x, y@ to both be non-negative.
trySqrtDenest ::
  -- | @a@ (rational part)
  Rational ->
  -- | @b@ (coefficient of the inner square root)
  Rational ->
  -- | @r@ (radicand of the inner square root)
  Rational ->
  -- | @(sign, x, y)@ such that @sqrt(a + b*sqrt r) = sqrt x + sign*sqrt y@
  Maybe (Int, Rational, Rational)
trySqrtDenest a b r
  | disc < 0 = Nothing
  | otherwise =
      let s = isRationalSqrt disc
       in case s of
            Nothing -> Nothing
            Just sd ->
              let x = (a + sd) / 2
                  y = (a - sd) / 2
                  sign = if b > 0 then 1 else -1
               in if x >= 0 && y >= 0
                    then Just (sign, x, y)
                    else Nothing
  where
    disc = a * a - b * b * r

-- | Check if a rational number is a perfect square.
-- Returns @Just (sqrt q)@ if @q@ is a perfect square, @Nothing@ otherwise.
isRationalSqrt :: Rational -> Maybe Rational
isRationalSqrt q
  | q < 0 = Nothing
  | q == 0 = Just 0
  | otherwise = do
      sn <- exactSquareRoot (numerator q)
      sd <- exactSquareRoot (denominator q)
      Just (fromInteger sn / fromInteger sd)

-- | Try to denest a radical expression that is a square root.
--
-- Looks for the pattern @Root 2 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit r))))@
-- and attempts Borodin denesting. Returns @Nothing@ if the expression
-- does not match the expected pattern or denesting fails.
denestSqrtExpr :: RadExpr Rational -> Maybe (RadExpr Rational)
denestSqrtExpr (Root 2 inner) = case matchSqrtNested inner of
  Just (a, b, r) -> do
    (sign, x, y) <- trySqrtDenest a b r
    let sx = Root 2 (Lit x)
        sy = Root 2 (Lit y)
    Just $
      if sign > 0
        then Add sx sy
        else Add sx (Neg sy)
  Nothing -> Nothing
denestSqrtExpr _ = Nothing

-- | Match the pattern @a + b*sqrt r@ in a radical expression.
-- Handles several syntactic variants (commutativity, implicit coefficient 1,
-- negation).
matchSqrtNested :: RadExpr Rational -> Maybe (Rational, Rational, Rational)
matchSqrtNested (Add (Lit a) (Mul (Lit b) (Root 2 (Lit r)))) =
  Just (a, b, r)
matchSqrtNested (Add (Mul (Lit b) (Root 2 (Lit r))) (Lit a)) =
  Just (a, b, r)
matchSqrtNested (Add (Lit a) (Root 2 (Lit r))) =
  Just (a, 1, r)
matchSqrtNested (Add (Root 2 (Lit r)) (Lit a)) =
  Just (a, 1, r)
matchSqrtNested (Add (Lit a) (Neg (Mul (Lit b) (Root 2 (Lit r))))) =
  Just (a, negate b, r)
matchSqrtNested (Add (Lit a) (Neg (Root 2 (Lit r)))) =
  Just (a, -1, r)
matchSqrtNested _ = Nothing

-- | Recursively try to denest all square roots in an expression.
--
-- Traverses bottom-up: when a square root is successfully denested,
-- the result is recursively denested again (in case the output
-- contains further denestable square roots).
denestSqrt :: RadExpr Rational -> RadExpr Rational
denestSqrt expr = case expr of
  Root 2 _ -> case denestSqrtExpr expr of
    Just denested -> denestSqrt denested -- try again on the result
    Nothing -> case expr of
      Root 2 a -> Root 2 (denestSqrt a)
  Neg a -> Neg (denestSqrt a)
  Add a b -> Add (denestSqrt a) (denestSqrt b)
  Mul a b -> Mul (denestSqrt a) (denestSqrt b)
  Inv a -> Inv (denestSqrt a)
  Root n a -> Root n (denestSqrt a)
  Pow a n -> Pow (denestSqrt a) n
  e -> e
