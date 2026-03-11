-- | Square root denesting.
--
-- Implements the algorithm for denesting expressions of the form
-- @√(a + b√r)@ where @a, b, r ∈ Q@.
--
-- If denesting is possible, @√(a + b√r) = √x + √y@ (or @√x - √y@)
-- where @x + y = a@ and @x * y = b²r/4@.
--
-- Reference: Borodin, Fagin, Hopcroft, Tompa (1985)
module Surd.Radical.Denest.Sqrt
  ( denestSqrt,
    denestSqrtExpr,
    trySqrtDenest,
  )
where

import Data.Ratio (denominator, numerator)
import Math.NumberTheory.Roots (exactSquareRoot)
import Surd.Types

-- | Try to denest @√(a + b√r)@ into @√x ± √y@.
--
-- Returns @Just (sign, x, y)@ where the result is @√x + sign*√y@,
-- or @Nothing@ if denesting is not possible over Q.
trySqrtDenest ::
  -- | a
  Rational ->
  -- | b
  Rational ->
  -- | r (the radicand of the inner square root)
  Rational ->
  -- | @(sign, x, y)@ such that @√(a + b√r) = √x + sign*√y@
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
-- Returns @Just √q@ if @q@ is a perfect square, @Nothing@ otherwise.
isRationalSqrt :: Rational -> Maybe Rational
isRationalSqrt q
  | q < 0 = Nothing
  | q == 0 = Just 0
  | otherwise = do
      sn <- exactSquareRoot (numerator q)
      sd <- exactSquareRoot (denominator q)
      Just (fromInteger sn / fromInteger sd)

-- | Try to denest a radical expression that is a square root.
-- Looks for the pattern @Sqrt (Add (Lit a) (Mul (Lit b) (Sqrt (Lit r))))@
-- and attempts denesting.
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

-- | Match the pattern @a + b*√r@ in a radical expression.
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
