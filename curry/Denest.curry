--- Radical denesting: simplifying nested radical expressions.
---
--- Implements sqrt denesting (Borodin et al.) and a dispatcher
--- for general denesting.
module Denest
  ( denest
  , denestSqrt
  , trySqrtDenest
  , isRationalSqrt
  ) where

import Rational
import RadExpr
import Normalize (normalize)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Try to denest sqrt(a + b*sqrt(r)) into sqrt(x) +/- sqrt(y).
---
--- Returns Just (sign, x, y) where result is sqrt(x) + sign*sqrt(y),
--- or Nothing if denesting is not possible over Q.
trySqrtDenest :: Rational -> Rational -> Rational -> Maybe (Int, Rational, Rational)
trySqrtDenest a b r =
  let disc = ratSub (ratMul a a) (ratMul (ratMul b b) r)
  in if ratLt disc rZero
     then Nothing
     else case isRationalSqrt disc of
            Nothing -> Nothing
            Just sd ->
              let x = ratDiv (ratAdd a sd) (Rational.fromInt 2)
                  y = ratDiv (ratSub a sd) (Rational.fromInt 2)
                  sign = if ratGt b rZero then 1 else negate 1
              in if ratGe x rZero && ratGe y rZero
                 then Just (sign, x, y)
                 else Nothing

--- Check if a rational number is a perfect square.
--- Returns Just sqrt(q) if q is a perfect square, Nothing otherwise.
isRationalSqrt :: Rational -> Maybe Rational
isRationalSqrt q
  | ratLt q rZero = Nothing
  | q == rZero    = Just rZero
  | otherwise =
      let n = numerator q
          d = denominator q
      in case (intExactSqrt n, intExactSqrt d) of
           (Just sn, Just sd) -> Just (mkRat sn sd)
           _                  -> Nothing

--- Integer exact square root: returns Just sqrt(n) if n is a perfect square.
intExactSqrt :: Int -> Maybe Int
intExactSqrt n
  | n < 0     = Nothing
  | n == 0    = Just 0
  | n == 1    = Just 1
  | otherwise =
      let s = isqrt n
      in if s * s == n then Just s else Nothing

--- Integer square root via Newton's method.
isqrt :: Int -> Int
isqrt n
  | n <= 0    = 0
  | otherwise = go n
  where
    go x =
      let x' = (x + n `div` x) `div` 2
      in if x' >= x then x else go x'

--- Try to denest a radical expression that is a square root.
--- Looks for the pattern sqrt(a + b*sqrt(r)).
denestSqrt :: RadExpr Rational -> Maybe (RadExpr Rational)
denestSqrt expr = case expr of
  Root 2 inner -> case matchSqrtNested inner of
    Just (a, b, r) -> case trySqrtDenest a b r of
      Just (sign, x, y) ->
        let sx = Root 2 (Lit x)
            sy = Root 2 (Lit y)
        in if sign > 0
           then Just (Add sx sy)
           else Just (Add sx (Neg sy))
      Nothing -> Nothing
    Nothing -> Nothing
  _ -> Nothing

--- Match the pattern a + b*sqrt(r) in a radical expression.
matchSqrtNested :: RadExpr Rational -> Maybe (Rational, Rational, Rational)
matchSqrtNested expr = case expr of
  Add (Lit a) (Mul (Lit b) (Root 2 (Lit r))) -> Just (a, b, r)
  Add (Mul (Lit b) (Root 2 (Lit r))) (Lit a) -> Just (a, b, r)
  Add (Lit a) (Root 2 (Lit r))               -> Just (a, rOne, r)
  Add (Root 2 (Lit r)) (Lit a)               -> Just (a, rOne, r)
  Add (Lit a) (Neg (Mul (Lit b) (Root 2 (Lit r)))) ->
    Just (a, ratNeg b, r)
  Add (Lit a) (Neg (Root 2 (Lit r))) ->
    Just (a, Rational.fromInt (negate 1), r)
  _ -> Nothing

--- General denesting dispatcher.
--- Tries various denesting strategies and returns the simplest result.
denest :: RadExpr Rational -> RadExpr Rational
denest expr = case expr of
  Root 2 _ ->
    case denestSqrt expr of
      Just e' -> denest (normalize e')
      Nothing -> descend expr
  Root n (Lit r) ->
    -- Try to simplify nth root of a rational
    normalize (Root n (Lit r))
  _ -> descend expr

--- Descend into subexpressions, trying to denest each one.
descend :: RadExpr Rational -> RadExpr Rational
descend expr = case expr of
  Lit r     -> Lit r
  Neg a     -> Neg (denest a)
  Add a b   -> Add (denest a) (denest b)
  Mul a b   -> Mul (denest a) (denest b)
  Inv a     -> Inv (denest a)
  Root n a  -> Root n (denest a)
  Pow a n   -> Pow (denest a) n
