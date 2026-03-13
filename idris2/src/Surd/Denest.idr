module Surd.Denest

import Surd.Rational
import Surd.Types
import Surd.Normalize
import Surd.Eval

%default covering

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

intSqrt : Integer -> Integer
intSqrt n = if n <= 0 then 0
            else assert_total $ go 1
  where
    go : Integer -> Integer
    go g = let g' = div (g + div n g) 2
           in if g' >= g then g else go g'

isSquare : Integer -> Bool
isSquare n = let s = intSqrt n in s * s == n

------------------------------------------------------------------------
-- Square root denesting (Borodin-Fagin-Hopcroft-Tompa)
------------------------------------------------------------------------

||| Try to denest sqrt(a + b*sqrt(c)) = sqrt(x) + sqrt(y)
||| where x + y = a and x*y = b^2*c/4.
|||
||| This is the Borodin-Fagin-Hopcroft-Tompa algorithm for depth-1
||| square root denesting.
denestSqrtSimple : RadExpr Rational -> Maybe (RadExpr Rational)
denestSqrtSimple (Root 2 inner) =
  -- Look for pattern: Lit a + Mul (Lit b) (Root 2 (Lit c))
  case inner of
    Add (Lit a) (Mul (Lit b) (Root 2 (Lit c))) =>
      -- sqrt(a + b*sqrt(c)) = sqrt(x) + sqrt(y)
      -- x + y = a, x*y = b^2*c/4
      let disc = a * a - b * b * c
      in if disc >= Rational.zero then
           -- Check if disc is a perfect square (rational)
           let dNum = numer disc
               dDen = denom disc
           in if isSquare (abs dNum) && isSquare dDen then
                let sqrtDisc = mkRat (intSqrt (abs dNum)) (intSqrt dDen)
                    x = (a + sqrtDisc) / Rational.fromInteger 2
                    y = (a - sqrtDisc) / Rational.fromInteger 2
                in if x >= Rational.zero && y >= Rational.zero then
                     let result = if b >= Rational.zero
                                    then Add (Root 2 (Lit x)) (Root 2 (Lit y))
                                    else Add (Root 2 (Lit x)) (Neg (Root 2 (Lit y)))
                     in Just result
                   else Nothing
              else Nothing
         else Nothing
    _ => Nothing
denestSqrtSimple _ = Nothing

------------------------------------------------------------------------
-- Nth root denesting
------------------------------------------------------------------------

||| Try to denest nth roots of rational numbers.
||| If the radicand is a perfect nth power, return the root.
denestNthRoot : RadExpr Rational -> Maybe (RadExpr Rational)
denestNthRoot (Root n (Lit r)) =
  if Surd.Rational.isZero r then Just (Lit Rational.zero)
  else if r == Rational.one then Just (Lit Rational.one)
  else Nothing  -- perfect power extraction is done in extractPerfectPowers
denestNthRoot _ = Nothing

------------------------------------------------------------------------
-- Top-level denesting dispatcher
------------------------------------------------------------------------

||| Full denesting pass without pre-normalization.
export
denestFull : RadExpr Rational -> RadExpr Rational
denestFull expr = case expr of
  Root 2 inner =>
    case denestSqrtSimple expr of
      Just result => denestFull result
      Nothing => Root 2 (denestFull inner)
  Root n inner =>
    case denestNthRoot expr of
      Just result => denestFull result
      Nothing => Root n (denestFull inner)
  Neg a => Neg (denestFull a)
  Add a b => Add (denestFull a) (denestFull b)
  Mul a b => Mul (denestFull a) (denestFull b)
  Inv a => Inv (denestFull a)
  Pow a n => Pow (denestFull a) n
  e => e

||| Denest a radical expression: apply all available denesting
||| algorithms recursively.
export
denest : RadExpr Rational -> RadExpr Rational
denest = denestFull . normalize
