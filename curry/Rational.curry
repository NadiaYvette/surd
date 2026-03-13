--- Rational number arithmetic with GCD normalization.
--- Curry's Int is arbitrary precision, so no overflow concerns.
module Rational
  ( Rational(..)
  , mkRat, rat
  , numerator, denominator
  , fromInt
  , ratAdd, ratSub, ratMul, ratDiv, ratNeg, ratAbs, ratSignum
  , ratInv, ratPow
  , ratEq, ratLe, ratLt, ratGe, ratGt, ratCompare
  , ratMin, ratMax
  , ratFloor, ratCeiling
  , showRat
  ) where

--- A rational number p/q in lowest terms with q > 0.
data Rational = Rat Int Int

--- Smart constructor: normalizes sign and reduces by GCD.
mkRat :: Int -> Int -> Rational
mkRat p q
  | q == 0    = error "mkRat: zero denominator"
  | p == 0    = Rat 0 1
  | otherwise =
      let g   = gcdInt (absInt p) (absInt q)
          p'  = p `div` g
          q'  = q `div` g
      in if q' < 0
         then Rat (negate p') (negate q')
         else Rat p' q'

--- Convenient synonym for mkRat.
rat :: Int -> Int -> Rational
rat = mkRat

--- Extract numerator.
numerator :: Rational -> Int
numerator (Rat p _) = p

--- Extract denominator.
denominator :: Rational -> Int
denominator (Rat _ q) = q

--- Lift an integer to a rational.
fromInt :: Int -> Rational
fromInt n = Rat n 1

--- GCD of two non-negative integers.
gcdInt :: Int -> Int -> Int
gcdInt a b
  | b == 0    = a
  | otherwise = gcdInt b (a `mod` b)

--- Absolute value of an integer.
absInt :: Int -> Int
absInt n = if n < 0 then negate n else n

--- Addition.
ratAdd :: Rational -> Rational -> Rational
ratAdd (Rat a b) (Rat c d) = mkRat (a * d + c * b) (b * d)

--- Subtraction.
ratSub :: Rational -> Rational -> Rational
ratSub (Rat a b) (Rat c d) = mkRat (a * d - c * b) (b * d)

--- Multiplication.
ratMul :: Rational -> Rational -> Rational
ratMul (Rat a b) (Rat c d) = mkRat (a * c) (b * d)

--- Division.
ratDiv :: Rational -> Rational -> Rational
ratDiv (Rat a b) (Rat c d)
  | c == 0    = error "ratDiv: division by zero"
  | otherwise = mkRat (a * d) (b * c)

--- Negation.
ratNeg :: Rational -> Rational
ratNeg (Rat p q) = Rat (negate p) q

--- Absolute value.
ratAbs :: Rational -> Rational
ratAbs (Rat p q) = Rat (absInt p) q

--- Signum: -1, 0, or 1.
ratSignum :: Rational -> Rational
ratSignum (Rat p _)
  | p > 0     = Rat 1 1
  | p == 0    = Rat 0 1
  | otherwise = Rat (negate 1) 1

--- Multiplicative inverse.
ratInv :: Rational -> Rational
ratInv (Rat p q)
  | p == 0    = error "ratInv: zero"
  | otherwise = mkRat q p

--- Integer power (non-negative exponent only for now; negative uses ratInv).
ratPow :: Rational -> Int -> Rational
ratPow r n
  | n == 0    = Rat 1 1
  | n < 0     = ratPow (ratInv r) (negate n)
  | otherwise = ratMul r (ratPow r (n - 1))

--- Equality.
ratEq :: Rational -> Rational -> Bool
ratEq (Rat a b) (Rat c d) = a == c && b == d

--- Comparison.
ratCompare :: Rational -> Rational -> Ordering
ratCompare (Rat a b) (Rat c d) = compare (a * d) (c * b)

ratLe :: Rational -> Rational -> Bool
ratLe x y = ratCompare x y /= GT

ratLt :: Rational -> Rational -> Bool
ratLt x y = ratCompare x y == LT

ratGe :: Rational -> Rational -> Bool
ratGe x y = ratCompare x y /= LT

ratGt :: Rational -> Rational -> Bool
ratGt x y = ratCompare x y == GT

ratMin :: Rational -> Rational -> Rational
ratMin x y = if ratLe x y then x else y

ratMax :: Rational -> Rational -> Rational
ratMax x y = if ratGe x y then x else y

--- Floor: largest integer <= r.
ratFloor :: Rational -> Int
ratFloor (Rat p q) =
  let d = p `div` q
  in if p `mod` q < 0 then d - 1 else d

--- Ceiling: smallest integer >= r.
ratCeiling :: Rational -> Int
ratCeiling (Rat p q) =
  let d = p `div` q
      m = p `mod` q
  in if m > 0 then d + 1 else d

--- Show a rational number.
showRat :: Rational -> String
showRat (Rat p q) =
  if q == 1 then show p
  else show p ++ "/" ++ show q

instance Eq Rational where
  (==) = ratEq

instance Ord Rational where
  compare = ratCompare
  (<=)    = ratLe

instance Show Rational where
  show = showRat
