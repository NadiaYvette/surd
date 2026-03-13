module Surd.Positive

import Data.Nat

%default total

||| A strictly positive natural number (> 0).
export
data Positive : Type where
  MkPositive : (val : Nat) -> {auto 0 prf : IsSucc val} -> Positive

||| Smart constructor: returns Nothing for zero.
export
positive : Nat -> Maybe Positive
positive Z = Nothing
positive (S k) = Just (MkPositive (S k))

||| Extract the underlying Nat.
export
unPositive : Positive -> Nat
unPositive (MkPositive v) = v

||| Convert to Integer.
export
toInteger : Positive -> Integer
toInteger p = cast (unPositive p)

export
Eq Positive where
  a == b = unPositive a == unPositive b

export
Ord Positive where
  compare a b = compare (unPositive a) (unPositive b)

export
Show Positive where
  show p = show (unPositive p)

||| Addition of two positive numbers is always positive.
export
add : Positive -> Positive -> Positive
add (MkPositive (S a)) (MkPositive (S b)) = MkPositive (S (a + S b))

||| Multiplication of two positive numbers is always positive.
export
mul : Positive -> Positive -> Positive
mul (MkPositive (S a)) (MkPositive (S b)) =
  case (S a) * (S b) of
    Z => MkPositive 1  -- unreachable: (S a)*(S b) >= 1
    (S k) => MkPositive (S k)

||| The positive number 1.
export
one : Positive
one = MkPositive 1
