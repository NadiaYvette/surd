||| Consolidated integer GCD and LCM, proved total via assert_smaller.
|||
||| Provides the foundation for Rational normalization and
||| polynomial GCD computations.
module Surd.GCD

%default total

||| GCD via Euclidean algorithm, proved total by assert_smaller on |b|.
||| Both inputs should be non-negative for canonical results;
||| the function takes abs internally.
export
gcdInteger : Integer -> Integer -> Integer
gcdInteger a 0 = abs a
gcdInteger a b = gcdInteger b (assert_smaller b (mod a b))

||| LCM of two integers.
export
lcmInteger : Integer -> Integer -> Integer
lcmInteger a b =
  if a == 0 || b == 0 then 0
  else div (abs (a * b)) (gcdInteger (abs a) (abs b))
