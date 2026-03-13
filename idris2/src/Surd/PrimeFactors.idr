module Surd.PrimeFactors

import Surd.Positive
import Data.List

%default covering

||| Trial-division primality test.
export
isPrime : Integer -> Bool
isPrime n =
  if n < 2 then False
  else if n < 4 then True
  else assert_total $ go 2
  where
    go : Integer -> Bool
    go p =
      if p * p > n then True
      else if mod n p == 0 then False
      else go (if p == 2 then 3 else p + 2)

||| Generate primes up to a limit via trial division.
primesUpTo : Integer -> List Integer
primesUpTo limit = assert_total $ go 2 []
  where
    go : Integer -> List Integer -> List Integer
    go n acc =
      if n > limit then reverse acc
      else if isPrime n then go (if n == 2 then 3 else n + 2) (n :: acc)
      else go (n + 2) acc

||| Factorise a positive integer into (prime, exponent) pairs.
|||
||| Example: factorise 360 = [(2, 3), (3, 2), (5, 1)]
export
factorise : Positive -> List (Integer, Nat)
factorise pos =
  let n = Positive.toInteger pos
  in if n == 1 then []
  else assert_total $ go n 2
  where
    extractPower : Integer -> Integer -> (Nat, Integer)
    extractPower m p = assert_total $ go' 0 m
      where
        go' : Nat -> Integer -> (Nat, Integer)
        go' e x =
          if mod x p == 0
            then go' (S e) (div x p)
            else (e, x)

    go : Integer -> Integer -> List (Integer, Nat)
    go 1 _ = []
    go m p =
      if p * p > m then [(m, 1)]
      else
        let (e, m') = extractPower m p
            rest = go m' (if p == 2 then 3 else p + 2)
        in if e > 0 then (p, e) :: rest else rest

||| Distinct prime factors of a positive integer, in ascending order.
export
primeFactors : Positive -> List Integer
primeFactors = map fst . factorise
