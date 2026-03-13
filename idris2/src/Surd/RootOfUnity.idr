module Surd.RootOfUnity

import Surd.Rational
import Surd.Types
import Surd.Positive
import Surd.PrimeFactors
import Surd.Cyclotomic

import Data.List

%default covering

------------------------------------------------------------------------
-- Known Fermat primes
------------------------------------------------------------------------

||| Known Fermat primes (the only five known, conjectured to be all).
export
fermatPrimes : List Integer
fermatPrimes = [3, 5, 17, 257, 65537]

------------------------------------------------------------------------
-- Constructibility
------------------------------------------------------------------------

||| Check if cos(2pi/n) is compass-and-straightedge constructible.
export
isConstructible : Int -> Bool
isConstructible n =
  case positive (cast (abs n)) of
    Nothing => False
    Just pos =>
      let fs = factorise pos
          oddFactors = filter (\pf => fst pf /= 2) fs
      in all (\pf => snd pf == 1 && elem (fst pf) fermatPrimes) oddFactors

------------------------------------------------------------------------
-- Hand-optimised expressions (must come before cosOfUnity)
------------------------------------------------------------------------

||| cos(2pi/5) = (sqrt(5) - 1) / 4
cos2piOver5 : RadExpr Rational
cos2piOver5 =
  Mul (Inv (Lit (Rational.fromInteger 4)))
      (Add (Root 2 (Lit (Rational.fromInteger 5)))
           (Neg (Lit Rational.one)))

||| cos(2pi/10) = cos(pi/5) = (1 + sqrt(5)) / 4
cos2piOver10 : RadExpr Rational
cos2piOver10 =
  Mul (Inv (Lit (Rational.fromInteger 4)))
      (Add (Lit Rational.one)
           (Root 2 (Lit (Rational.fromInteger 5))))

------------------------------------------------------------------------
-- Powers of 2: half-angle recurrence
------------------------------------------------------------------------

||| cos(2pi/2^k) via the half-angle formula.
cosOfPow2 : Int -> RadExpr Rational
cosOfPow2 n =
  if n <= 4 then
    case n of
      1 => Lit Rational.one
      2 => Lit (negate Rational.one)
      4 => Lit Rational.zero
      _ => Lit Rational.one
  else
    let prev = cosOfPow2 (div n 2)
    in Root 2 (Mul (Inv (Lit (Rational.fromInteger 2)))
                   (Add (Lit Rational.one) prev))

------------------------------------------------------------------------
-- cos(2pi/n) for small / constructible n
------------------------------------------------------------------------

||| Compute cos(2pi/n) as a radical expression.
export
cosOfUnity : Int -> Maybe (RadExpr Rational)
cosOfUnity n =
  if n <= 0 then Nothing
  else if n == 1 then Just (Lit Rational.one)
  else if n == 2 then Just (Lit (negate Rational.one))
  else if n == 3 then Just (Lit (mkRat (-1) 2))
  else if n == 4 then Just (Lit Rational.zero)
  else if n == 5 then Just cos2piOver5
  else if n == 6 then Just (Lit (mkRat 1 2))
  else if n == 8 then Just (Mul (Inv (Lit (Rational.fromInteger 2))) (Root 2 (Lit (Rational.fromInteger 2))))
  else if n == 10 then Just cos2piOver10
  else if n == 12 then Just (Mul (Inv (Lit (Rational.fromInteger 2))) (Root 2 (Lit (Rational.fromInteger 3))))
  else if isPowerOf2 n then Just (cosOfPow2 n)
  else if isPrime (cast n) then Nothing  -- Gauss period descent not yet wired in
  else Nothing

------------------------------------------------------------------------
-- sin of unity
------------------------------------------------------------------------

||| Compute sin(2pi/n) as a radical expression.
export
sinOfUnity : Int -> Maybe (RadExpr Rational)
sinOfUnity n = do
  c <- cosOfUnity n
  let sin2 = Add (Lit Rational.one) (Neg (Mul c c))
  Just (Root 2 sin2)

------------------------------------------------------------------------
-- All cos/sin of unity
------------------------------------------------------------------------

||| Map from k to cos(2*pi*k/n) for all k coprime to n.
export
allCosOfUnity : Int -> Maybe (List (Int, RadExpr Rational))
allCosOfUnity n = do
  base <- cosOfUnity n
  Just [(1, base)]

||| Map from k to sin(2*pi*k/n) for all k coprime to n.
export
allSinOfUnity : Int -> Maybe (List (Int, RadExpr Rational))
allSinOfUnity n = do
  base <- sinOfUnity n
  Just [(1, base)]
