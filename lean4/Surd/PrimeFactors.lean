/-
  Surd.PrimeFactors — Trial-division prime factorization.
-/
import Surd.Positive

namespace Surd

/-- A prime factorization: list of (prime, exponent) pairs. -/
def Factorization := List (Nat × Nat)

/-- Extract all factors of d from n, returning (n/d^e, e). -/
private partial def extractFactor (n d : Nat) : Nat × Nat :=
  go n 0
where
  go (n e : Nat) : Nat × Nat :=
    if d == 0 then (n, e)
    else if n % d == 0 then go (n / d) (e + 1)
    else (n, e)

/-- Trial-division factorization of a natural number.
    Returns a list of (prime, exponent) pairs in ascending order. -/
partial def factoriseNat (n : Nat) : Factorization :=
  if n ≤ 1 then []
  else go n 2
where
  go (n d : Nat) : Factorization :=
    if n ≤ 1 then []
    else if d * d > n then [(n, 1)]
    else if n % d == 0 then
      let (n', e) := extractFactor n d
      (d, e) :: go n' (d + 1)
    else
      go n (d + 1)

/-- Factorize a Positive. -/
def Positive.factorise (p : Positive) : Factorization :=
  factoriseNat p.val

/-- Check if a natural number is prime. -/
partial def isPrime (n : Nat) : Bool :=
  if n < 2 then false
  else go 2
where
  go (d : Nat) : Bool :=
    if d * d > n then true
    else if n % d == 0 then false
    else go (d + 1)

/-- List of distinct prime factors (without exponents). -/
def primeFactors (n : Nat) : List Nat :=
  (factoriseNat n).map Prod.fst

/-- Euler's totient function. -/
def eulerTotient (n : Positive) : Nat :=
  (n.factorise).foldl (fun acc (p, e) =>
    acc * (p - 1) * p ^ (e - 1)) 1

end Surd
