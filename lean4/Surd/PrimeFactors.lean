/-
  Surd.PrimeFactors — Trial-division prime factorization.
-/
import Surd.Positive

namespace Surd

/-- A prime factorization: list of (prime, exponent) pairs. -/
def Factorization := List (Nat × Nat)

/-- Extract all factors of d from n, returning (n/d^e, e).
    Terminates because n strictly decreases when d ≥ 2 divides n. -/
private def extractFactor (n d : Nat) : Nat × Nat :=
  go n 0 n
where
  go (n e fuel : Nat) : Nat × Nat :=
    match fuel with
    | 0 => (n, e)
    | fuel' + 1 =>
      if d ≤ 1 then (n, e)
      else if n % d == 0 then go (n / d) (e + 1) fuel'
      else (n, e)

/-- Trial-division factorization of a natural number.
    Returns a list of (prime, exponent) pairs in ascending order. -/
def factoriseNat (n : Nat) : Factorization :=
  if n ≤ 1 then []
  else go n 2 n
where
  go (n d fuel : Nat) : Factorization :=
    match fuel with
    | 0 => if n > 1 then [(n, 1)] else []
    | fuel' + 1 =>
      if n ≤ 1 then []
      else if d * d > n then [(n, 1)]
      else if n % d == 0 then
        let (n', e) := extractFactor n d
        (d, e) :: go n' (d + 1) fuel'
      else
        go n (d + 1) fuel'

/-- Factorize a Positive. -/
def Positive.factorise (p : Positive) : Factorization :=
  factoriseNat p.val

/-- Check if a natural number is prime. -/
def isPrime (n : Nat) : Bool :=
  if n < 2 then false
  else go 2 n
where
  go (d fuel : Nat) : Bool :=
    match fuel with
    | 0 => true
    | fuel' + 1 =>
      if d * d > n then true
      else if n % d == 0 then false
      else go (d + 1) fuel'

/-- List of distinct prime factors (without exponents). -/
def primeFactors (n : Nat) : List Nat :=
  (factoriseNat n).map Prod.fst

/-- Euler's totient function. -/
def eulerTotient (n : Positive) : Nat :=
  (n.factorise).foldl (fun acc (p, e) =>
    acc * (p - 1) * p ^ (e - 1)) 1

end Surd
