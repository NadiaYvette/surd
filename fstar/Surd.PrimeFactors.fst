/// Small prime factorization and related utilities.
/// Uses the Div effect for potentially non-terminating trial division.
module Surd.PrimeFactors

open Surd.Positive

/// Integer power (non-negative exponent).
let rec pow_int (base: int) (exp: nat) : Tot int (decreases exp) =
  if exp = 0 then 1
  else op_Multiply base (pow_int base (exp - 1))

/// Trial-division primality test.
val is_prime : int -> Dv bool
let is_prime n =
  if n < 2 then false
  else if n < 4 then true
  else
    let rec check (d: pos) : Dv bool =
      if op_Multiply d d > n then true
      else if n % d = 0 then false
      else check (d + 1)
    in
    if n % 2 = 0 then false
    else check 3

/// Extract the exponent of prime p in n: returns (exponent, remaining quotient).
val extract_factor : pos -> int -> Dv (int & int)
let extract_factor p n =
  let rec go (n: int) (e: int) : Dv (int & int) =
    if n % p = 0 then go (n / p) (e + 1)
    else (e, n)
  in
  go n 0

/// Factorise a positive integer into (prime, exponent) pairs, in ascending order.
///
/// factorise 360 = [(2,3); (3,2); (5,1)]
/// factorise 1   = []
val factorise : positive -> Dv (list (int & int))
let factorise n =
  let rec go (m: int) (d: pos) : Dv (list (int & int)) =
    if m <= 1 then []
    else if op_Multiply d d > m then
      [(m, 1)]
    else
      let (e, q) = extract_factor d m in
      if e > 0 then (d, e) :: go q (d + 1)
      else go m (d + 1)
  in
  go n 2

/// Distinct prime factors in ascending order.
val prime_factors : positive -> Dv (list int)
let prime_factors n =
  FStar.List.Tot.map fst (factorise n)

/// Euler's totient function.
val euler_totient : positive -> Dv int
let euler_totient n =
  let factors = factorise n in
  FStar.List.Tot.fold_left
    (fun (acc:int) ((p, e) : int & int) ->
      let e' : nat = if e - 1 >= 0 then e - 1 else 0 in
      op_Multiply acc (op_Multiply (p - 1) (pow_int p e')))
    1
    factors

/// Number of divisors.
val num_divisors : positive -> Dv int
let num_divisors n =
  let factors = factorise n in
  FStar.List.Tot.fold_left
    (fun (acc:int) ((_, e) : int & int) -> op_Multiply acc (e + 1))
    1
    factors
