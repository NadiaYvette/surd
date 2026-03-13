/// Cyclotomic polynomials and related number-theoretic utilities.
///
/// The nth cyclotomic polynomial Phi_n(x) is the minimal polynomial of
/// primitive nth roots of unity over Q. It has degree phi(n) (Euler's totient).
module Surd.Cyclotomic

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.PrimeFactors
open Surd.Positive

/// Proper divisors of n (all divisors d with 1 <= d < n).
val proper_divisors : positive -> Dv (list positive)
let proper_divisors n =
  let rec go (d: positive) : Dv (list positive) =
    if d >= n then []
    else if n % d = 0 then d :: go (d + 1)
    else go (d + 1)
  in
  go 1

/// x^n - 1 as a polynomial over Q.
let x_to_n_minus_1 (n: positive) : poly rational =
  let rec zeros (k: nat) : Tot (list rational) (decreases k) =
    if k = 0 then []
    else rat_zero :: zeros (k - 1)
  in
  mk_poly #rational #ring_rational (rat_neg rat_one :: zeros (n - 1) @ [rat_one])

/// Compute the nth cyclotomic polynomial Phi_n(x) over Q.
///
/// Uses the identity: x^n - 1 = prod_{d|n} Phi_d(x)
/// So Phi_n(x) = (x^n - 1) / prod_{d|n, d<n} Phi_d(x)
val cyclotomic : positive -> Dv (poly rational)
let rec cyclotomic n =
  if n = 1 then
    mk_poly #rational #ring_rational [rat_neg rat_one; rat_one]
  else
    let xnm1 = x_to_n_minus_1 n in
    let divs = proper_divisors n in
    let rec mul_all (ds: list positive) : Dv (poly rational) =
      match ds with
      | [] -> [rat_one]
      | d :: rest -> mul_poly #rational #ring_rational (cyclotomic d) (mul_all rest)
    in
    let denom = mul_all divs in
    match denom with
    | [] -> xnm1
    | _ -> let (q, _) = div_mod_poly #rational #field_rational xnm1 denom in q

/// Moebius function mu(n).
val moebius_mu : positive -> Dv int
let moebius_mu n =
  if n = 1 then 1
  else
    let fs = factorise n in
    if existsb (fun ((_, e) : int & int) -> e > 1) fs then 0
    else if length fs % 2 = 0 then 1 else 0 - 1

/// Test if n is a prime power: n = p^k for some prime p and k >= 1.
val is_prime_power : positive -> Dv (option (int & int))
let is_prime_power n =
  let fs = factorise n in
  match fs with
  | [(p, e)] -> Some (p, e)
  | _ -> None

/// Degree of the nth cyclotomic polynomial (= phi(n)).
val cyclotomic_degree : positive -> Dv int
let cyclotomic_degree n = euler_totient n
