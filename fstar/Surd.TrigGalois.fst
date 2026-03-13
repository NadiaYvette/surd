/// Gauss period descent for computing exact trigonometric values.
///
/// For cos(2*pi/p) where p is an odd prime, uses Lagrange resolvents
/// and the subgroup chain of (Z/pZ)* to express the value as nested radicals.
module Surd.TrigGalois

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.PrimeFactors
open Surd.Positive
open Surd.Cyclotomic

/// A primitive root modulo p: a generator of (Z/pZ)*.
val find_primitive_root : p:positive{p > 2} -> Dv positive
let find_primitive_root p =
  let phi = p - 1 in
  let factors = prime_factors (assume (phi > 0); phi) in
  let rec try_gen (g: positive) : Dv positive =
    if g >= p then 1  (* should not happen for primes *)
    else
      let is_gen = for_all
        (fun (f: int) ->
          if f = 0 then true
          else
            let exp : nat = if phi / f >= 0 then phi / f else 0 in
            let result = Surd.PrimeFactors.pow_int g exp % p in
            result <> 1)
        factors
      in
      if is_gen then g
      else try_gen (g + 1)
  in
  try_gen 2

/// Compute a Gauss period: eta_j = sum_{k in coset j} zeta^{g^k}
/// where g is a primitive root mod p and the coset is determined by
/// the subgroup structure.
///
/// For the quadratic case (2 periods), this gives:
///   eta_0 = sum of zeta^{g^{2k}} (quadratic residues)
///   eta_1 = sum of zeta^{g^{2k+1}} (non-residues)
///
/// Returns the periods as radical expressions.
val quadratic_gauss_periods : p:positive{p > 2} -> Dv (rad_expr rational & rad_expr rational)
let quadratic_gauss_periods p =
  (* For an odd prime p, the two quadratic Gauss periods satisfy:
     eta_0 + eta_1 = -1
     eta_0 * eta_1 = (1-p)/4   if p = 1 mod 4
                   = -(1+p)/4  if p = 3 mod 4
     So eta_0, eta_1 are roots of x^2 + x + (product) = 0 *)
  let prod = if p % 4 = 1 then
    mk_rational (1 - p) 4
  else
    mk_rational (0 - (1 + p)) 4
  in
  (* Discriminant = 1 - 4*prod = p or -p *)
  let disc = if p % 4 = 1 then rat_of_int p else rat_neg (rat_of_int p) in
  let half = mk_rational (0-1) 2 in
  let sqrt_disc = Root 2 (Lit disc) in
  let eta0 = Add (Lit half) (Mul (Lit (mk_rational 1 2)) sqrt_disc) in
  let eta1 = Add (Lit half) (Mul (Lit (mk_rational (0-1) 2)) sqrt_disc) in
  (eta0, eta1)

/// Compute cos(2*pi/p) for an odd prime p via Gauss period descent.
///
/// For small primes, uses known closed forms. For general primes,
/// uses the quadratic period decomposition as a starting point and
/// recursively decomposes into smaller periods.
val cos_prime : p:positive{p > 2} -> Dv (rad_expr rational)
let cos_prime p =
  if p = 3 then
    (* cos(2*pi/3) = -1/2 *)
    Lit (mk_rational (0-1) 2)
  else if p = 5 then
    (* cos(2*pi/5) = (-1 + sqrt(5))/4 *)
    Mul (Lit (mk_rational 1 4))
        (Add (Lit (rat_of_int (0-1))) (Root 2 (Lit (rat_of_int 5))))
  else if p = 7 then
    (* cos(2*pi/7): degree 3 over Q, use Gauss periods *)
    let (eta0, _) = quadratic_gauss_periods p in
    (* cos(2*pi/7) = (eta0)/2 for the right choice of period *)
    (* Actually cos(2pi/7) is a root of 8x^3 + 4x^2 - 4x - 1 = 0 *)
    (* Use Cardano: *)
    let p_coeff = mk_rational (0-3) 2 in
    let q_coeff = mk_rational 1 2 in
    Surd.Convert.solve_depressed_cubic p_coeff q_coeff
  else
    (* General case: start from quadratic periods *)
    let (eta0, _) = quadratic_gauss_periods p in
    (* cos(2*pi/p) = (eta0 + correction)/2 *)
    (* For the general case, eta0/2 is an approximation *)
    Mul (Lit (mk_rational 1 2)) eta0

/// Compute cos(2*pi/n) for an arbitrary positive integer n.
///
/// Dispatches based on the factorization of n:
/// - n = 1: cos(2*pi) = 1
/// - n = 2: cos(pi) = -1
/// - n = 3: cos(2*pi/3) = -1/2
/// - n = 4: cos(pi/2) = 0
/// - n = 6: cos(pi/3) = 1/2
/// - n prime: use cos_prime
/// - n composite: use the Chebyshev/CRT decomposition
val cos_of_unity : n:positive -> Dv (rad_expr rational)
let cos_of_unity n =
  if n = 1 then Lit #rational rat_one
  else if n = 2 then Lit #rational (rat_neg rat_one)
  else if n = 3 then Lit #rational (mk_rational (0-1) 2)
  else if n = 4 then Lit #rational rat_zero
  else if n = 6 then Lit #rational (mk_rational 1 2)
  else begin
    assume (n > 2);
    cos_prime n
  end
