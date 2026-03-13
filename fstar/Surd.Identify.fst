/// Galois group identification for polynomials of degree <= 5.
///
/// Uses discriminant, resolvent polynomials, and factorization patterns
/// to determine the Galois group.
module Surd.Identify

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.Factoring
open Surd.Resolvent
open Surd.TransitiveGroup

/// Check if a rational is a perfect square.
val is_perfect_square_rat : rational -> bool
let is_perfect_square_rat r =
  if rat_lt r rat_zero then false
  else if rat_eq r rat_zero then true
  else
    let n = Surd.Rational.abs_int r.num in
    let d = r.den in
    let rec isqrt (x k: nat) : Tot bool (decreases (x - k)) =
      if op_Multiply k k = x then true
      else if op_Multiply k k > x || k > x then false
      else isqrt x (k + 1)
    in
    isqrt n 0 && isqrt d 0

/// Identify the Galois group of a polynomial of degree 2.
let identify_degree_2 (f: poly rational) : Dv transitive_group_id =
  let disc = poly_discriminant f in
  if is_perfect_square_rat disc then TG_Cn 1
  else TG_Cn 2

/// Identify the Galois group of a polynomial of degree 3.
val identify_degree_3 : poly rational -> Dv transitive_group_id
let identify_degree_3 f =
  (* Check if f has a rational root *)
  let factors = factor_poly f in
  let has_linear = existsb (fun p -> degree p = 1) factors in
  if has_linear then
    (* Reducible: group is trivial or C_2 *)
    TG_Cn 2
  else
    let disc = poly_discriminant f in
    if is_perfect_square_rat disc then TG_An 3  (* A_3 = C_3 *)
    else TG_Sn 3

/// Identify the Galois group of a polynomial of degree 4.
val identify_degree_4 : poly rational -> Dv transitive_group_id
let identify_degree_4 f =
  let factors = factor_poly f in
  let has_linear = existsb (fun p -> degree p = 1) factors in
  if has_linear then TG_Sn 3  (* cubic quotient *)
  else
    let disc = poly_discriminant f in
    (* Compute cubic resolvent *)
    match f with
    | [r; q; p; _; _] ->
      let resolvent = cubic_resolvent_quartic p q r in
      let res_factors = factor_poly resolvent in
      let n_linear = length (filter (fun p -> degree p = 1) res_factors) in
      if n_linear = 3 then
        if is_perfect_square_rat disc then TG_V4
        else TG_Dn 4
      else if n_linear = 1 then
        if is_perfect_square_rat disc then TG_An 4
        else TG_Sn 4
      else TG_Sn 4
    | _ -> TG_Sn 4

/// Identify the Galois group of a polynomial of degree 5.
val identify_degree_5 : poly rational -> Dv transitive_group_id
let identify_degree_5 f =
  let factors = factor_poly f in
  let has_linear = existsb (fun p -> degree p = 1) factors in
  if has_linear then TG_Sn 4  (* quartic quotient *)
  else
    let disc = poly_discriminant f in
    if is_perfect_square_rat disc then TG_An 5
    else
      (* Check for F_20: the polynomial is solvable iff the sextic resolvent
         has a rational root *)
      let sex = sextic_resolvent f in
      let sex_factors = factor_poly sex in
      let has_sex_root = existsb (fun p -> degree p = 1) sex_factors in
      if has_sex_root then TG_F20
      else TG_Sn 5

/// Main entry point: identify the Galois group of an irreducible polynomial.
val identify_galois_group : poly rational -> Dv transitive_group_id
let identify_galois_group f =
  let d = degree f in
  if d <= 1 then TG_Cn 1
  else if d = 2 then identify_degree_2 f
  else if d = 3 then identify_degree_3 f
  else if d = 4 then identify_degree_4 f
  else if d = 5 then identify_degree_5 f
  else TG_Other d 0  (* degree > 5: not yet supported *)
