/// Algebraic field extensions K(alpha) where alpha is a root of an irreducible
/// polynomial over K.
///
/// Elements of K(alpha) are polynomials of degree < deg(minpoly)
/// with arithmetic modulo the minimal polynomial.
module Surd.Extension

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly

/// An element of K(alpha): polynomial in alpha, reduced modulo minpoly.
noeq type ext_elem (k:Type) = {
  ee_coeffs: poly k;
  ee_minpoly: poly k;
}

/// Create an extension element, reducing modulo the minimal polynomial.
val mk_ext_elem : #k:Type -> {| field k |} -> poly k -> (mp: poly k{Cons? mp}) -> Dv (ext_elem k)
let mk_ext_elem #k #d coeffs mp =
  let (_, r) = div_mod_poly coeffs mp in
  { ee_coeffs = r; ee_minpoly = mp }

/// The zero element.
let ext_zero (#k:Type) (mp: poly k) : ext_elem k =
  { ee_coeffs = zero_poly; ee_minpoly = mp }

/// The one element.
let ext_one (#k:Type) {| ring k |} (mp: poly k) : ext_elem k =
  { ee_coeffs = [r_one]; ee_minpoly = mp }

/// alpha itself as an element.
let ext_gen (#k:Type) {| ring k |} (mp: poly k) : ext_elem k =
  { ee_coeffs = [r_zero; r_one]; ee_minpoly = mp }

/// Lift a scalar from K into K(alpha).
let ext_of_scalar (#k:Type) {| ring k |} (c: k) (mp: poly k) : ext_elem k =
  { ee_coeffs = const_poly c; ee_minpoly = mp }

/// Addition.
val ext_add : #k:Type -> {| field k |} -> ext_elem k -> ext_elem k -> Dv (ext_elem k)
let ext_add #k #d a b =
  let rng : ring k = f_ring in
  match a.ee_minpoly with
  | [] -> { ee_coeffs = add_poly #k #rng a.ee_coeffs b.ee_coeffs; ee_minpoly = a.ee_minpoly }
  | _ -> mk_ext_elem (add_poly #k #rng a.ee_coeffs b.ee_coeffs) a.ee_minpoly

/// Negation.
let ext_neg (#k:Type) {| ring k |} (a: ext_elem k) : ext_elem k =
  { ee_coeffs = neg_poly a.ee_coeffs; ee_minpoly = a.ee_minpoly }

/// Multiplication: multiply polynomials, reduce mod minpoly.
val ext_mul : #k:Type -> {| field k |} -> ext_elem k -> ext_elem k -> Dv (ext_elem k)
let ext_mul #k #d a b =
  let rng : ring k = f_ring in
  let prod = mul_poly #k #rng a.ee_coeffs b.ee_coeffs in
  match a.ee_minpoly with
  | [] -> { ee_coeffs = prod; ee_minpoly = a.ee_minpoly }
  | _ -> mk_ext_elem prod a.ee_minpoly

/// Extended Euclidean algorithm for polynomials.
val ext_gcd_poly : #k:Type -> {| field k |} -> poly k -> poly k -> Dv (poly k & poly k & poly k)
let rec ext_gcd_poly #k #d a b =
  let rng : ring k = f_ring in
  match b with
  | [] -> (a, [rng.r_one], zero_poly)
  | _ ->
    let (q, r) = div_mod_poly a b in
    let (g, s, t) = ext_gcd_poly b r in
    (g, t, sub_poly #k #rng s (mul_poly #k #rng t q))

/// Multiplicative inverse via extended GCD.
val ext_inv : #k:Type -> {| field k |} -> ext_elem k -> Dv (ext_elem k)
let ext_inv #k #d a =
  let rng : ring k = f_ring in
  match a.ee_coeffs, a.ee_minpoly with
  | [], _ -> a
  | _, [] -> a
  | _, _ ->
    let (g, s, _) = ext_gcd_poly a.ee_coeffs a.ee_minpoly in
    let lc_g = match lead_coeff g with | Some c -> c | None -> rng.r_one in
    let s_normalized = scale_poly #k #rng (f_inv lc_g) s in
    mk_ext_elem s_normalized a.ee_minpoly

/// Equality.
let ext_eq (#k:Type) {| ring k |} (a b : ext_elem k) : bool =
  poly_eq a.ee_coeffs b.ee_coeffs

/// Power via repeated squaring.
val ext_pow : #k:Type -> {| field k |} -> ext_elem k -> nat -> Dv (ext_elem k)
let rec ext_pow #k #d a n =
  if n = 0 then ext_one #k #(f_ring #k) a.ee_minpoly
  else if n = 1 then a
  else
    let half = ext_pow a (n / 2) in
    let sq = ext_mul half half in
    if n % 2 = 0 then sq
    else ext_mul sq a
