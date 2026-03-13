/// Elliptic integral reduction and symbolic computation.
///
/// Handles integrals of the form integral R(x, sqrt(P(x))) dx where
/// P(x) is a cubic or quartic polynomial. These cannot be expressed
/// in terms of elementary functions but can be reduced to standard
/// elliptic integrals (Legendre normal form).
module Surd.EllipticIntegration

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly

/// Standard form of an elliptic integral.
noeq type elliptic_form =
  | EllipticK : rational -> elliptic_form    (* K(k) = complete elliptic integral of first kind *)
  | EllipticE : rational -> elliptic_form    (* E(k) = complete elliptic integral of second kind *)
  | EllipticPi : rational -> rational -> elliptic_form  (* Pi(n, k) = third kind *)
  | IncompleteK : rational -> rational -> elliptic_form  (* F(phi, k) = incomplete first kind *)
  | IncompleteE : rational -> rational -> elliptic_form  (* E(phi, k) = incomplete second kind *)

/// A polynomial under a square root in an elliptic integral.
noeq type elliptic_integrand = {
  ei_polynomial: poly rational;   (* the polynomial P(x) under sqrt *)
  ei_rational_part: poly rational; (* R(x) multiplying 1/sqrt(P(x)) *)
}

/// Classify the degree of the polynomial to determine elliptic type.
let classify_integrand (ei: elliptic_integrand) : string =
  let d = degree ei.ei_polynomial in
  if d <= 2 then "elementary"        (* can use Euler substitution *)
  else if d = 3 then "elliptic-cubic"
  else if d = 4 then "elliptic-quartic"
  else "hyperelliptic"

/// Reduce a quartic to Weierstrass normal form y^2 = 4x^3 - g2*x - g3
/// via a Moebius transformation.
///
/// Given y^2 = a4*x^4 + a3*x^3 + a2*x^2 + a1*x + a0, if r is a root
/// of the quartic, substitute x = r + 1/t to reduce to a cubic in t.
val reduce_to_weierstrass : poly rational -> Dv (option (rational & rational))
let reduce_to_weierstrass p =
  let d = degree p in
  if d = 3 then
    (* Already cubic: extract g2, g3 from 4x^3 - g2*x - g3 form *)
    match p with
    | [a0; a1; _; a3] ->
      if rat_eq a3 rat_zero then None
      else
        (* Scale to make leading coefficient 4 *)
        let scale = rat_div_total (rat_of_int 4) a3 in
        let g3 = rat_neg (rat_mul scale a0) in
        let g2 = rat_neg (rat_mul scale a1) in
        Some (g2, g3)
    | _ -> None
  else if d = 4 then
    (* Quartic: needs a root to reduce *)
    None  (* stub: full reduction requires root-finding *)
  else None

/// Compute the j-invariant of an elliptic curve y^2 = 4x^3 - g2*x - g3.
/// j = 1728 * g2^3 / (g2^3 - 27*g3^2).
let j_invariant (g2 g3: rational) : rational =
  let g2_cubed = rat_mul g2 (rat_mul g2 g2) in
  let g3_sq = rat_mul g3 g3 in
  let delta = rat_sub g2_cubed (rat_mul (rat_of_int 27) g3_sq) in
  if rat_eq delta rat_zero then rat_zero  (* degenerate *)
  else rat_mul (rat_of_int 1728) (rat_div_total g2_cubed delta)

/// Symbolic representation of an elliptic integral result.
noeq type elliptic_result = {
  er_forms: list elliptic_form;         (* sum of standard forms *)
  er_coefficients: list rational;       (* rational coefficients *)
  er_elementary_part: rad_expr rational; (* elementary (non-elliptic) part *)
}

/// Attempt to express an integral in terms of standard elliptic integrals.
val reduce_to_standard : elliptic_integrand -> Dv (option elliptic_result)
let reduce_to_standard ei =
  let d = degree ei.ei_polynomial in
  if d <= 2 then
    (* Elementary: not elliptic *)
    None
  else if d = 3 || d = 4 then
    (* Elliptic: attempt Weierstrass reduction *)
    match reduce_to_weierstrass ei.ei_polynomial with
    | Some (g2, g3) ->
      (* Compute modular parameter k from g2, g3 *)
      (* k^2 = (e2 - e3)/(e1 - e3) where e1, e2, e3 are roots *)
      (* Stub: return K(0) as placeholder *)
      Some { er_forms = [EllipticK rat_zero];
             er_coefficients = [rat_one];
             er_elementary_part = Lit rat_zero }
    | None -> None
  else None  (* hyperelliptic: not supported *)
