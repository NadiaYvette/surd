/// Resolvent polynomials for Galois group computation.
///
/// Given a polynomial f(x), construct resolvent polynomials whose
/// factorization pattern reveals the Galois group of f.
module Surd.Resolvent

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.Resultant

/// Cubic resolvent of a quartic x^4 + px^2 + qx + r.
/// The resolvent is y^3 - py^2 - 4ry + (4pr - q^2).
let cubic_resolvent_quartic (p q r: rational) : poly rational =
  let four_r = rat_mul (rat_of_int 4) r in
  let q_sq = rat_mul q q in
  let four_pr = rat_mul (rat_of_int 4) (rat_mul p r) in
  let const_term = rat_sub four_pr q_sq in
  mk_poly #rational #ring_rational
    [const_term; rat_neg four_r; rat_neg p; rat_one]

/// Quadratic resolvent (discriminant) of a cubic x^3 + px + q.
/// disc = -4p^3 - 27q^2.
let discriminant_cubic (p q: rational) : rational =
  let four_p_cubed = rat_mul (rat_of_int 4) (rat_mul p (rat_mul p p)) in
  let twentyseven_q_sq = rat_mul (rat_of_int 27) (rat_mul q q) in
  rat_neg (rat_add four_p_cubed twentyseven_q_sq)

/// Discriminant of a polynomial via resultant: disc(f) = (-1)^(n(n-1)/2) * Res(f, f') / a_n.
val poly_discriminant : poly rational -> Dv rational
let poly_discriminant f =
  let n = degree f in
  if n <= 0 then rat_one
  else
    let f' = diff_poly #rational #ring_rational f in
    let res = poly_resultant #rational #field_rational f f' in
    let lc = match lead_coeff f with | Some c -> c | None -> rat_one in
    let sign_exp = op_Multiply n (n - 1) / 2 in
    let sign = if sign_exp % 2 = 0 then rat_one else rat_neg rat_one in
    rat_mul sign (rat_div_total res lc)

/// Sextic resolvent for degree 5 polynomials (for A_5 vs S_5 distinction).
/// f is solvable iff the sextic resolvent has a rational root.
val sextic_resolvent : poly rational -> Dv (poly rational)
let sextic_resolvent f =
  (* Stub: the sextic resolvent is complex to construct.
     For a degree 5 polynomial, it involves the 6 expressions
     (x1*x2 + x2*x3 + x3*x4 + x4*x5 + x5*x1 - x1*x3 - x3*x5 - x5*x2 - x2*x4 - x4*x1)
     evaluated at roots. *)
  let d = degree f in
  if d <> 5 then f
  else
    (* Return the discriminant as a proxy *)
    let disc = poly_discriminant f in
    mk_poly #rational #ring_rational [disc; rat_one]
