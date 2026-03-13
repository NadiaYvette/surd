/// Algebraic numbers represented as (minimal polynomial, isolating interval) pairs.
///
/// An algebraic number alpha is uniquely determined by its minimal polynomial
/// (irreducible over Q) and an isolating interval that contains exactly one root
/// of that polynomial.
module Surd.AlgNum

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.Interval
open Surd.RootIsolation
open Surd.Resultant

/// An algebraic number: (minpoly, isolating interval).
noeq type alg_num = {
  an_poly: poly rational;   (* minimal polynomial, monic and irreducible *)
  an_interval: interval;     (* isolating interval containing exactly this root *)
}

/// Create an algebraic number from a rational.
let alg_of_rational (r: rational) : alg_num =
  { an_poly = mk_poly #rational #ring_rational [rat_neg r; rat_one];  (* x - r *)
    an_interval = { lo = r; hi = r } }

/// Degree of an algebraic number (= degree of its minimal polynomial).
let alg_degree (a: alg_num) : int = degree a.an_poly

/// Approximate value as a rational (midpoint of isolating interval).
let alg_approx (a: alg_num) : rational = midpoint a.an_interval

/// Check if an algebraic number is rational (degree 1).
let is_rational (a: alg_num) : bool = alg_degree a = 1

/// Extract the rational value if the algebraic number is rational.
let to_rational (a: alg_num) : option rational =
  if is_rational a then
    match a.an_poly with
    | [c; _] -> Some (rat_neg c)  (* x + c = 0 => x = -c *)
    | _ -> None
  else None

/// Refine the isolating interval to a given precision.
val alg_refine : alg_num -> rational -> Dv alg_num
let alg_refine a eps =
  { a with an_interval = refine_root a.an_poly a.an_interval eps }

/// Addition of algebraic numbers via composed sum of minimal polynomials.
/// The result polynomial may not be irreducible; we take the factor containing
/// the sum's approximation.
val alg_add : alg_num -> alg_num -> Dv alg_num
let alg_add a b =
  let sum_poly = composed_sum #rational #field_rational a.an_poly b.an_poly in
  let sum_approx = rat_add (alg_approx a) (alg_approx b) in
  let sum_iv = { lo = rat_sub sum_approx (rat_of_int 1);
                 hi = rat_add sum_approx (rat_of_int 1) } in
  { an_poly = sum_poly; an_interval = sum_iv }

/// Negation of an algebraic number: p(-x) with negated interval.
let alg_neg (a: alg_num) : alg_num =
  { an_poly = negate_var #rational #ring_rational a.an_poly;
    an_interval = ineg a.an_interval }

/// Subtraction.
val alg_sub : alg_num -> alg_num -> Dv alg_num
let alg_sub a b = alg_add a (alg_neg b)

/// Comparison: refine both intervals until they are disjoint, then compare.
val alg_compare : alg_num -> alg_num -> Dv int
let rec alg_compare a b =
  if rat_lt a.an_interval.hi b.an_interval.lo then 0 - 1
  else if rat_gt a.an_interval.lo b.an_interval.hi then 1
  else
    let a' = alg_refine a (rat_div_total (width a.an_interval) (rat_of_int 4)) in
    let b' = alg_refine b (rat_div_total (width b.an_interval) (rat_of_int 4)) in
    alg_compare a' b'

/// Equality: same minimal polynomial and overlapping intervals (after refinement).
val alg_eq : alg_num -> alg_num -> Dv bool
let alg_eq a b =
  if not (poly_eq #rational #ring_rational a.an_poly b.an_poly) then false
  else overlaps a.an_interval b.an_interval
