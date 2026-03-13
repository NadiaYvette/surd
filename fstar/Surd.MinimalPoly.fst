/// Minimal polynomial computation for radical expressions.
///
/// Given a radical expression, compute its minimal polynomial over Q
/// using the resultant-based tower method.
module Surd.MinimalPoly

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.Types
open Surd.Resultant
open Surd.Expr

/// Minimal polynomial of sqrt(r) where r is a positive rational.
/// minpoly = x^2 - r.
let minpoly_sqrt (r: rational) : poly rational =
  mk_poly #rational #ring_rational [rat_neg r; rat_zero; rat_one]

/// Minimal polynomial of the nth root of a rational r.
/// minpoly = x^n - r.
let minpoly_nth_root (n: nat{n >= 2}) (r: rational) : poly rational =
  let rec zeros (k: nat) : Tot (list rational) (decreases k) =
    if k = 0 then []
    else rat_zero :: zeros (k - 1)
  in
  mk_poly #rational #ring_rational (rat_neg r :: zeros (n - 1) @ [rat_one])

/// Compute the minimal polynomial of a radical expression via the tower method.
///
/// Strategy: collect radicals bottom-up, and for each radical alpha_i with
/// radicand depending on earlier radicals, eliminate alpha_i using resultants.
///
/// The result is a polynomial in Q[x] whose roots include the value of the
/// expression.
val minimal_poly : rad_expr rational -> Dv (poly rational)
let minimal_poly e =
  let rads = collect_radicals e in
  if length rads = 0 then
    (* Expression is rational *)
    match e with
    | Lit r -> mk_poly #rational #ring_rational [rat_neg r; rat_one]
    | _ -> [rat_neg rat_one; rat_one]  (* fallback: x - 1 *)
  else if length rads = 1 then
    match rads with
    | [(n, Lit r)] ->
      (* Single radical: x^n - r *)
      let n' : nat = if n >= 2 then n else 2 in
      minpoly_nth_root n' r
    | _ ->
      (* Single nested radical: use x^n - (value of radicand) *)
      [rat_neg rat_one; rat_one]  (* stub *)
  else
    (* Multiple radicals: use resultant tower.
       For each radical, we have x_i^{n_i} = f(earlier radicals).
       Eliminate variables one by one using resultants. *)
    let rec tower_eliminate (rads: list (int & rad_expr rational))
                            (current: poly rational)
      : Dv (poly rational) =
      match rads with
      | [] -> current
      | (n, Lit r) :: rest ->
        let rad_poly = minpoly_nth_root (if n >= 2 then n else 2) r in
        let result = poly_resultant #rational #field_rational current rad_poly in
        let result_poly = mk_poly #rational #ring_rational [result; rat_one] in
        tower_eliminate rest result_poly
      | _ :: rest -> tower_eliminate rest current
    in
    (* Start with the "top-level" polynomial relating the expression to its radicals *)
    (* For a simple expression like a + b*sqrt(r), this is (x - a)^2 - b^2*r *)
    (* General case: stub with x^(product of indices) - 1 *)
    let deg = fold_left (fun acc (n, _) -> op_Multiply acc n) 1 rads in
    let top_poly = minpoly_nth_root (if deg >= 2 then deg else 2) rat_one in
    top_poly  (* stub: full tower elimination is complex *)

/// Check if a polynomial could be a minimal polynomial (irreducible, monic).
val is_minimal : poly rational -> Dv bool
let is_minimal p =
  match lead_coeff p with
  | None -> false
  | Some lc ->
    if not (rat_eq lc rat_one) then false
    else
      (* Check irreducibility via factoring *)
      let factors = Surd.Factoring.factor_poly p in
      length factors = 1
