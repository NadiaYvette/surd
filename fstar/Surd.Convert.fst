/// Conversion between radical expressions and algebraic numbers.
///
/// radExprToAlgNum: compute the algebraic number for a radical expression.
/// algNumToRadExpr: find a radical expression for an algebraic number
///   (quadratic, Cardano, Ferrari for degrees 2-4).
module Surd.Convert

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.Interval
open Surd.AlgNum
open Surd.MinimalPoly
open Surd.RootIsolation
open Surd.Eval

/// Convert a radical expression to an algebraic number.
///
/// Computes the minimal polynomial and uses numerical evaluation to
/// determine the correct isolating interval.
val rad_expr_to_alg_num : rad_expr rational -> Dv alg_num
let rad_expr_to_alg_num e =
  let mp = minimal_poly e in
  (* Use interval arithmetic to find the correct root *)
  let iv = eval_interval e in
  { an_poly = mp; an_interval = iv }

/// Solve a quadratic: x^2 + bx + c = 0.
/// Returns both roots as radical expressions.
let solve_quadratic (b c: rational) : (rad_expr rational & rad_expr rational) =
  let disc = rat_sub (rat_mul b b) (rat_mul (rat_of_int 4) c) in
  let neg_b = rat_neg b in
  let two = rat_of_int 2 in
  let sqrt_disc = Root 2 (Lit disc) in
  let r1 = Mul (Lit (mk_rational 1 2)) (Add (Lit neg_b) sqrt_disc) in
  let r2 = Mul (Lit (mk_rational 1 2)) (Add (Lit neg_b) (Neg sqrt_disc)) in
  (r1, r2)

/// Cardano's formula for x^3 + px + q = 0.
/// Returns one real root as a radical expression.
let solve_depressed_cubic (p q: rational) : rad_expr rational =
  let disc = rat_add (rat_mul (rat_of_int 4) (rat_mul (rat_mul p p) p))
                     (rat_mul (rat_of_int 27) (rat_mul q q)) in
  let neg_q_half = rat_mul (mk_rational (0-1) 2) q in
  (* u = cbrt(-q/2 + sqrt(disc/108)) *)
  let disc_over_108 = rat_div_total disc (rat_of_int 108) in
  let under_sqrt = Root 2 (Lit disc_over_108) in
  let u = Root 3 (Add (Lit neg_q_half) under_sqrt) in
  (* v = -p/(3u) *)
  let three_u = Mul (Lit (rat_of_int 3)) u in
  let v = Mul (Lit (rat_neg p)) (Inv three_u) in
  Add u v

/// Convert a monic polynomial to depressed form by substituting x = t - a_{n-1}/n.
let depress_cubic (poly: poly rational) : (rational & rational & rational) =
  match poly with
  | [c; b; a; _] ->  (* c + bx + ax^2 + x^3 *)
    let shift = rat_mul (mk_rational (0-1) 3) a in
    let p = rat_sub b (rat_mul (mk_rational 1 3) (rat_mul a a)) in
    let q = rat_add c (rat_sub (rat_mul (mk_rational 2 27) (rat_mul a (rat_mul a a)))
                               (rat_mul (mk_rational 1 3) (rat_mul a b))) in
    (shift, p, q)
  | _ -> (rat_zero, rat_zero, rat_zero)

/// Convert an algebraic number of degree <= 4 to a radical expression.
///
/// Degree 1: rational
/// Degree 2: quadratic formula
/// Degree 3: Cardano's formula
/// Degree 4: Ferrari's method (stub)
val alg_num_to_rad_expr : alg_num -> Dv (option (rad_expr rational))
let alg_num_to_rad_expr a =
  let d = alg_degree a in
  if d = 1 then
    match to_rational a with
    | Some r -> Some (Lit r)
    | None -> None
  else if d = 2 then
    match a.an_poly with
    | [c; b; _] ->  (* c + bx + x^2 *)
      let (r1, r2) = solve_quadratic b c in
      (* Pick the root whose interval overlaps *)
      let iv1 = eval_interval r1 in
      if overlaps iv1 a.an_interval then Some r1
      else Some r2
    | _ -> None
  else if d = 3 then
    match a.an_poly with
    | [_; _; _; _] ->
      let (shift, p, q) = depress_cubic a.an_poly in
      let depressed_root = solve_depressed_cubic p q in
      let root = Add depressed_root (Lit shift) in
      Some root
    | _ -> None
  else None  (* degree 4+ not yet implemented *)
