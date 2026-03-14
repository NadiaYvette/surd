(** Conversion between radical expressions and algebraic numbers.

    [rad_expr_to_alg_num] computes the minimal polynomial and isolating
    interval of a radical expression.

    [alg_num_to_rad_expr] attempts to express an algebraic number as
    a radical expression (possible for degree <= 4 via quadratic formula,
    Cardano, Ferrari). *)

module P = Poly.RatPoly
module R = Rational

(** Convert a radical expression to an algebraic number. *)
let rad_expr_to_alg_num expr =
  let mp = Minimal_poly.minimal_poly expr in
  let v = Eval.eval_float expr in
  (* Create isolating interval around the numerical value *)
  let eps = 0.001 in
  let lo = R.of_ints (int_of_float ((v -. eps) *. 1000.0)) 1000 in
  let hi = R.of_ints (int_of_float ((v +. eps) *. 1000.0)) 1000 in
  Alg_num.make mp (Interval.make lo hi)

(** Attempt to convert an algebraic number to a radical expression.
    Works for degree <= 2 (quadratic formula). *)
let alg_num_to_rad_expr alg =
  let mp = alg.Alg_num.minpoly in
  let d = P.degree mp in
  match d with
  | 1 ->
    (* Linear: x + a/b = 0 -> x = -a/b *)
    let coeffs = P.to_coeffs mp in
    let a0 = List.nth coeffs 0 in
    let a1 = List.nth coeffs 1 in
    Some (Rad_expr.Lit (R.neg (R.div a0 a1)))
  | 2 ->
    (* Quadratic formula *)
    let coeffs = P.to_coeffs mp in
    let c = List.nth coeffs 0 in
    let b = List.nth coeffs 1 in
    let a = List.nth coeffs 2 in
    let disc = R.sub (R.mul b b) (R.mul (R.of_int 4) (R.mul a c)) in
    let two_a = R.mul (R.of_int 2) a in
    let neg_b = R.neg b in
    (* Choose the root that lies in the isolating interval *)
    let sqrt_disc = Rad_expr.Root (2, Rad_expr.Lit disc) in
    let root_plus = Rad_expr.Mul (
      Rad_expr.Inv (Rad_expr.Lit two_a),
      Rad_expr.Add (Rad_expr.Lit neg_b, sqrt_disc)) in
    let root_minus = Rad_expr.Mul (
      Rad_expr.Inv (Rad_expr.Lit two_a),
      Rad_expr.Add (Rad_expr.Lit neg_b, Rad_expr.Neg sqrt_disc)) in
    (* Evaluate both and pick the one in the interval *)
    let v_plus = Eval.eval_float root_plus in
    let mid = Alg_num.to_float alg in
    if Float.abs (v_plus -. mid) < 0.01 then Some (Normalize.normalize root_plus)
    else Some (Normalize.normalize root_minus)
  | _ -> None  (* Cardano/Ferrari: stub *)
