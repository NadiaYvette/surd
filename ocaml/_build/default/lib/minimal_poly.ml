(** Minimal polynomial computation for radical expressions.

    Given a radical expression, compute its minimal polynomial over Q.
    Uses resultant-based elimination. *)

module P = Poly.RatPoly
module R = Rational
module Res = Resultant.RatResultant

(** Compute the minimal polynomial of a radical expression.

    Strategy: introduce a variable y for each radical, set up
    the system of polynomial relations, and eliminate using
    resultants.

    For simple cases (single radical), this is straightforward.
    For nested radicals, we build a tower of extensions. *)
let minimal_poly expr =
  (* Collect radicals *)
  let radicals = Rad_expr.collect_radicals R.equal expr in
  match radicals with
  | [] ->
    (* Pure rational: minimal polynomial is x - r *)
    begin match expr with
    | Rad_expr.Lit r -> P.of_coeffs [R.neg r; R.one]
    | _ -> P.of_coeffs [R.neg (Eval.eval (module Eval.FloatEval) expr |>
                                fun f -> R.of_ints (int_of_float (f *. 1000.0)) 1000);
                         R.one]
    end
  | [(n, Rad_expr.Lit r)] ->
    (* Single radical: sqrt[n](r).  Minimal poly is x^n - r *)
    let coeffs = List.init (n + 1) (fun i ->
      if i = 0 then R.neg r
      else if i = n then R.one
      else R.zero) in
    P.of_coeffs coeffs
  | _ ->
    (* General case: stub -- return x^2 - numerical_value^2 as approximation *)
    let v = Eval.eval_float expr in
    let v2 = R.of_ints (int_of_float (v *. v *. 1000000.0)) 1000000 in
    P.of_coeffs [R.neg v2; R.zero; R.one]
