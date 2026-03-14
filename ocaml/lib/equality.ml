(** Mathematical equality testing for radical expressions.

    Structural equality is not sufficient: e.g., [sqrt(2)^2] and [2]
    are mathematically equal but structurally different.  We use
    numerical evaluation with sufficient precision to test equality. *)

module R = Rational

(** Test whether two radical expressions are mathematically equal
    by evaluating both to complex numbers and comparing. *)
let are_equal a b =
  let za = Eval.eval_complex a in
  let zb = Eval.eval_complex b in
  let diff = Complex.sub za zb in
  Complex.norm diff < 1e-10

(** Test whether a radical expression is mathematically zero. *)
let is_zero expr =
  let z = Eval.eval_complex expr in
  Complex.norm z < 1e-10
