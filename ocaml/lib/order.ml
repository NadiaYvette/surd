(** Ordering of radical expressions.

    For real-valued radical expressions, provides a total order
    via numerical evaluation. *)

(** Compare two radical expressions numerically.
    Returns a negative number, zero, or positive number. *)
let compare_numerical a b =
  let va = Eval.eval_float a in
  let vb = Eval.eval_float b in
  Float.compare va vb
