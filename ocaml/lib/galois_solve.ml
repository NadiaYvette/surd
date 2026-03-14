(** Radical solutions for solvable polynomials.

    Stub: given a polynomial with a solvable Galois group,
    express its roots as radical expressions. *)

module P = Poly.RatPoly

(** Attempt to solve a polynomial by radicals.
    Returns [Some roots] if the Galois group is solvable,
    [None] otherwise. *)
let solve _f : Rational.t Rad_expr.t list option = None

(** Check if a polynomial is solvable by radicals. *)
let is_solvable _f = false
