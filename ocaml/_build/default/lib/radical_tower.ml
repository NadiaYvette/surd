(** Radical tower construction.

    Stub: given a solvable Galois group, construct a tower of
    radical extensions leading to the roots. *)

(** A step in a radical tower: adjoin the n-th root of an element. *)
type tower_step = {
  index : int;     (** root index *)
  radicand : Rational.t Rad_expr.t;
}

(** A radical tower: a sequence of radical extensions. *)
type t = tower_step list

(** Construct a radical tower for a polynomial with solvable Galois group. *)
let construct _f _group = ([] : t)
