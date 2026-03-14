(** Resolvent polynomials for Galois group computation.

    Stub: provides the interface for resolvent computation.
    The sextic resolvent of a quintic is used to identify
    the Galois group. *)

module P = Poly.RatPoly

(** Compute the cubic resolvent of a quartic polynomial. *)
let cubic_resolvent _f = P.zero

(** Compute the sextic resolvent of a quintic polynomial. *)
let sextic_resolvent _f = P.zero
