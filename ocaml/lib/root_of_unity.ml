(** Roots of unity and Chebyshev polynomials.

    Provides exact radical forms for all n-th roots of unity
    and Chebyshev polynomial evaluation. *)

module R = Rational

(** Compute all cos(2*k*pi/n) for k = 0, ..., n-1.
    Returns a list of (k, radical_expression) pairs. *)
let all_cos_of_unity n =
  List.init n (fun k ->
    (k, Trig.cos_exact (2 * k) n))

(** Compute all sin(2*k*pi/n) for k = 0, ..., n-1. *)
let all_sin_of_unity n =
  List.init n (fun k ->
    (k, Trig.sin_exact (2 * k) n))
