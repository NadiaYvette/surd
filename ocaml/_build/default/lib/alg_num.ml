(** Algebraic numbers represented as (minimal polynomial, isolating interval).

    An algebraic number is a root of a polynomial with rational
    coefficients.  We represent it as the pair of its minimal polynomial
    and an interval that isolates it from all other roots. *)

module P = Poly.RatPoly
module R = Rational

type t = {
  minpoly : P.t;
  interval : Interval.t;
}

(** Create an algebraic number from a minimal polynomial and an
    isolating interval.  The interval must contain exactly one root
    of the polynomial. *)
let make minpoly interval = { minpoly; interval }

(** Refine the isolating interval by bisection until width < epsilon. *)
let refine epsilon alg =
  let rec go iv =
    if R.compare (Interval.width iv) epsilon <= 0 then { alg with interval = iv }
    else
      let mid = R.div (R.add (Interval.lo iv) (Interval.hi iv)) (R.of_int 2) in
      let v = P.eval alg.minpoly mid in
      let v_lo = P.eval alg.minpoly (Interval.lo iv) in
      if R.is_zero v then { alg with interval = Interval.of_rational mid }
      else if (R.is_pos v && R.is_pos v_lo) || (R.is_neg v && R.is_neg v_lo) then
        go (Interval.make mid (Interval.hi iv))
      else
        go (Interval.make (Interval.lo iv) mid)
  in
  go alg.interval

(** Approximate value as a float. *)
let to_float alg =
  let lo = R.to_float (Interval.lo alg.interval) in
  let hi = R.to_float (Interval.hi alg.interval) in
  (lo +. hi) /. 2.0

(** The degree of the algebraic number (degree of its minimal polynomial). *)
let degree alg = P.degree alg.minpoly

let to_string alg =
  Printf.sprintf "AlgNum(%s, %s)"
    (P.to_string R.to_string alg.minpoly)
    (Interval.to_string alg.interval)
