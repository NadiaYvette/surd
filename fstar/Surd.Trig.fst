/// Exact trigonometric values at rational multiples of pi.
///
/// cos_exact(p/q * pi) and sin_exact(p/q * pi) return radical expressions
/// over Q giving the exact value.
module Surd.Trig

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Positive
open Surd.PrimeFactors
open Surd.Normalize
open Surd.RootOfUnity
open Surd.TrigGalois

/// Reduce p/q to the range [0, 2) and track sign/reflection.
///
/// cos(x) has period 2*pi and symmetry cos(pi + x) = -cos(x),
/// cos(pi - x) = -cos(x), cos(2*pi - x) = cos(x).
///
/// Returns (numerator, denominator, negate_result) such that
/// cos(p/q * pi) = (+/-)cos(num/den * pi) where num/den in [0, 1/2].
let reduce_angle (p q: int) : (nat & positive & bool) =
  (* Normalize to [0, 2q) *)
  let q_pos : positive = if q > 0 then q else 1 in
  let p_mod = ((p % (op_Multiply 2 q_pos)) + op_Multiply 2 q_pos) % (op_Multiply 2 q_pos) in
  (* p_mod in [0, 2q) represents angle p_mod/(2q) * 2pi = p_mod/q * pi *)
  if p_mod = 0 then (0, q_pos, false)           (* cos(0) = 1 *)
  else if p_mod <= q_pos / 2 then
    (p_mod, q_pos, false)                        (* [0, pi/2]: direct *)
  else if p_mod <= q_pos then
    (q_pos - p_mod, q_pos, true)                 (* (pi/2, pi]: cos(pi-x) = -cos(x) *)
  else if p_mod <= q_pos + q_pos / 2 then
    (p_mod - q_pos, q_pos, true)                 (* (pi, 3pi/2]: cos(pi+x) = -cos(x) *)
  else
    (op_Multiply 2 q_pos - p_mod, q_pos, false)  (* (3pi/2, 2pi): cos(2pi-x) = cos(x) *)

/// Table of exact cosine values for common angles.
let cos_table (p q: nat) : option (rad_expr rational) =
  if q = 0 then None
  else
    let g = Surd.Rational.gcd_nat p q in
    let p' = p / (if g > 0 then g else 1) in
    let q' = q / (if g > 0 then g else 1) in
    (* cos(p'/q' * pi) *)
    if p' = 0 then Some (Lit rat_one)                (* cos(0) = 1 *)
    else if p' = 1 && q' = 1 then Some (Lit (rat_neg rat_one))  (* cos(pi) = -1 *)
    else if p' = 1 && q' = 2 then Some (Lit rat_zero)           (* cos(pi/2) = 0 *)
    else if p' = 1 && q' = 3 then Some (Lit (mk_rational 1 2))  (* cos(pi/3) = 1/2 *)
    else if p' = 1 && q' = 4 then
      Some (Mul (Lit (mk_rational 1 2)) (Root 2 (Lit (rat_of_int 2))))  (* cos(pi/4) = sqrt(2)/2 *)
    else if p' = 1 && q' = 6 then
      Some (Mul (Lit (mk_rational 1 2)) (Root 2 (Lit (rat_of_int 3))))  (* cos(pi/6) = sqrt(3)/2 *)
    else None

/// Compute cos(p/q * pi) as an exact radical expression.
///
/// Strategy:
/// 1. Reduce the angle to [0, pi/2].
/// 2. Check the table for common values.
/// 3. For cos(pi/n), compute cos(2*pi/(2n)) using Gauss periods.
/// 4. For cos(k*pi/n), use Chebyshev.
val cos_exact : int -> positive -> Dv (rad_expr rational)
let cos_exact p q =
  let (p', q', negate) = reduce_angle p q in
  (* cos(p'/q' * pi) = cos(2*p'*pi / (2*q')) = cos(2*p'/(2*q') * pi) *)
  let result =
    match cos_table p' q' with
    | Some e -> e
    | None ->
      (* cos(p'/q' * pi) = cos(2*p' * pi / (2*q'))
         This is cos(2*k*pi/n) where k = p', n = 2*q' *)
      let n = op_Multiply 2 q' in
      assume (n > 0);
      cos_k_of_n p' n
  in
  if negate then Neg result
  else result

/// Compute sin(p/q * pi) as an exact radical expression.
///
/// Uses sin(x) = cos(pi/2 - x).
val sin_exact : int -> positive -> Dv (rad_expr rational)
let sin_exact p q =
  (* sin(p/q * pi) = cos(pi/2 - p/q * pi) = cos((q - 2p)/(2q) * pi) *)
  let new_p = q - op_Multiply 2 p in
  let new_q = op_Multiply 2 q in
  assume (new_q > 0);
  cos_exact new_p new_q

/// Compute tan(p/q * pi) = sin(p/q * pi) / cos(p/q * pi).
val tan_exact : int -> positive -> Dv (rad_expr rational)
let tan_exact p q =
  let s = sin_exact p q in
  let c = cos_exact p q in
  Mul s (Inv c)

/// Convenience: cos_exact with a rational argument (p/q * pi).
val cos_exact_rat : rational -> Dv (rad_expr rational)
let cos_exact_rat r =
  assume (r.den > 0);
  cos_exact r.num r.den

/// Convenience: sin_exact with a rational argument.
val sin_exact_rat : rational -> Dv (rad_expr rational)
let sin_exact_rat r =
  assume (r.den > 0);
  sin_exact r.num r.den
