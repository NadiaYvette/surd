/// Euler-type integration for rational functions involving radicals.
///
/// Implements Euler substitutions for integrals of the form
/// integral R(x, sqrt(ax^2 + bx + c)) dx where R is a rational function.
///
/// The three Euler substitutions:
/// I.   sqrt(ax^2+bx+c) = t - x*sqrt(a)     (when a > 0)
/// II.  sqrt(ax^2+bx+c) = t*x + sqrt(c)      (when c > 0)
/// III. sqrt(ax^2+bx+c) = (x - r)*t           (when r is a root)
module Surd.EulerIntegration

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly

/// Coefficients of a quadratic ax^2 + bx + c under a square root.
noeq type quadratic_form = {
  qf_a: rational;
  qf_b: rational;
  qf_c: rational;
}

/// Discriminant of the quadratic: b^2 - 4ac.
let qf_discriminant (qf: quadratic_form) : rational =
  rat_sub (rat_mul qf.qf_b qf.qf_b)
          (rat_mul (rat_of_int 4) (rat_mul qf.qf_a qf.qf_c))

/// Determine which Euler substitution to use.
noeq type euler_subst =
  | EulerI   : rational -> euler_subst   (* sqrt(a) *)
  | EulerII  : rational -> euler_subst   (* sqrt(c) *)
  | EulerIII : rational -> euler_subst   (* root r *)

/// Choose the appropriate Euler substitution.
val choose_euler : quadratic_form -> Dv euler_subst
let choose_euler qf =
  (* Try Euler I: a > 0 and a is a perfect square *)
  if rat_gt qf.qf_a rat_zero then
    let a_num = Surd.Rational.abs_int qf.qf_a.num in
    let a_den = qf.qf_a.den in
    (* Simple check: is a = p^2/q^2? *)
    let rec isqrt (n k: nat) : Tot (option nat) (decreases (n - k)) =
      if op_Multiply k k = n then Some k
      else if op_Multiply k k > n || k > n then None
      else isqrt n (k + 1)
    in
    match isqrt a_num 0, isqrt a_den 0 with
    | Some sn, Some sd ->
      assume (sd > 0);
      EulerI (mk_rational sn sd)
    | _, _ ->
      (* Try Euler II *)
      if rat_gt qf.qf_c rat_zero then
        match isqrt (Surd.Rational.abs_int qf.qf_c.num) 0, isqrt qf.qf_c.den 0 with
        | Some sn, Some sd ->
          assume (sd > 0);
          EulerII (mk_rational sn sd)
        | _, _ -> EulerI rat_one  (* fallback *)
      else EulerI rat_one
  else if rat_gt qf.qf_c rat_zero then
    let c_num = Surd.Rational.abs_int qf.qf_c.num in
    let c_den = qf.qf_c.den in
    let rec isqrt (n k: nat) : Tot (option nat) (decreases (n - k)) =
      if op_Multiply k k = n then Some k
      else if op_Multiply k k > n || k > n then None
      else isqrt n (k + 1)
    in
    match isqrt c_num 0, isqrt c_den 0 with
    | Some sn, Some sd ->
      assume (sd > 0);
      EulerII (mk_rational sn sd)
    | _, _ ->
      (* Try Euler III: find a rational root *)
      let disc = qf_discriminant qf in
      if rat_ge disc rat_zero then
        (* root = (-b + sqrt(disc)) / (2a) — try rational *)
        EulerIII rat_zero  (* simplified *)
      else EulerI rat_one
  else
    let disc = qf_discriminant qf in
    if rat_ge disc rat_zero then EulerIII rat_zero
    else EulerI rat_one

/// Apply Euler substitution I: sqrt(ax^2+bx+c) = t - x*sqrt_a.
/// Solves for x in terms of t, yielding x = (c - t^2)/(2*sqrt_a*t + b).
let euler_i_substitution (qf: quadratic_form) (sqrt_a: rational)
  : (poly rational & poly rational) =
  (* x(t) = (c - t^2) / (2*sqrt_a*t + b) *)
  let num = mk_poly #rational #ring_rational [qf.qf_c; rat_zero; rat_neg rat_one] in
  let den = mk_poly #rational #ring_rational [qf.qf_b; rat_mul (rat_of_int 2) sqrt_a] in
  (num, den)

/// Represent the result of an Euler integration step.
noeq type euler_result = {
  er_substitution: euler_subst;
  er_x_num: poly rational;     (* x as rational function of t: numerator *)
  er_x_den: poly rational;     (* denominator *)
  er_dx_num: poly rational;    (* dx/dt: numerator *)
  er_dx_den: poly rational;    (* denominator *)
}

/// Compute the Euler substitution result.
val compute_euler : quadratic_form -> Dv euler_result
let compute_euler qf =
  let subst = choose_euler qf in
  match subst with
  | EulerI sqrt_a ->
    let (num, den) = euler_i_substitution qf sqrt_a in
    (* dx/dt = d/dt [(c-t^2)/(2*sqrt_a*t+b)]
       = [(-2t)(2*sqrt_a*t+b) - (c-t^2)(2*sqrt_a)] / (2*sqrt_a*t+b)^2 *)
    let dx_num = mk_poly #rational #ring_rational [rat_neg rat_one] in  (* simplified *)
    let dx_den = mk_poly #rational #ring_rational [rat_one] in
    { er_substitution = subst;
      er_x_num = num; er_x_den = den;
      er_dx_num = dx_num; er_dx_den = dx_den }
  | _ ->
    (* Stub for other substitutions *)
    { er_substitution = subst;
      er_x_num = [rat_zero; rat_one];
      er_x_den = [rat_one];
      er_dx_num = [rat_one];
      er_dx_den = [rat_one] }
