/// Sturm sequence-based real root isolation for polynomials over Q.
///
/// Given a square-free polynomial, computes isolating intervals for
/// each real root using Sturm's theorem.
module Surd.RootIsolation

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.Interval
open Surd.RootBound

/// Compute the Sturm sequence of a polynomial.
/// f_0 = f, f_1 = f', f_{i+1} = -rem(f_{i-1}, f_i)
val sturm_sequence : poly rational -> Dv (list (poly rational))
let sturm_sequence f =
  let rng : ring rational = ring_rational in
  let f' = diff_poly #rational #rng f in
  let rec go (a b : poly rational) (fuel: nat) : Dv (list (poly rational)) =
    if fuel = 0 then [a]
    else
      match b with
      | [] -> [a]
      | _ ->
        let (_, r) = div_mod_poly #rational #field_rational a b in
        let neg_r = neg_poly #rational #rng r in
        a :: go b neg_r (fuel - 1)
  in
  go f f' 100

/// Count sign changes in a sequence of rationals (ignoring zeros).
let sign_changes (vals: list rational) : nat =
  let nonzero = filter (fun r -> not (rat_eq r rat_zero)) vals in
  let signs = map (fun r -> if rat_gt r rat_zero then 1 else 0 - 1) nonzero in
  let rec count_changes (xs: list int) : Tot nat (decreases xs) =
    match xs with
    | [] -> 0
    | [_] -> 0
    | a :: b :: rest ->
      (if op_Multiply a b < 0 then 1 else 0) + count_changes (b :: rest)
  in
  count_changes signs

/// Evaluate the Sturm sequence at a rational point.
let eval_sturm_at (seq: list (poly rational)) (x: rational) : list rational =
  map (fun p -> eval_poly #rational #ring_rational p x) seq

/// Count the number of real roots in an open interval (a, b) using Sturm's theorem.
/// V(a) - V(b) where V(x) is the number of sign changes in the Sturm sequence at x.
let count_roots_in (seq: list (poly rational)) (a b: rational) : int =
  let va = sign_changes (eval_sturm_at seq a) in
  let vb = sign_changes (eval_sturm_at seq b) in
  va - vb

/// Isolate all real roots of a square-free polynomial into disjoint intervals.
/// Returns a list of intervals, each containing exactly one root.
val isolate_roots : poly rational -> Dv (list interval)
let isolate_roots f =
  match f with
  | [] -> []
  | _ ->
    let seq = sturm_sequence f in
    let bound = real_root_bound f in
    let lo = rat_neg bound in
    let hi = bound in
    let n_roots = count_roots_in seq lo hi in
    if n_roots <= 0 then []
    else
      (* Bisect to isolate each root *)
      let rec bisect_isolate (lo hi: rational) (n: int) (fuel: nat)
        : Dv (list interval) =
        if fuel = 0 || n <= 0 then []
        else if n = 1 then [{ lo = lo; hi = hi }]
        else
          let mid = rat_div_total (rat_add lo hi) (rat_of_int 2) in
          let n_left = count_roots_in seq lo mid in
          let n_right = count_roots_in seq mid hi in
          (* Check if mid is a root *)
          let at_mid = eval_poly #rational #ring_rational f mid in
          let mid_roots = if rat_eq at_mid rat_zero then 1 else 0 in
          let left_intervals = bisect_isolate lo mid n_left (fuel - 1) in
          let mid_intervals = if mid_roots > 0 then [{ lo = mid; hi = mid }] else [] in
          let right_intervals = bisect_isolate mid hi n_right (fuel - 1) in
          left_intervals @ mid_intervals @ right_intervals
      in
      bisect_isolate lo hi n_roots 200

/// Refine an isolating interval to have width < epsilon.
val refine_root : poly rational -> interval -> rational -> Dv interval
let rec refine_root f iv eps =
  let w = width iv in
  if rat_le w eps then iv
  else
    let mid = midpoint iv in
    let val_lo = eval_poly #rational #ring_rational f iv.lo in
    let val_mid = eval_poly #rational #ring_rational f mid in
    (* Bisect based on sign change *)
    if op_Multiply val_lo.num val_mid.num <= 0 then
      refine_root f { lo = iv.lo; hi = mid } eps
    else
      refine_root f { lo = mid; hi = iv.hi } eps

/// Count total number of real roots (with multiplicity handled by square-free).
val count_real_roots : poly rational -> Dv nat
let count_real_roots f =
  length (isolate_roots f)
