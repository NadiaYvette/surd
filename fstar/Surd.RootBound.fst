/// Root bounds for univariate polynomials over Q.
///
/// Provides Cauchy bound and Lagrange bound for isolating roots
/// within intervals.
module Surd.RootBound

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly

/// Helper: max of absolute ratios |c/lc|.
let max_abs_ratio (lc_abs: rational) (cs: list rational) : rational =
  fold_left (fun (acc: rational) (c: rational) ->
    rat_max acc (rat_div_total (rat_abs c) lc_abs)) rat_zero cs

/// Helper: sum of absolute ratios |c/lc|.
let sum_abs_ratio (lc_abs: rational) (cs: list rational) : rational =
  fold_left (fun (acc: rational) (c: rational) ->
    rat_add acc (rat_div_total (rat_abs c) lc_abs)) rat_zero cs

/// Cauchy bound: all roots of p(x) satisfy |x| <= 1 + max(|a_i/a_n|).
let cauchy_bound (p: poly rational) : rational =
  match rev p with
  | [] -> rat_zero
  | lc :: rest ->
    if rat_eq lc rat_zero then rat_of_int 1000
    else rat_add rat_one (max_abs_ratio (rat_abs lc) rest)

/// Lagrange bound: |root| <= max(1, sum(|a_i/a_n|)).
let lagrange_bound (p: poly rational) : rational =
  match rev p with
  | [] -> rat_zero
  | lc :: rest ->
    if rat_eq lc rat_zero then rat_of_int 1000
    else rat_max rat_one (sum_abs_ratio (rat_abs lc) rest)

/// Positive root bound.
let positive_root_bound (p: poly rational) : rational = cauchy_bound p

/// Negate the variable: p(-x).
let negate_var_q (p: poly rational) : poly rational =
  let rec go (cs: list rational) (i: nat) : Tot (list rational) (decreases cs) =
    match cs with
    | [] -> []
    | c :: rest ->
      (if i % 2 = 1 then rat_neg c else c) :: go rest (i + 1)
  in
  mk_poly #rational #ring_rational (go p 0)

/// Negative root bound.
let negative_root_bound (p: poly rational) : rational =
  cauchy_bound (negate_var_q p)

/// Overall real root bound.
let real_root_bound (p: poly rational) : rational =
  rat_max (positive_root_bound p) (negative_root_bound p)
