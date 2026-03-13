/// Roots of unity and related utilities.
///
/// Provides all_cos_of_unity and all_sin_of_unity which compute
/// cos(2*k*pi/n) and sin(2*k*pi/n) for all k = 0, ..., n-1.
module Surd.RootOfUnity

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Positive
open Surd.PrimeFactors
open Surd.TrigGalois

/// Dv-compatible list map.
val map_dv : #a:Type -> #b:Type -> (a -> Dv b) -> list a -> Dv (list b)
let rec map_dv #a #b f xs =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map_dv f rest

/// Compute cos(k*theta) using Chebyshev polynomials.
/// T_0(x) = 1, T_1(x) = x, T_{k+1}(x) = 2*x*T_k(x) - T_{k-1}(x).
val chebyshev_cos : nat -> rad_expr rational -> Dv (rad_expr rational)
let rec chebyshev_cos k base =
  if k = 0 then Lit #rational rat_one
  else if k = 1 then base
  else
    let t_prev = chebyshev_cos (k - 2) base in
    let t_curr = chebyshev_cos (k - 1) base in
    let two = Lit #rational (rat_of_int 2) in
    Add (Mul two (Mul base t_curr)) (Neg t_prev)

/// Compute all cos(2*k*pi/n) for k = 0, ..., n-1.
val all_cos_of_unity : n:positive -> Dv (list (int & rad_expr rational))
let all_cos_of_unity n =
  if n = 1 then [(0, Lit #rational rat_one)]
  else if n = 2 then [(0, Lit #rational rat_one); (1, Lit #rational (rat_neg rat_one))]
  else
    let base_cos = cos_of_unity n in
    let rec go (k: nat) : Dv (list (int & rad_expr rational)) =
      if k >= n then []
      else
        let cos_k = chebyshev_cos k base_cos in
        (k, cos_k) :: go (k + 1)
    in
    go 0

/// Compute sin from cos using sin(theta) = sqrt(1 - cos^2(theta))
/// with sign determined by quadrant.
val sin_from_cos : nat -> nat -> rad_expr rational -> Dv (rad_expr rational)
let sin_from_cos k n cos_val =
  let one = Lit #rational rat_one in
  let cos_sq = Pow cos_val 2 in
  let sin_sq = Add one (Neg cos_sq) in
  let sin_abs = Root 2 sin_sq in
  if op_Multiply 2 k <= n then sin_abs
  else Neg sin_abs

/// Compute all sin(2*k*pi/n) for k = 0, ..., n-1.
val all_sin_of_unity : n:positive -> Dv (list (int & rad_expr rational))
let all_sin_of_unity n =
  if n = 1 then [(0, Lit #rational rat_zero)]
  else if n = 2 then [(0, Lit #rational rat_zero); (1, Lit #rational rat_zero)]
  else
    let cos_vals = all_cos_of_unity n in
    map_dv (fun ((k, c) : int & rad_expr rational) ->
      (k, sin_from_cos (if k >= 0 then k else 0) n c))
    cos_vals

/// Compute a single cos(2*k*pi/n) value.
val cos_k_of_n : k:nat -> n:positive -> Dv (rad_expr rational)
let cos_k_of_n k n =
  let base = cos_of_unity n in
  chebyshev_cos (k % n) base

/// Compute a single sin(2*k*pi/n) value.
val sin_k_of_n : k:nat -> n:positive -> Dv (rad_expr rational)
let sin_k_of_n k n =
  let c = cos_k_of_n k n in
  sin_from_cos (k % n) n c
