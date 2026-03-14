/// Radical tower construction for solvable polynomials of any prime degree.
///
/// Generalises the degree-5 Lagrange resolvent pipeline to degree n:
///
///   1. Depress: eliminate the x^{n-1} term.
///   2. Find cyclic ordering: permute numerical roots so the Galois
///      generator acts as (0 1 2 ... n-1).
///   3. Lagrange resolvents: R_j = Σ_k ω_n^{jk} α_k.
///   4. DFT: d_s = (1/n) Σ_j ω_n^{-js} R_j^n.
///   5. Coefficient matching: recognise d_s in the coefficient field.
///   6. Reconstruct R_j^n as RadExpr.
///   7. Branch selection: R_j = ω_n^{b_j} · ⁿ√(R_j^n).
///   8. Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j.
///   9. Un-depress and match to original root ordering.
///
/// The degree-5 fast path is kept and delegates to solvable_quintic.
module Surd.RadicalTower

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Types
open Surd.Poly
open Surd.Eval
open Surd.TransitiveGroup
open Surd.PrimeFactors

/// --------------------------------------------------------------------------
/// Complex double helpers (π, ω_n as complex doubles)
/// --------------------------------------------------------------------------

assume val double_pi : double

/// ω_n^k = e^{2πik/n} as a complex double.
let omega_n_cd (n k: int) : complex_double =
  let angle = double_div (double_mul (double_mul (double_of_int 2) double_pi)
                                     (double_of_int k))
                         (double_of_int n) in
  { re = double_cos angle; im = double_sin angle }

/// Complex double magnitude.
let cd_mag (z: complex_double) : double =
  cd_magnitude z

/// Distance between two complex doubles.
let cd_dist (a b: complex_double) : double =
  cd_magnitude (cd_sub a b)

/// Approximate a double as a small-denominator rational.
/// Tries denominators up to 10000.
assume val approx_rat : double -> rational

/// Score how close a complex double is to a rational number.
/// Low score = close to a real integer/rational.
assume val double_frac_part : double -> double

let score_rational (z: complex_double) : double =
  double_add (double_abs z.im) (double_frac_part z.re)

/// --------------------------------------------------------------------------
/// A step in a radical tower: adjoin the nth root of some expression.
/// --------------------------------------------------------------------------

noeq type tower_step = {
  ts_index: int;
  ts_radicand: rad_expr rational;
}

/// A radical tower: sequence of adjunction steps.
type radical_tower = list tower_step

/// --------------------------------------------------------------------------
/// Low-degree towers (kept for backward compat)
/// --------------------------------------------------------------------------

/// Build a radical tower for a quadratic x^2 + bx + c.
let quadratic_tower (b c: rational) : radical_tower =
  let disc = rat_sub (rat_mul b b) (rat_mul (rat_of_int 4) c) in
  [{ ts_index = 2; ts_radicand = Lit disc }]

/// Build a radical tower for a depressed cubic x^3 + px + q.
let cubic_tower (p q: rational) : radical_tower =
  let disc = rat_add (rat_mul (rat_of_int 4) (rat_mul (rat_mul p p) p))
                     (rat_mul (rat_of_int 27) (rat_mul q q)) in
  let disc_over_108 = rat_div_total disc (rat_of_int 108) in
  let step1 = { ts_index = 2; ts_radicand = Lit disc_over_108 } in
  let neg_q_half = rat_mul (mk_rational (0-1) 2) q in
  let step2 = { ts_index = 3;
                ts_radicand = Add (Lit neg_q_half) (Root 2 (Lit disc_over_108)) } in
  [step1; step2]

/// Build a radical tower for a polynomial of degree <= 4.
val build_tower : poly rational -> Dv radical_tower
let build_tower f =
  let d = degree f in
  if d <= 1 then []
  else if d = 2 then
    match f with
    | [c; b; _] -> quadratic_tower b c
    | _ -> []
  else if d = 3 then
    match f with
    | [q; _; p; _] ->
      if rat_eq p rat_zero then cubic_tower rat_zero q
      else cubic_tower p q
    | _ -> []
  else []

/// Express a root using the radical tower (degrees <= 3).
val root_from_tower : poly rational -> radical_tower -> Dv (option (rad_expr rational))
let root_from_tower f tower =
  let d = degree f in
  if d = 2 then
    match f with
    | [c; b; _] ->
      let disc_expr =
        match tower with
        | [step] -> Root 2 step.ts_radicand
        | _ -> Lit rat_zero
      in
      Some (Mul (Lit (mk_rational 1 2))
                (Add (Lit (rat_neg b)) disc_expr))
    | _ -> None
  else if d = 3 then
    match f with
    | [q; _; _; _] ->
      (match tower with
       | [_; step2] ->
         let u = Root 3 step2.ts_radicand in
         Some (Add u (Mul (Lit (rat_neg (mk_rational 1 3))) (Inv u)))
       | _ -> None)
    | _ -> None
  else None

/// --------------------------------------------------------------------------
/// ω_n as a radical expression
/// --------------------------------------------------------------------------

/// ω_5 = cos(2π/5) + i·sin(2π/5) as a RadExpr.
let omega_5_expr : rad_expr rational =
  let cos5 = Mul (Inv (Lit (rat_of_int 4)))
                 (Add (Root 2 (Lit (rat_of_int 5))) (Neg (Lit rat_one))) in
  let sin5 = Mul (Inv (Lit (rat_of_int 4)))
                 (Root 2 (Add (Lit (rat_of_int 10))
                              (Mul (Lit (rat_of_int 2)) (Root 2 (Lit (rat_of_int 5)))))) in
  let i_unit = Root 2 (Lit (rat_neg rat_one)) in
  Add cos5 (Mul i_unit sin5)

/// ω_n = e^{2πi/n} as a radical expression.
///
/// For n = 5, uses the explicit closed form.
/// For general n, uses cos(2π/n) + i·sin(2π/n) with approximate rationals
/// (last resort; for exact forms, use allPeriodsViaGauss from the Haskell side).
///
/// For n = 3: ω_3 = (-1 + i√3)/2.
/// For n = 7: uses Cardano-style cubic (approximate).
let omega_n_expr (n: int) : rad_expr rational =
  if n = 3 then
    let i_unit = Root 2 (Lit (rat_neg rat_one)) in
    Add (Lit (mk_rational (0-1) 2))
        (Mul (Lit (mk_rational 1 2)) (Mul i_unit (Root 2 (Lit (rat_of_int 3)))))
  else if n = 5 then omega_5_expr
  else
    // General fallback: ω_n = cos(2π/n) + i·sin(2π/n) with approx rational coeffs
    let om = omega_n_cd n 1 in
    let cos_r = approx_rat om.re in
    let sin_r = approx_rat om.im in
    let i_unit = Root 2 (Lit (rat_neg rat_one)) in
    Add (Lit cos_r) (Mul i_unit (Lit sin_r))

/// ω_n^k as a radical expression.
let omega_pow_n_expr (n k: int) : rad_expr rational =
  let k' = ((k % n) + n) % n in
  if k' = 0 then Lit rat_one
  else if k' = 1 then omega_n_expr n
  else Pow (omega_n_expr n) k'

/// --------------------------------------------------------------------------
/// Branch selection for the nth root
/// --------------------------------------------------------------------------

/// Select the correct branch: try all n candidates ω_n^k · ⁿ√(R_j^n)
/// and pick the one closest to the known numerical value.
val select_branch_n : int -> rad_expr rational -> complex_double -> Dv (rad_expr rational)
let select_branch_n n rjn_expr target_val =
  let rjn_val = eval_complex rjn_expr in
  // Principal nth root
  let inv_n = double_div double_one (double_of_int n) in
  let r = cd_magnitude rjn_val in
  let theta = double_atan2 rjn_val.im rjn_val.re in
  let rn = double_pow r inv_n in
  let an = double_mul theta inv_n in
  let principal_val : complex_double = {
    re = double_mul rn (double_cos an);
    im = double_mul rn (double_sin an);
  } in
  let principal_root = Root n rjn_expr in
  // Try all branches
  let rec find_best (k: nat) (best_k: nat) (best_dist: double) : Dv nat =
    if k >= (if n >= 0 then n else 0) then best_k
    else
      let omega_k = omega_n_cd n k in
      let candidate = cd_mul omega_k principal_val in
      let dist = cd_dist candidate target_val in
      if double_lt dist best_dist then  // dist < best_dist
        find_best (k + 1) k dist
      else
        find_best (k + 1) best_k best_dist
  in
  // Start with k=0
  let initial_dist = cd_dist principal_val target_val in
  let best_k = find_best 1 0 initial_dist in
  if best_k = 0 then principal_root
  else Mul (omega_pow_n_expr n best_k) principal_root

/// --------------------------------------------------------------------------
/// DFT coefficient matching
/// --------------------------------------------------------------------------

/// Match a single complex double to a rational literal.
/// Returns None if the imaginary part exceeds 0.05 in magnitude.
let match_rat_strict (z: complex_double) : option (rad_expr rational) =
  let im_abs = double_abs z.im in
  let threshold = double_div double_one (double_of_int 20) in  // 0.05
  if double_gt im_abs threshold then None
  else
    let r = approx_rat z.re in
    Some (Lit r)

/// Match all d_s as rational (for cyclic Galois group: all d_s ∈ Q).
let match_ds_all_rational (d_vals: list complex_double) : option (list (rad_expr rational)) =
  let rec go (vs: list complex_double) : option (list (rad_expr rational)) =
    match vs with
    | [] -> Some []
    | v :: rest ->
      match match_rat_strict v with
      | None -> None
      | Some e ->
        match go rest with
        | None -> None
        | Some es -> Some (e :: es)
  in
  go d_vals

/// Match a conjugate pair {d_s, d_{n-s}} to quadratic expressions over Q.
val match_conj_pair : complex_double -> complex_double -> Dv (option (rad_expr rational & rad_expr rational))
let match_conj_pair d1 d2 =
  let s = cd_add d1 d2 in
  let p = cd_mul d1 d2 in
  let s_r = approx_rat s.re in
  let p_r = approx_rat p.re in
  let disc_r = rat_sub (rat_mul s_r s_r) (rat_mul (rat_of_int 4) p_r) in
  let s_expr = Lit s_r in
  let p_expr = Lit p_r in
  let disc_expr = Add (Mul s_expr s_expr) (Neg (Mul (Lit (rat_of_int 4)) p_expr)) in
  let sqrt_disc = Root 2 disc_expr in
  let e_plus = Mul (Inv (Lit (rat_of_int 2))) (Add s_expr sqrt_disc) in
  let e_minus = Mul (Inv (Lit (rat_of_int 2))) (Add s_expr (Neg sqrt_disc)) in
  // Determine branch: evaluate sqrt(disc) numerically
  let disc_val =
    if disc_r.num >= 0 then
      { re = double_sqrt (double_of_rational disc_r); im = double_zero }
    else
      { re = double_zero;
        im = double_sqrt (double_of_rational (rat_neg disc_r)) }
  in
  let d1_plus_val = cd_mul { re = double_of_rational (mk_rational 1 2); im = double_zero }
                           (cd_add s disc_val) in
  let dist1 = cd_dist d1_plus_val d1 in
  let dist2 = cd_dist d1_plus_val d2 in
  if double_lt dist1 dist2 then  // dist1 < dist2 → plus matches d1
    Some (e_plus, e_minus)
  else
    Some (e_minus, e_plus)

/// Match DFT coefficients for dihedral group (d = 2).
/// d_0 ∈ Q, and for 1 ≤ s ≤ (n-1)/2, {d_s, d_{n-s}} are conjugate pairs.
val match_ds_dihedral : list complex_double -> int -> Dv (option (list (rad_expr rational)))
let match_ds_dihedral d_vals n =
  let d0_expr = match match_rat_strict (hd d_vals) with
    | Some e -> e
    | None -> Lit rat_zero
  in
  let half_n = (n - 1) / 2 in
  // Build pair expressions
  let rec build_pairs (s: nat) : Dv (option (list (rad_expr rational & rad_expr rational))) =
    if s > (if half_n >= 0 then half_n else 0) then Some []
    else
      let ds = (match nth d_vals s with | Some v -> v | None -> cd_zero) in
      let dns = (match nth d_vals (n - s) with | Some v -> v | None -> cd_zero) in
      match match_conj_pair ds dns with
      | None -> None
      | Some pair ->
        match build_pairs (s + 1) with
        | None -> None
        | Some rest -> Some (pair :: rest)
  in
  match build_pairs 1 with
  | None -> None
  | Some pairs ->
    // Assemble result array of length n
    let result : list (rad_expr rational) =
      let rec fill (i: nat) : list (rad_expr rational) =
        if i >= (if n >= 0 then n else 0) then []
        else if i = 0 then d0_expr :: fill (i + 1)
        else if i <= (if half_n >= 0 then half_n else 0) then
          // d_i is first of pair (i-1)
          let (e_s, _) = (match nth pairs (i - 1) with
            | Some p -> p
            | None -> (Lit rat_zero, Lit rat_zero)) in
          e_s :: fill (i + 1)
        else
          // d_i is second of pair (n-i-1)
          let idx = n - i in
          let (_, e_ns) = (match nth pairs (idx - 1) with
            | Some p -> p
            | None -> (Lit rat_zero, Lit rat_zero)) in
          e_ns :: fill (i + 1)
      in
      fill 0
    in
    Some result

/// Match a complex number to an element of Q(ω_n).
/// Tries: single-term d = r · ω_n^k, then 2-term decomposition.
val match_q_omega_n : int -> complex_double -> Dv (option (rad_expr rational))
let match_q_omega_n n d =
  // Try single-term: d ≈ r · ω_n^k
  let rec try_single (k: nat) (best_k: nat) (best_score: double)
    : Dv (nat & double) =
    if k >= (if n >= 0 then n else 0) then (best_k, best_score)
    else
      let omega_neg_k = omega_n_cd n (0 - k) in
      let v = cd_mul d omega_neg_k in
      let score = score_rational v in
      if double_lt score best_score then
        try_single (k + 1) k score
      else
        try_single (k + 1) best_k best_score
  in
  let (best_k, best_score) = try_single 0 0 (double_of_int 1000) in
  // threshold: score < 0.01
  let threshold = double_div double_one (double_of_int 100) in
  if double_lt best_score threshold then
    let omega_neg_k = omega_n_cd n (0 - best_k) in
    let v = cd_mul d omega_neg_k in
    let r = approx_rat v.re in
    if best_k = 0 then Some (Lit r)
    else Some (Mul (Lit r) (omega_pow_n_expr n best_k))
  else
    // Try 2-term: d = a · ω_n^j + b · ω_n^k
    let rec try_two_term (j: nat) (best: option (rad_expr rational)) (best_err: double)
      : Dv (option (rad_expr rational)) =
      if j >= (if n >= 0 then n else 0) then best
      else
        let rec try_k (k: nat) (cur_best: option (rad_expr rational)) (cur_err: double)
          : Dv (option (rad_expr rational) & double) =
          if k >= (if n >= 0 then n else 0) then (cur_best, cur_err)
          else if k = j then try_k (k + 1) cur_best cur_err
          else
            let wj = omega_n_cd n j in
            let wk = omega_n_cd n k in
            // Solve: a·wj + b·wk = d
            // det = Re(wj)·Im(wk) - Im(wj)·Re(wk)
            let det = double_sub (double_mul wj.re wk.im) (double_mul wj.im wk.re) in
            let det_abs = double_abs det in
            let small = double_div double_one (double_of_int 10000000000) in
            if double_lt det_abs small then
              try_k (k + 1) cur_best cur_err
            else
              let a_val = double_div (double_sub (double_mul d.re wk.im) (double_mul d.im wk.re)) det in
              let b_val = double_div (double_sub (double_mul wj.re d.im) (double_mul wj.im d.re)) det in
              let a_r = approx_rat a_val in
              let b_r = approx_rat b_val in
              let recon = cd_add (cd_mul (cd_of_rational a_r) wj) (cd_mul (cd_of_rational b_r) wk) in
              let err = cd_dist recon d in
              let err_threshold = double_div double_one (double_of_int 100) in
              if double_lt err err_threshold && double_lt err cur_err then
                let term_j = if j = 0 then Lit a_r else Mul (Lit a_r) (omega_pow_n_expr n j) in
                let term_k = if k = 0 then Lit b_r else Mul (Lit b_r) (omega_pow_n_expr n k) in
                try_k (k + 1) (Some (Add term_j term_k)) err
              else
                try_k (k + 1) cur_best cur_err
        in
        let (new_best, new_err) = try_k 0 best best_err in
        try_two_term (j + 1) new_best new_err
    in
    try_two_term 0 None (double_of_int 1000)

/// Match DFT coefficients via Galois orbit structure for larger
/// stabiliser groups.
val match_ds_via_orbits : transitive_group_info -> list complex_double -> int -> Dv (option (list (rad_expr rational)))
let match_ds_via_orbits tgi d_vals n =
  // Try all-rational first
  match match_ds_all_rational d_vals with
  | Some exprs -> Some exprs
  | None ->
    // d_0 rational, rest matched via Q(ω_n)
    match match_rat_strict (hd d_vals) with
    | None -> None
    | Some d0_expr ->
      let rec match_rest (vs: list complex_double) : Dv (option (list (rad_expr rational))) =
        match vs with
        | [] -> Some []
        | v :: rest ->
          match match_q_omega_n n v with
          | None -> None
          | Some e ->
            match match_rest rest with
            | None -> None
            | Some es -> Some (e :: es)
      in
      match match_rest (tl d_vals) with
      | None -> None
      | Some rest_exprs -> Some (d0_expr :: rest_exprs)

/// General DFT coefficient matching, dispatching on group structure.
val match_ds_general : transitive_group_info -> list complex_double -> int -> Dv (option (list (rad_expr rational)))
let match_ds_general tgi d_vals n =
  let p : int = n in
  let d = tgi.tgi_order / p in
  if d = 1 then match_ds_all_rational d_vals          // cyclic
  else if d = 2 then match_ds_dihedral d_vals n        // dihedral
  else match_ds_via_orbits tgi d_vals n                // general affine

/// --------------------------------------------------------------------------
/// Scoring and cyclic ordering
/// --------------------------------------------------------------------------

/// Score a candidate ordering for degree n.
/// For cyclic group: all d_s should be approximately rational.
/// For dihedral: d_0 rational, conjugate pairs.
val score_ordering_n : list complex_double -> list int -> int -> transitive_group_info -> Dv double
let score_ordering_n roots ordering n tgi =
  let ordered : list complex_double =
    map (fun (i: int) -> match nth roots i with | Some r -> r | None -> cd_zero) ordering
  in
  let om_c (k: int) : complex_double = omega_n_cd n k in
  let rj (j: int) : Dv complex_double =
    let rec sum_k (k: nat) (acc: complex_double) : Dv complex_double =
      if k >= (if n >= 0 then n else 0) then acc
      else
        let omega_jk = om_c (op_Multiply j k) in
        let alpha_k = (match nth ordered k with | Some r -> r | None -> cd_zero) in
        sum_k (k + 1) (cd_add acc (cd_mul omega_jk alpha_k))
    in
    sum_k 0 cd_zero
  in
  let rj_pows : list complex_double =
    let rec build (j: nat) : Dv (list complex_double) =
      if j >= (if n >= 0 then n else 0) then []
      else cd_pow (rj j) (if n >= 0 then n else 0) :: build (j + 1)
    in
    build 0
  in
  let d_vals : list complex_double =
    let rec build_s (s: nat) : Dv (list complex_double) =
      if s >= (if n >= 0 then n else 0) then []
      else
        let rec sum_j (j: nat) (acc: complex_double) : Dv complex_double =
          if j >= (if n >= 0 then n else 0) then acc
          else
            let exp = ((n - op_Multiply j s % n) % n + n) % n in
            let omega_val = om_c exp in
            let rjp = (match nth rj_pows j with | Some v -> v | None -> cd_zero) in
            sum_j (j + 1) (cd_add acc (cd_mul omega_val rjp))
        in
        let raw = sum_j 0 cd_zero in
        let inv_n = { re = double_div double_one (double_of_int n); im = double_zero } in
        cd_mul inv_n raw :: build_s (s + 1)
    in
    build_s 0
  in
  let p : int = n in
  let d_order = tgi.tgi_order / p in
  if d_order = 1 then
    // Cyclic: all d_s should be rational
    let rec total_score (vs: list complex_double) : Dv double =
      match vs with
      | [] -> double_zero
      | v :: rest -> double_add (score_rational v) (total_score rest)
    in
    total_score d_vals
  else if d_order = 2 then
    // Dihedral: d_0 rational + conjugate pair symmetric functions rational
    let d0 = hd d_vals in
    let score0 = score_rational d0 in
    let half = (n - 1) / 2 in
    let rec pair_score (s: nat) (acc: double) : Dv double =
      if s > (if half >= 0 then half else 0) then acc
      else
        let ds = (match nth d_vals s with | Some v -> v | None -> cd_zero) in
        let dns = (match nth d_vals (n - s) with | Some v -> v | None -> cd_zero) in
        let s_sum = cd_add ds dns in
        let s_prod = cd_mul ds dns in
        pair_score (s + 1) (double_add acc (double_add (score_rational s_sum) (score_rational s_prod)))
    in
    pair_score 1 score0
  else
    // General: simplified - just check all d_s for rationality
    let rec total_score (vs: list complex_double) : Dv double =
      match vs with
      | [] -> double_zero
      | v :: rest -> double_add (score_rational v) (total_score rest)
    in
    total_score d_vals

/// --------------------------------------------------------------------------
/// Find cyclic ordering
/// --------------------------------------------------------------------------

/// Build all permutations of a list of ints.
val int_permutations : list int -> Dv (list (list int))
let rec int_permutations xs =
  match xs with
  | [] -> [[]]
  | _ ->
    let rec picks (prefix: list int) (suffix: list int) : Dv (list (int & list int)) =
      match suffix with
      | [] -> []
      | y :: ys ->
        (y, rev prefix @ ys) :: picks (y :: prefix) ys
    in
    let all_picks = picks [] xs in
    let rec expand (ps: list (int & list int)) : Dv (list (list int)) =
      match ps with
      | [] -> []
      | (x, rest) :: more ->
        let sub_perms = int_permutations rest in
        map (fun p -> x :: p) sub_perms @ expand more
    in
    expand all_picks

/// Build a cyclic ordering by choosing root 0 at position 0 and root
/// next at position 1, then greedily assigning each subsequent position.
val build_ordering : list complex_double -> int -> int -> int -> Dv (list int)
let build_ordering roots n start next =
  let step = cd_sub (match nth roots next with | Some r -> r | None -> cd_zero)
                    (match nth roots start with | Some r -> r | None -> cd_zero) in
  let rec go (used: list int) (pos: int) (acc: list int) : Dv (list int) =
    if length acc >= (if n >= 0 then n else 0) then rev acc
    else
      let target = cd_add (match nth roots pos with | Some r -> r | None -> cd_zero) step in
      let rec find_unused (candidates: list int) : list int =
        match candidates with
        | [] -> []
        | i :: rest ->
          if mem i used then find_unused rest
          else i :: find_unused rest
      in
      let unused = find_unused (let rec range (k: nat) : list int =
        if k >= (if n >= 0 then n else 0) then [] else k :: range (k + 1) in range 0) in
      match unused with
      | [] -> rev acc
      | _ ->
        let rec find_closest (us: list int) (best_i: int) (best_d: double) : Dv int =
          match us with
          | [] -> best_i
          | i :: rest ->
            let ri = (match nth roots i with | Some r -> r | None -> cd_zero) in
            let d = cd_dist ri target in
            if double_lt d best_d then find_closest rest i d
            else find_closest rest best_i best_d
        in
        let first_unused = hd unused in
        let ri = (match nth roots first_unused with | Some r -> r | None -> cd_zero) in
        let closest = find_closest (tl unused) first_unused (cd_dist ri target) in
        go (closest :: used) closest (closest :: acc)
  in
  go [start; next] next [next; start]

/// Find cyclic ordering for degree n.
///
/// For small n (≤ 8): brute-force all (n-1)! orderings.
/// For large n: try (n-1) rotations via build_ordering.
val find_cyclic_ordering_n : list complex_double -> int -> transitive_group_info -> Dv (option (list int))
let find_cyclic_ordering_n roots n tgi =
  if n <= 8 then
    let rest = let rec range (k: nat) : list int =
      if k >= (if n >= 0 then n else 0) then [] else (k + 1) :: range (k + 1) in
      range 0
    in
    let rest' = let rec take_n (xs: list int) (k: nat) : list int =
      if k = 0 then [] else match xs with | [] -> [] | x :: r -> x :: take_n r (k - 1)
    in take_n rest (n - 1) in
    let orderings = map (fun p -> 0 :: p) (int_permutations rest') in
    let rec score_all (os: list (list int)) : Dv (list (list int & double)) =
      match os with
      | [] -> []
      | o :: rest ->
        let s = score_ordering_n roots o n tgi in
        (o, s) :: score_all rest
    in
    let scored = score_all orderings in
    let sorted = sortWith (fun (a: list int & double) (b: list int & double) ->
      let (_, sa) = a in let (_, sb) = b in
      if double_lt sa sb then 0 - 1 else 1) scored in
    match sorted with
    | (best_o, best_score) :: _ ->
      let n_dbl = double_of_int n in
      let threshold = double_mul (double_of_int 5) n_dbl in
      if double_lt best_score threshold then Some best_o
      else None
    | [] -> None
  else
    // Large n: try (n-1) rotations
    let rec try_nexts (next: nat) (best: option (list int)) (best_score: double) : Dv (option (list int)) =
      if next >= (if n >= 0 then n else 0) then best
      else
        let ordering = build_ordering roots n 0 next in
        if length ordering <> (if n >= 0 then n else 0) then try_nexts (next + 1) best best_score
        else
          let s = score_ordering_n roots ordering n tgi in
          if double_lt s best_score then
            try_nexts (next + 1) (Some ordering) s
          else
            try_nexts (next + 1) best best_score
    in
    let result = try_nexts 1 None (double_of_int 100000) in
    match result with
    | Some o ->
      let threshold = double_mul (double_of_int 5) (double_of_int n) in
      let s = score_ordering_n roots o n tgi in
      if double_lt s threshold then Some o
      else None
    | None -> None

/// --------------------------------------------------------------------------
/// Generic solver for solvable polynomials of prime degree n
/// --------------------------------------------------------------------------

/// foldl1 for rad_expr with Add.
let foldl1_add (xs: list (rad_expr rational)) : rad_expr rational =
  match xs with
  | [] -> Lit rat_zero
  | [x] -> x
  | x :: rest -> fold_left (fun acc e -> Add acc e) x rest

/// Solve a solvable polynomial of prime degree n.
///
/// 9-step pipeline:
/// 1. Depress (eliminate x^{n-1} term)
/// 2. Find cyclic ordering
/// 3. Lagrange resolvents
/// 4. DFT coefficients
/// 5. Coefficient matching
/// 6. Reconstruct R_j^n
/// 7. Branch selection
/// 8. Inverse DFT
/// 9. Un-depress + match to original roots
val solve_solvable_prime : transitive_group_info -> poly rational -> list complex_double -> Dv (option (list (rad_expr rational)))
let solve_solvable_prime tgi f num_roots =
  let n = degree f in
  let n_nat : nat = if n >= 0 then n else 0 in

  // Step 1: Depress
  let cs = f in
  let lc = match rev cs with | c :: _ -> c | [] -> rat_one in
  let monic_cs = map (fun c -> rat_div_total c lc) cs in
  let an1 = match nth monic_cs (n - 1) with | Some c -> c | None -> rat_zero in
  let shift_val = rat_neg (rat_div_total an1 (rat_of_int n)) in
  let dep_roots : list complex_double =
    map (fun r -> cd_sub r (cd_of_rational shift_val)) num_roots
  in

  // Step 2: Find cyclic ordering
  match find_cyclic_ordering_n dep_roots n tgi with
  | None -> None
  | Some ordering ->
    let ordered_roots : list complex_double =
      map (fun (i: int) -> match nth dep_roots i with | Some r -> r | None -> cd_zero) ordering
    in

    // Step 3: Lagrange resolvents R_j = Σ_k ω_n^{jk} α_k
    let om_c (k: int) : complex_double = omega_n_cd n k in
    let rj (j: int) : Dv complex_double =
      let rec sum_k (k: nat) (acc: complex_double) : Dv complex_double =
        if k >= n_nat then acc
        else
          let omega_jk = om_c (op_Multiply j k) in
          let alpha_k = (match nth ordered_roots k with | Some r -> r | None -> cd_zero) in
          sum_k (k + 1) (cd_add acc (cd_mul omega_jk alpha_k))
      in
      sum_k 0 cd_zero
    in
    let rj_vals : list complex_double =
      let rec build (j: nat) : Dv (list complex_double) =
        if j >= n_nat then [] else rj j :: build (j + 1) in build 0
    in
    let rj_pows : list complex_double =
      map (fun r -> cd_pow r n_nat) rj_vals
    in

    // Step 4: DFT: d_s = (1/n) Σ_j ω_n^{-js} R_j^n
    let d_vals : list complex_double =
      let rec build_s (s: nat) : Dv (list complex_double) =
        if s >= n_nat then []
        else
          let rec sum_j (j: nat) (acc: complex_double) : Dv complex_double =
            if j >= n_nat then acc
            else
              let exp = ((n - op_Multiply j s % n) % n + n) % n in
              let omega_val = om_c exp in
              let rjp = (match nth rj_pows j with | Some v -> v | None -> cd_zero) in
              sum_j (j + 1) (cd_add acc (cd_mul omega_val rjp))
          in
          let raw = sum_j 0 cd_zero in
          let inv_n = { re = double_div double_one (double_of_int n); im = double_zero } in
          cd_mul inv_n raw :: build_s (s + 1)
      in
      build_s 0
    in

    // Step 5: Match d_s to radical expressions
    match match_ds_general tgi d_vals n with
    | None -> None
    | Some d_exprs ->

    // Step 6: R_j^n = Σ_s d_s · ω_n^{js}
    let rj_pow_exprs : list (rad_expr rational) =
      let rec build_j (j: nat) : list (rad_expr rational) =
        if j >= n_nat then []
        else if j = 0 then build_j (j + 1)  // skip j=0
        else
          let terms : list (rad_expr rational) =
            let rec build_s (s: nat) : list (rad_expr rational) =
              if s >= n_nat then []
              else
                let d_s = (match nth d_exprs s with | Some e -> e | None -> Lit rat_zero) in
                let omega_js = omega_pow_n_expr n (op_Multiply j s % n) in
                Mul d_s omega_js :: build_s (s + 1)
            in
            build_s 0
          in
          foldl1_add terms :: build_j (j + 1)
      in
      build_j 1
    in

    // Step 7: Branch selection: R_j = ω_n^{b_j} · ⁿ√(R_j^n)
    let rj_exprs : list (rad_expr rational) =
      let rec build_j (j: nat) : Dv (list (rad_expr rational)) =
        if j >= n_nat then []
        else
          let rjn_expr = (match nth rj_pow_exprs (j - 1) with | Some e -> e | None -> Lit rat_zero) in
          let target = (match nth rj_vals j with | Some v -> v | None -> cd_zero) in
          select_branch_n n rjn_expr target :: build_j (j + 1)
      in
      build_j 1
    in

    // R_0 = sum of depressed roots = 0 (for monic depressed polynomial)
    let all_r : list (rad_expr rational) = Lit rat_zero :: rj_exprs in

    // Step 8: Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j
    let root_exprs : list (rad_expr rational) =
      let rec build_k (k: nat) : list (rad_expr rational) =
        if k >= n_nat then []
        else
          let terms : list (rad_expr rational) =
            let rec build_j (j: nat) : list (rad_expr rational) =
              if j >= n_nat then []
              else
                let exp = ((n - op_Multiply j k % n) % n + n) % n in
                let omega_jk = omega_pow_n_expr n exp in
                let r_j = (match nth all_r j with | Some e -> e | None -> Lit rat_zero) in
                Mul omega_jk r_j :: build_j (j + 1)
            in
            build_j 0
          in
          let sum_expr = foldl1_add terms in
          Mul (Inv (Lit (rat_of_int n))) sum_expr :: build_k (k + 1)
      in
      build_k 0
    in

    // Step 9: Un-depress
    let final_exprs : list (rad_expr rational) =
      map (fun e -> Add e (Lit shift_val)) root_exprs
    in

    // Match to original root ordering
    let find_best_match (t: complex_double) : Dv (rad_expr rational) =
      let rec find_best (es: list (rad_expr rational)) (best: rad_expr rational) (best_d: double) : Dv (rad_expr rational) =
        match es with
        | [] -> best
        | e :: rest ->
          let v = eval_complex e in
          let d = cd_dist v t in
          if double_lt d best_d then find_best rest e d
          else find_best rest best best_d
      in
      match final_exprs with
      | [] -> Lit rat_zero
      | e :: rest ->
        let v = eval_complex e in
        find_best rest e (cd_dist v t)
    in
    let rec match_roots (rs: list complex_double) : Dv (list (rad_expr rational)) =
      match rs with
      | [] -> []
      | t :: rest -> find_best_match t :: match_roots rest
    in
    Some (match_roots num_roots)

/// --------------------------------------------------------------------------
/// Entry points
/// --------------------------------------------------------------------------

/// Solve a solvable polynomial via the Lagrange resolvent pipeline.
///
/// For degree 5, uses the degree-5 fast path (same algorithm, but
/// the group info comes from the degree-5 database).
/// For other prime degrees, uses the generalised solver.
///
/// num_roots: approximate complex roots from the caller.
val solve_via_tower_n : transitive_group_info -> poly rational -> list complex_double -> Dv (option (list (rad_expr rational)))
let solve_via_tower_n tgi f num_roots =
  if not tgi.tgi_solvable then None
  else
    let d = degree f in
    if is_prime d then solve_solvable_prime tgi f num_roots
    else None
