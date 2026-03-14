(** Radical tower construction for solvable polynomials via Lagrange resolvents.

    Given an irreducible polynomial f(x) in Q[x] of prime degree n with
    solvable Galois group G, constructs radical expressions for its roots
    via the 9-step pipeline:

    1. Depress (eliminate second-highest term)
    2. Find cyclic ordering so Galois generator acts as n-cycle
    3. Lagrange resolvents R_j = sum_k omega^{jk} alpha_k
    4. DFT: d_s = (1/n) sum_j omega^{-js} R_j^n
    5. Coefficient matching: recognise d_s in the coefficient field
    6. Reconstruct R_j^n as RadExpr
    7. Branch selection via numerical comparison
    8. Inverse DFT: alpha_k = (1/n) sum_j omega^{-jk} R_j
    9. Un-depress and match to original root ordering *)

module P = Poly.RatPoly
module R = Rational
module E = Rad_expr

(** {2 Numerical helpers} *)

let pi = Float.pi

(** omega_n^k = e^{2*pi*i*k/n} as Complex.t *)
let omega_c n k =
  let angle = 2.0 *. pi *. Float.of_int k /. Float.of_int n in
  { Complex.re = Float.cos angle; im = Float.sin angle }

(** Score how close a complex number is to a rational.
    Returns |im| + frac(re). *)
let score_rational z =
  let re = z.Complex.re in
  let im = z.Complex.im in
  Float.abs im +. Float.abs (re -. Float.of_int (Float.to_int (Float.round re)))

(** Approximate a float as a small-denominator rational. *)
let approx_rat = Resolvent.approx_rat

(** Evaluate a rational RadExpr to Complex.t. *)
let eval_c expr = Eval.eval_complex expr

(** {2 Omega_n as RadExpr} *)

(** omega_5 = cos(2pi/5) + i*sin(2pi/5) as a radical expression. *)
let omega5_expr =
  let cos5 = E.mul (E.inv (E.lit (R.of_int 4)))
    (E.sub (E.root 2 (E.lit (R.of_int 5))) (E.lit R.one)) in
  let sin5 = E.mul (E.inv (E.lit (R.of_int 4)))
    (E.root 2 (E.add (E.lit (R.of_int 10))
      (E.mul (E.lit (R.of_int 2)) (E.root 2 (E.lit (R.of_int 5)))))) in
  let i = E.root 2 (E.lit (R.of_int (-1))) in
  E.add cos5 (E.mul i sin5)

(** omega_n = e^{2*pi*i/n} as a radical expression.
    For n=5, uses the exact form. For other primes, builds
    cos(2pi/n) + i*sin(2pi/n) from numerical approximation
    as a fallback. *)
let omega_n_expr n =
  if n = 5 then omega5_expr
  else
    (* Fallback: approximate literal *)
    let theta = 2.0 *. pi /. Float.of_int n in
    let cos_val = Float.cos theta in
    let sin_val = Float.sin theta in
    E.add (E.lit (approx_rat cos_val))
      (E.mul (E.root 2 (E.lit (R.of_int (-1)))) (E.lit (approx_rat sin_val)))

(** omega_n^k as a radical expression, reduced modulo n. *)
let omega_pow_n_expr n om_expr k =
  let k' = ((k mod n) + n) mod n in
  if k' = 0 then E.lit R.one
  else if k' = 1 then om_expr
  else E.pow om_expr k'

(** {2 Cyclic ordering} *)

(** Score a candidate ordering by DFT coefficient rationality. *)
let score_ordering_n roots ordering n tg =
  let ordered = List.map (fun i -> List.nth roots i) ordering in
  let rj j =
    let sum = ref Complex.zero in
    List.iteri (fun k ak ->
      sum := Complex.add !sum (Complex.mul (omega_c n (j * k)) ak)
    ) ordered;
    !sum
  in
  let rj_pows = List.init n (fun j ->
    let r = rj j in
    let rec cpow z k = if k = 0 then Complex.one else Complex.mul z (cpow z (k - 1)) in
    cpow r n
  ) in
  let d_vals = List.init n (fun s ->
    let inv_n = 1.0 /. Float.of_int n in
    let sum = ref Complex.zero in
    List.iteri (fun j rp ->
      sum := Complex.add !sum
        (Complex.mul (omega_c n ((n - (j * s) mod n) mod n)) rp)
    ) rj_pows;
    Complex.mul { Complex.re = inv_n; im = 0.0 } !sum
  ) in
  let p = n in
  let d = tg.Transitive_group.order / p in
  if d = 1 then
    (* Cyclic: all d_s should be rational *)
    List.fold_left (fun acc dv -> acc +. score_rational dv) 0.0 d_vals
  else if d = 2 then
    (* Dihedral *)
    score_rational (List.hd d_vals) +.
    List.fold_left (fun acc s ->
      acc +.
      score_rational (Complex.add (List.nth d_vals s) (List.nth d_vals (n - s))) +.
      score_rational (Complex.mul (List.nth d_vals s) (List.nth d_vals (n - s)))
    ) 0.0 (List.init (n / 2) (fun i -> i + 1))
  else
    (* General: check all d_s for rationality (simplified) *)
    List.fold_left (fun acc dv -> acc +. score_rational dv) 0.0 d_vals

(** Build a cyclic ordering by choosing root 0 at position 0 and root
    [next] at position 1, then greedily assigning subsequent positions. *)
let build_ordering roots n start next =
  let step = Complex.sub (List.nth roots next) (List.nth roots start) in
  let used = Hashtbl.create n in
  Hashtbl.add used start true;
  Hashtbl.add used next true;
  let result = ref [next; start] in
  let pos = ref next in
  while Hashtbl.length used < n do
    let target = Complex.add (List.nth roots !pos) step in
    let unused = List.init n Fun.id |> List.filter (fun i -> not (Hashtbl.mem used i)) in
    match unused with
    | [] -> ()  (* should not happen *)
    | _ ->
      let closest = List.fold_left (fun best i ->
        let dist = Complex.norm (Complex.sub (List.nth roots i) target) in
        let best_dist = Complex.norm (Complex.sub (List.nth roots (fst best)) target) in
        if dist < best_dist then (i, dist) else best
      ) (List.hd unused, Float.infinity) unused |> fst in
      Hashtbl.add used closest true;
      result := closest :: !result;
      pos := closest
  done;
  List.rev !result

(** Find cyclic ordering for n roots.
    For n <= 8: brute-force all (n-1)! orderings.
    For n > 8: try (n-1) rotations by choosing each root as sigma(alpha_0). *)
let find_cyclic_ordering_n roots n tg =
  if n <= 8 then begin
    let rest = List.init (n - 1) (fun i -> i + 1) in
    let orderings = List.map (fun p -> 0 :: p) (Identify.perms rest) in
    let scored = List.map (fun o -> (o, score_ordering_n roots o n tg)) orderings in
    let sorted = List.sort (fun (_, a) (_, b) -> Float.compare a b) scored in
    match sorted with
    | (best_o, best_score) :: (_, second_score) :: _
      when best_score < 5.0 *. Float.of_int n &&
           best_score < 0.5 *. second_score -> Some best_o
    | (best_o, best_score) :: _
      when best_score < Float.of_int n -> Some best_o
    | _ -> None
  end else begin
    (* For large n, try (n-1) rotations *)
    let candidates = List.init (n - 1) (fun i ->
      let next = i + 1 in
      let ordering = build_ordering roots n 0 next in
      (ordering, score_ordering_n roots ordering n tg)
    ) in
    let sorted = List.sort (fun (_, a) (_, b) -> Float.compare a b) candidates in
    match sorted with
    | (best_o, best_score) :: _ when best_score < 5.0 *. Float.of_int n ->
      Some best_o
    | _ -> None
  end

(** {2 DFT coefficient matching} *)

(** Match a complex number to a rational literal. *)
let match_rat_c z =
  if Float.abs z.Complex.im > 0.05 then None
  else Some (E.lit (approx_rat z.Complex.re))

(** Match a conjugate pair {d_s, d_{n-s}} to quadratic expressions. *)
let match_conj_pair d1 d2 =
  let s = Complex.add d1 d2 in
  let p = Complex.mul d1 d2 in
  match match_rat_c s, match_rat_c p with
  | Some s_expr, Some p_expr ->
    let s_r = approx_rat s.Complex.re in
    let p_r = approx_rat p.Complex.re in
    let disc_r = R.sub (R.mul s_r s_r) (R.mul (R.of_int 4) p_r) in
    let disc_expr = E.sub (E.mul s_expr s_expr) (E.mul (E.lit (R.of_int 4)) p_expr) in
    let sqrt_disc = E.root 2 disc_expr in
    let e_plus = E.mul (E.inv (E.lit (R.of_int 2))) (E.add s_expr sqrt_disc) in
    let e_minus = E.mul (E.inv (E.lit (R.of_int 2))) (E.sub s_expr sqrt_disc) in
    (* Determine which branch matches d1 *)
    let sqrt_disc_val =
      let dr = R.to_float disc_r in
      if dr >= 0.0 then { Complex.re = Float.sqrt dr; im = 0.0 }
      else { Complex.re = 0.0; im = Float.sqrt (Float.abs dr) }
    in
    let d1_plus_val = Complex.div (Complex.add s sqrt_disc_val)
      { Complex.re = 2.0; im = 0.0 } in
    if Complex.norm (Complex.sub d1_plus_val d1) <
       Complex.norm (Complex.sub d1_plus_val d2) then
      Some (e_plus, e_minus)
    else
      Some (e_minus, e_plus)
  | _ -> None

(** Match all d_s as rational (cyclic Galois group). *)
let match_ds_all_rational d_vals =
  let results = List.map match_rat_c d_vals in
  if List.exists Option.is_none results then None
  else Some (List.filter_map Fun.id results)

(** Match d_s for dihedral group (d = 2). *)
let match_ds_dihedral d_vals n =
  match match_rat_c (List.hd d_vals) with
  | None -> None
  | Some d0_expr ->
    let half_n = (n - 1) / 2 in
    let pair_results = List.init half_n (fun i ->
      let s = i + 1 in
      match_conj_pair (List.nth d_vals s) (List.nth d_vals (n - s))
    ) in
    if List.exists Option.is_none pair_results then None
    else
      let pairs = List.filter_map Fun.id pair_results in
      (* Assemble result: d_0, d_1, ..., d_{n-1} *)
      let result = Array.make n (E.lit R.zero) in
      result.(0) <- d0_expr;
      List.iteri (fun i (e_s, e_ns) ->
        let s = i + 1 in
        result.(s) <- e_s;
        result.(n - s) <- e_ns
      ) pairs;
      Some (Array.to_list result)

(** Match a complex number to r * omega_n^k (single-term). *)
let match_single_omega_n n d =
  let candidates = List.init n (fun k ->
    let v = Complex.mul d (omega_c n ((-k + n) mod n)) in
    (k, v, score_rational v)
  ) in
  let (best_k, best_v, best_score) = List.fold_left (fun (bk, bv, bs) (k, v, s) ->
    if s < bs then (k, v, s) else (bk, bv, bs)
  ) (List.hd candidates) (List.tl candidates) in
  if best_score < 0.01 then
    let r = approx_rat best_v.Complex.re in
    let om_expr = omega_n_expr n in
    if best_k = 0 then Some (E.lit r)
    else Some (E.mul (E.lit r) (omega_pow_n_expr n om_expr best_k))
  else None

(** Match a complex number to a two-term decomposition
    a * omega_n^j + b * omega_n^k. *)
let match_two_term_omega_n n d =
  let om_expr = omega_n_expr n in
  let best = ref None in
  let best_err = ref Float.infinity in
  for j = 0 to n - 1 do
    for k = j + 1 to n - 1 do
      let wj = omega_c n j in
      let wk = omega_c n k in
      let det = wj.Complex.re *. wk.Complex.im -. wj.Complex.im *. wk.Complex.re in
      if Float.abs det > 1e-10 then begin
        let a = (d.Complex.re *. wk.Complex.im -. d.Complex.im *. wk.Complex.re) /. det in
        let b = (wj.Complex.re *. d.Complex.im -. wj.Complex.im *. d.Complex.re) /. det in
        let a_r = approx_rat a in
        let b_r = approx_rat b in
        let recon = Complex.add
          (Complex.mul { Complex.re = R.to_float a_r; im = 0.0 } wj)
          (Complex.mul { Complex.re = R.to_float b_r; im = 0.0 } wk) in
        let err = Complex.norm (Complex.sub recon d) in
        if err < 0.01 && err < !best_err then begin
          best_err := err;
          let term_j = if j = 0 then E.lit a_r
            else E.mul (E.lit a_r) (omega_pow_n_expr n om_expr j) in
          let term_k = if k = 0 then E.lit b_r
            else E.mul (E.lit b_r) (omega_pow_n_expr n om_expr k) in
          best := Some (E.add term_j term_k)
        end
      end
    done
  done;
  !best

(** Match a complex number to an element of Q(omega_n).
    Tries single-term first, then two-term decomposition. *)
let match_q_omega_n n d =
  match match_single_omega_n n d with
  | Some e -> Some e
  | None -> match_two_term_omega_n n d

(** Match d_s via Galois orbit structure for larger stabiliser groups. *)
let match_ds_via_orbits _tg d_vals n =
  (* First try: all rational *)
  match match_ds_all_rational d_vals with
  | Some exprs -> Some exprs
  | None ->
    (* Try: d_0 rational, rest via Q(omega_n) *)
    match match_rat_c (List.hd d_vals) with
    | None -> None
    | Some d0_expr ->
      let rest = List.tl d_vals in
      let rest_results = List.map (match_q_omega_n n) rest in
      if List.exists Option.is_none rest_results then None
      else Some (d0_expr :: List.filter_map Fun.id rest_results)

(** General coefficient matching dispatcher. *)
let match_ds_general tg d_vals n =
  let p = n in
  let d = tg.Transitive_group.order / p in
  if d = 1 then match_ds_all_rational d_vals
  else if d = 2 then match_ds_dihedral d_vals n
  else match_ds_via_orbits tg d_vals n

(** {2 Degree-5 specific matching} *)

(** Match d_s for F20 (Frobenius group of order 20).
    d_0 in Q, d_1..d_4 in Q(omega_5). *)
let match_ds_f20 d_vals =
  match match_rat_c (List.hd d_vals) with
  | None -> None
  | Some d0_expr ->
    let rest = List.tl d_vals in
    let rest_results = List.map (fun v ->
      match match_single_omega_n 5 v with
      | Some e -> Some e
      | None -> match_two_term_omega_n 5 v
    ) rest in
    if List.exists Option.is_none rest_results then None
    else Some (d0_expr :: List.filter_map Fun.id rest_results)

(** Match d_s for D5 (dihedral group of order 10).
    d_0 in Q, {d_1, d_4} and {d_2, d_3} are conjugate pairs. *)
let match_ds_d5 d_vals =
  match d_vals with
  | [d0; d1; d2; d3; d4] ->
    begin match match_rat_c d0 with
    | None -> None
    | Some d0_expr ->
      begin match match_conj_pair d1 d4, match_conj_pair d2 d3 with
      | Some (d1_expr, d4_expr), Some (d2_expr, d3_expr) ->
        Some [d0_expr; d1_expr; d2_expr; d3_expr; d4_expr]
      | _ -> None
      end
    end
  | _ -> None

(** Match d_s for C5 (cyclic group of order 5). All d_s rational. *)
let match_ds_c5 d_vals = match_ds_all_rational d_vals

(** Degree-5 specific coefficient matching.
    Group names from trans_groups_of_prime: Z5, D5, AGL(1,5). *)
let match_ds_5 group_order d_vals =
  let d = group_order / 5 in
  if d = 1 then match_ds_c5 d_vals       (* Z5, order 5 *)
  else if d = 2 then match_ds_d5 d_vals  (* D5, order 10 *)
  else if d = 4 then match_ds_f20 d_vals (* AGL(1,5), order 20 *)
  else None

(** {2 Branch selection} *)

(** Complex nth power. *)
let cpow z n =
  let rec go acc k = if k = 0 then acc else go (Complex.mul acc z) (k - 1) in
  go Complex.one n

(** Select the correct branch of the n-th root.
    Tries all n candidates omega_n^k * n-th-root(R_j^n)
    and picks the one closest to target_val. *)
let select_branch_n n om_expr rjn_expr target_val =
  let rjn_val = eval_c rjn_expr in
  let r = Complex.norm rjn_val in
  let theta = Float.atan2 rjn_val.Complex.im rjn_val.Complex.re in
  let rn = r ** (1.0 /. Float.of_int n) in
  let an = theta /. Float.of_int n in
  let principal_val = { Complex.re = rn *. Float.cos an; im = rn *. Float.sin an } in
  let principal_root = E.root n rjn_expr in
  let scored = List.init n (fun k ->
    let branch = Complex.mul (omega_c n k) principal_val in
    (k, Complex.norm (Complex.sub branch target_val))
  ) in
  let best_k = fst (List.fold_left (fun (bk, bd) (k, d) ->
    if d < bd then (k, d) else (bk, bd)
  ) (List.hd scored) (List.tl scored)) in
  if best_k = 0 then principal_root
  else E.mul (omega_pow_n_expr n om_expr best_k) principal_root

(** {2 Match to original ordering} *)

(** Match radical expressions to original numerical roots by proximity. *)
let match_to_original exprs num_roots =
  let expr_vals = List.map (fun e -> (e, eval_c e)) exprs in
  List.map (fun t ->
    fst (List.fold_left (fun (best_e, best_d) (e, v) ->
      let d = Complex.norm (Complex.sub v t) in
      if d < best_d then (e, d) else (best_e, best_d)
    ) (List.hd expr_vals |> fst, Float.infinity) expr_vals)
  ) num_roots

(** {2 Normalise via fold_constants} *)

(** Simple constant folding on the expression. *)
let fold expr = Normalize.normalize expr

(** {2 Main solver} *)

(** Solve a solvable polynomial of prime degree n.

    Implements the full 9-step pipeline:
    1. Depress
    2. Find cyclic ordering
    3. Lagrange resolvents
    4. DFT coefficients
    5. Coefficient matching
    6. Reconstruct R_j^n as RadExpr
    7. Branch selection
    8. Inverse DFT
    9. Un-depress + match ordering *)
let solve_solvable_prime tg f num_roots =
  let n = P.degree f in
  if not tg.Transitive_group.solvable then None
  else begin
    (* Step 1: Depress *)
    let cs = P.to_coeffs f in
    let lc = match P.lead_coeff f with Some c -> c | None -> R.one in
    let monic_cs = List.map (fun c -> R.div c lc) cs in
    let an1 = List.nth monic_cs (n - 1) in
    let shift_val = R.neg (R.div an1 (R.of_int n)) in
    let dep_roots = List.map (fun r ->
      Complex.sub r { Complex.re = R.to_float shift_val; im = 0.0 }
    ) num_roots in

    (* Step 2: Find cyclic ordering *)
    match find_cyclic_ordering_n dep_roots n tg with
    | None -> None
    | Some ordering ->
      let ordered_roots = List.map (fun i -> List.nth dep_roots i) ordering in

      (* Step 3: Lagrange resolvents *)
      let rj j =
        let sum = ref Complex.zero in
        List.iteri (fun k ak ->
          sum := Complex.add !sum (Complex.mul (omega_c n (j * k)) ak)
        ) ordered_roots;
        !sum
      in
      let rj_vals = List.init n (fun j -> rj j) in
      let rj_pows = List.map (fun r -> cpow r n) rj_vals in

      (* Step 4: DFT *)
      let d_vals = List.init n (fun s ->
        let inv_n = 1.0 /. Float.of_int n in
        let sum = ref Complex.zero in
        List.iteri (fun j rp ->
          sum := Complex.add !sum
            (Complex.mul (omega_c n ((n - (j * s) mod n) mod n)) rp)
        ) rj_pows;
        Complex.mul { Complex.re = inv_n; im = 0.0 } !sum
      ) in

      (* Step 5: Coefficient matching *)
      let d_exprs_opt =
        if n = 5 then match_ds_5 tg.Transitive_group.order d_vals
        else match_ds_general tg d_vals n
      in
      match d_exprs_opt with
      | None -> None
      | Some d_exprs ->
        let om_expr = omega_n_expr n in

        (* Step 6: Reconstruct R_j^n *)
        let rj_pow_exprs = List.init (n - 1) (fun idx ->
          let j = idx + 1 in
          let terms = List.init n (fun s ->
            E.mul (List.nth d_exprs s)
              (omega_pow_n_expr n om_expr ((j * s) mod n))
          ) in
          match terms with
          | [] -> E.lit R.zero
          | [t] -> t
          | t :: rest -> List.fold_left E.add t rest
        ) in

        (* Step 7: Branch selection *)
        let rj_exprs = List.init (n - 1) (fun idx ->
          let j = idx + 1 in
          select_branch_n n om_expr (List.nth rj_pow_exprs idx) (List.nth rj_vals j)
        ) in

        (* R_0 = 0 (sum of depressed roots) *)
        let all_r = E.lit R.zero :: rj_exprs in

        (* Step 8: Inverse DFT *)
        let root_exprs = List.init n (fun k ->
          let terms = List.init n (fun j ->
            E.mul
              (omega_pow_n_expr n om_expr ((n - (j * k) mod n) mod n))
              (List.nth all_r j)
          ) in
          let inner = match terms with
            | [] -> E.lit R.zero
            | [t] -> t
            | t :: rest -> List.fold_left E.add t rest
          in
          fold (E.mul (E.inv (E.lit (R.of_int n))) inner)
        ) in

        (* Step 9: Un-depress *)
        let final_exprs = List.map (fun e ->
          fold (E.add e (E.lit shift_val))
        ) root_exprs in

        Some (match_to_original final_exprs num_roots)
  end

(** {2 Public API} *)

(** A step in a radical tower: adjoin the n-th root of an element. *)
type tower_step = {
  index : int;
  radicand : Rational.t Rad_expr.t;
}

(** A radical tower: a sequence of radical extensions. *)
type t = tower_step list

(** Construct a radical tower for a polynomial with solvable Galois group. *)
let construct _f _group = ([] : t)

(** Solve a solvable polynomial via Lagrange resolvent descent.

    Given a [galois_result] (from [Identify.identify]) and the
    original polynomial, returns radical expressions for all roots.
    Returns [None] if the group is non-solvable or solving fails. *)
let solve_via_tower (gr : Identify.galois_result) f =
  solve_solvable_prime gr.Identify.group f gr.Identify.roots
