(** Radical solutions for solvable polynomials.

    Top-level interface for solving polynomials via Galois group
    computation. Composes Identify (group identification) and
    Radical_tower (Lagrange resolvent descent).

    For degree <= 4, returns [None] so the caller can use direct
    Cardano/Ferrari formulas. For prime degree >= 5, identifies
    the Galois group and solves if solvable. *)

module P = Poly.RatPoly

(** Identify the Galois group and solve if solvable.

    Returns [Some (group_name, roots)] if the polynomial has a
    solvable Galois group and can be expressed in radicals.
    Returns [None] if the group is non-solvable (A_n or S_n),
    the degree is unsupported, or solving fails. *)
let identify_and_solve f =
  if P.degree f <= 4 then None
  else
    match Identify.identify f with
    | None -> None
    | Some gr ->
      if not gr.Identify.group.Transitive_group.solvable then None
      else
        match Radical_tower.solve_via_tower gr f with
        | None -> None
        | Some roots ->
          Some (Identify.group_name gr, roots)

(** Attempt to solve a polynomial by radicals.
    Returns [Some roots] if the Galois group is solvable,
    [None] otherwise. *)
let solve f : Rational.t Rad_expr.t list option =
  match identify_and_solve f with
  | None -> None
  | Some (_, roots) -> Some roots

(** Check if a polynomial is solvable by radicals. *)
let is_solvable f =
  if P.degree f <= 4 then true
  else
    match Identify.identify f with
    | None -> false
    | Some gr -> gr.Identify.group.Transitive_group.solvable

(** Pick the radical expression whose numerical value is closest
    to a target real value. *)
let pick_closest_real exprs target =
  let scored = List.map (fun e ->
    let v = Eval.eval_complex e in
    let dist =
      if Float.abs v.Complex.im < 1e-6 then
        Float.abs (v.Complex.re -. target)
      else
        Complex.norm (Complex.sub v { Complex.re = target; im = 0.0 })
    in
    (e, dist)
  ) exprs in
  let valid = List.filter (fun (_, d) -> d < 1e-4) scored in
  match valid with
  | [] ->
    let (best_e, best_d) = List.fold_left (fun (be, bd) (e, d) ->
      if d < bd then (e, d) else (be, bd)
    ) (List.hd scored) scored in
    if best_d < 1.0 then Some best_e else None
  | _ ->
    Some (fst (List.fold_left (fun (be, bd) (e, d) ->
      if d < bd then (e, d) else (be, bd)
    ) (List.hd valid) valid))
