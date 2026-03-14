(** Sturm-based root isolation for polynomials over Q.

    Given a square-free polynomial with rational coefficients,
    computes isolating intervals for all real roots. *)

module P = Poly.RatPoly
module R = Rational

(** Compute the Sturm sequence of a polynomial. *)
let sturm_sequence f =
  let f' = P.diff f in
  let rec go prev curr acc =
    if P.degree curr < 0 then List.rev acc
    else
      let _, r = P.div_mod prev curr in
      let neg_r = P.neg r in
      go curr neg_r (neg_r :: acc)
  in
  f :: f' :: go f f' []

(** Count sign changes in a list of rationals. *)
let sign_changes values =
  let non_zero = List.filter (fun v -> not (R.is_zero v)) values in
  let rec count = function
    | [] | [_] -> 0
    | a :: (b :: _ as rest) ->
      (if (R.is_pos a && R.is_neg b) || (R.is_neg a && R.is_pos b) then 1 else 0)
      + count rest
  in
  count non_zero

(** Count real roots of [f] in the interval [(a, b)] using Sturm's theorem. *)
let count_roots_in sturm_seq a b =
  let at_a = List.map (fun p -> P.eval p a) sturm_seq in
  let at_b = List.map (fun p -> P.eval p b) sturm_seq in
  sign_changes at_a - sign_changes at_b

(** Isolate real roots of a square-free polynomial into disjoint intervals.
    Returns a list of intervals, each containing exactly one root. *)
let isolate_roots f =
  if P.degree f <= 0 then []
  else
    let ss = sturm_sequence f in
    (* Find a bound on the roots *)
    let bound =
      let coeffs = P.to_coeffs f in
      let lc = match P.lead_coeff f with Some c -> c | None -> R.one in
      let max_ratio = List.fold_left (fun acc c ->
        let r = R.abs (R.div c lc) in
        if R.compare r acc > 0 then r else acc) R.zero coeffs in
      R.add max_ratio R.one
    in
    let neg_bound = R.neg bound in
    let total = count_roots_in ss neg_bound bound in
    if total = 0 then []
    else
      (* Bisect to isolate *)
      let rec bisect lo hi =
        let n = count_roots_in ss lo hi in
        if n = 0 then []
        else if n = 1 then [Interval.make lo hi]
        else
          let mid = R.div (R.add lo hi) (R.of_int 2) in
          bisect lo mid @ bisect mid hi
      in
      bisect neg_bound bound
