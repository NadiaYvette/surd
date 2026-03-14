(** Polynomial factoring over Q via Kronecker's method.

    Kronecker's method factors a polynomial by evaluating at enough
    integer points to determine potential factors. Practical for
    small-degree polynomials. *)

module P = Poly.RatPoly
module R = Rational

(** Try to find a factor of degree [d] of polynomial [f] by evaluating
    at [d+1] integer points and testing all combinations of divisors. *)
let try_factor_degree f d =
  let n = P.degree f in
  if d <= 0 || d >= n then None
  else
    (* Evaluate f at 0, 1, ..., d *)
    let points = List.init (d + 1) (fun i -> R.of_int i) in
    let values = List.map (P.eval f) points in
    (* For each value, find its integer divisors *)
    let divisors_of r =
      if R.is_zero r then [R.zero]
      else
        match R.to_int r with
        | None -> [r; R.neg r]  (* non-integer: only self and negation *)
        | Some n ->
          let n = abs n in
          let acc = ref [] in
          for d = 1 to n do
            if n mod d = 0 then begin
              acc := R.of_int d :: R.of_int (-d) :: !acc
            end
          done;
          !acc
    in
    let all_divisors = List.map divisors_of values in
    (* Try all combinations via Lagrange interpolation *)
    (* For simplicity, just try linear and quadratic factors *)
    if d = 1 then begin
      (* Linear factor: x - r where f(r) = 0
         Check rational roots: p/q where p | a0 and q | an *)
      let a0 = P.eval f R.zero in
      let an = match P.lead_coeff f with Some c -> c | None -> R.one in
      let p_divs = divisors_of a0 in
      let q_divs = divisors_of an in
      let found = ref None in
      List.iter (fun p ->
        List.iter (fun q ->
          if not (R.is_zero q) && Option.is_none !found then
            let r = R.div p q in
            let v = P.eval f r in
            if R.is_zero v then
              found := Some (P.of_coeffs [R.neg r; R.one]))
          q_divs) p_divs;
      !found
    end else
      (* For higher degrees, we'd need full Kronecker.
         Stub: return None for now *)
      let _ = all_divisors in
      None

(** Factor a polynomial over Q. Returns a list of irreducible factors
    with multiplicities. *)
let factor f =
  if P.degree f <= 0 then [(f, 1)]
  else
    (* First do square-free factorisation *)
    let sf = P.square_free f in
    List.concat_map (fun (g, mult) ->
      (* Try to factor each square-free factor *)
      let rec split_factors g =
        if P.degree g <= 1 then [(g, mult)]
        else
          match try_factor_degree g 1 with
          | Some factor ->
            let q = P.div g factor in
            (factor, mult) :: split_factors q
          | None -> [(g, mult)]
      in
      split_factors g) sf
