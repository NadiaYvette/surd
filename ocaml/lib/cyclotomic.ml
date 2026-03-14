(** Cyclotomic polynomials over the rationals.

    The n-th cyclotomic polynomial Phi_n(x) is the minimal polynomial
    of a primitive n-th root of unity over Q. *)

module P = Poly.RatPoly
module R = Rational

(** Euler's totient function. *)
let euler_totient n =
  let pos = Positive.of_int_exn n in
  let factors = Prime_factors.factorise pos in
  List.fold_left (fun acc (p, e) ->
    let rec ipow base exp = if exp = 0 then 1 else base * ipow base (exp - 1) in
    acc * (p - 1) * ipow p (e - 1)) 1 factors

(** Compute the n-th cyclotomic polynomial.

    Uses the identity:
    x^n - 1 = product of Phi_d(x) for d | n

    So Phi_n(x) = (x^n - 1) / product of Phi_d(x) for d | n, d < n *)
let cyclotomic n =
  if n <= 0 then invalid_arg "cyclotomic: n must be positive";
  (* Compute divisors of n *)
  let divisors =
    let acc = ref [] in
    for d = 1 to n do
      if n mod d = 0 then acc := d :: !acc
    done;
    List.rev !acc
  in
  (* Build x^n - 1 *)
  let xn_minus_1 =
    let coeffs = List.init (n + 1) (fun i ->
      if i = 0 then R.minus_one
      else if i = n then R.one
      else R.zero) in
    P.of_coeffs coeffs
  in
  (* Divide out Phi_d for all proper divisors *)
  (* We cache cyclotomic polynomials as we compute them *)
  let cache = Hashtbl.create 16 in
  let rec get_cyclo d =
    match Hashtbl.find_opt cache d with
    | Some p -> p
    | None ->
      let p = if d = 1 then
        P.of_coeffs [R.neg R.one; R.one]  (* x - 1 *)
      else
        let divs_d = List.filter (fun k -> k < d && d mod k = 0)
          (List.init d (fun i -> i + 1)) in
        let product = List.fold_left (fun acc k ->
          P.mul acc (get_cyclo k)) P.one divs_d in
        let x_d_minus_1 =
          let coeffs = List.init (d + 1) (fun i ->
            if i = 0 then R.minus_one
            else if i = d then R.one
            else R.zero) in
          P.of_coeffs coeffs
        in
        P.div x_d_minus_1 product
      in
      Hashtbl.add cache d p;
      p
  in
  ignore (divisors, xn_minus_1);
  get_cyclo n
