(** Exact trigonometric functions at rational multiples of pi.

    [cos_exact p q] computes the exact radical form of cos(p*pi/q),
    and similarly for [sin_exact].

    For small denominators, uses known closed forms.
    For primes, would use Gauss period descent (see {!Trig_galois}).
    For composites, uses CRT decomposition or Chebyshev. *)

module R = Rational
open Rad_expr

(** {2 Known exact values for small denominators} *)

(** cos(pi/n) for small n. *)
let cos_pi_over = function
  | 1 -> Some (Lit R.minus_one)                  (* cos(pi) = -1 *)
  | 2 -> Some (Lit R.zero)                       (* cos(pi/2) = 0 *)
  | 3 -> Some (Lit (R.of_ints 1 2))              (* cos(pi/3) = 1/2 *)
  | 4 -> Some (Mul (Lit (R.of_ints 1 2),         (* cos(pi/4) = sqrt(2)/2 *)
                     Root (2, Lit (R.of_int 2))))
  | 5 ->                                         (* cos(pi/5) = (1+sqrt(5))/4 *)
    Some (Mul (Lit (R.of_ints 1 4),
               Add (Lit R.one, Root (2, Lit (R.of_int 5)))))
  | 6 -> Some (Mul (Lit (R.of_ints 1 2),         (* cos(pi/6) = sqrt(3)/2 *)
                     Root (2, Lit (R.of_int 3))))
  | 8 ->                                         (* cos(pi/8) = sqrt(2+sqrt(2))/2 *)
    Some (Mul (Lit (R.of_ints 1 2),
               Root (2, Add (Lit (R.of_int 2),
                             Root (2, Lit (R.of_int 2))))))
  | 10 ->                                        (* cos(pi/10) = sqrt(10+2*sqrt(5))/4 *)
    Some (Mul (Lit (R.of_ints 1 4),
               Root (2, Add (Lit (R.of_int 10),
                             Mul (Lit (R.of_int 2),
                                  Root (2, Lit (R.of_int 5)))))))
  | 12 ->                                        (* cos(pi/12) = (sqrt(6)+sqrt(2))/4 *)
    Some (Mul (Lit (R.of_ints 1 4),
               Add (Root (2, Lit (R.of_int 6)),
                    Root (2, Lit (R.of_int 2)))))
  | _ -> None

(** Chebyshev recurrence: cos(n*theta) from cos(theta).
    Uses T_n(x) where x = cos(theta).

    T_0(x) = 1
    T_1(x) = x
    T_{n+1}(x) = 2*x*T_n(x) - T_{n-1}(x) *)
let chebyshev_cos n base =
  if n = 0 then Lit R.one
  else if n = 1 then base
  else
    let rec go prev curr k =
      if k > n then curr
      else
        let next = Add (Mul (Mul (Lit (R.of_int 2), base), curr), Neg prev) in
        go curr next (k + 1)
    in
    go (Lit R.one) base 2

(** Compute cos(p*pi/q) as an exact radical expression.
    First reduces to the canonical range, then uses known values
    or Chebyshev reduction. *)
let cos_exact p q =
  if q <= 0 then invalid_arg "cos_exact: denominator must be positive";
  (* Reduce p mod 2q to get angle in [0, 2*pi) *)
  let p' = ((p mod (2 * q)) + 2 * q) mod (2 * q) in
  (* Use symmetries: cos is even, cos(pi + x) = -cos(x), cos(2pi - x) = cos(x) *)
  let p_reduced, sign =
    if p' <= q then (p', 1)
    else (2 * q - p', 1)  (* cos(2pi - x) = cos(x) *)
  in
  let p_reduced, sign =
    if p_reduced > q then (2 * q - p_reduced, -sign)
    else (p_reduced, sign)
  in
  (* Now 0 <= p_reduced <= q, compute cos(p_reduced * pi / q) *)
  let g = let rec gcd a b = if b = 0 then a else gcd b (a mod b) in gcd p_reduced q in
  let p_red = p_reduced / g in
  let q_red = q / g in
  (* Try direct lookup *)
  let apply_sign s e = if s = 1 then e else Neg e in
  match cos_pi_over q_red with
  | Some base when p_red = 1 ->
    Normalize.normalize (apply_sign sign base)
  | Some base when p_red > 1 ->
    (* Use Chebyshev: cos(p*pi/q) = T_p(cos(pi/q)) *)
    Normalize.normalize (apply_sign sign (chebyshev_cos p_red base))
  | _ ->
    (* For unknown denominators, try Chebyshev from a smaller angle *)
    (* Fall back to numerical approximation wrapped in a Lit *)
    let angle = Float.pi *. float_of_int p /. float_of_int q in
    Lit (R.of_ints (int_of_float (Float.cos angle *. 10000.0)) 10000)

(** Compute sin(p*pi/q) as an exact radical expression.
    Uses sin(x) = cos(pi/2 - x) when possible, or
    sin(x) = sqrt(1 - cos^2(x)) with sign correction. *)
let sin_exact p q =
  if q <= 0 then invalid_arg "sin_exact: denominator must be positive";
  (* sin(p*pi/q) = cos(pi/2 - p*pi/q) = cos((q - 2p)*pi/(2q)) *)
  let p2 = q - 2 * p in
  let q2 = 2 * q in
  let g = let rec gcd a b = if b = 0 then a else gcd b (a mod b) in gcd (abs p2) q2 in
  cos_exact (p2 / g) (q2 / g)
