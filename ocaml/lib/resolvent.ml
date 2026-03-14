(** Resolvent polynomials and root finding for Galois group computation.

    Provides numerical root finding via Aberth-Ehrlich simultaneous
    iteration, resolvent construction from numerical roots,
    discriminant computation, and rationality tests. *)

module P = Poly.RatPoly

(** {2 Numerical root finding via Aberth-Ehrlich} *)

(** Cauchy's upper bound on root magnitudes. *)
let cauchy_bound f =
  let cs = P.to_coeffs f in
  let n = List.length cs - 1 in
  let an = Float.abs (Rational.to_float (List.nth cs n)) in
  1.0 +. List.fold_left (fun acc i ->
    Float.max acc (Float.abs (Rational.to_float (List.nth cs i)) /. an)
  ) 0.0 (List.init n Fun.id)

(** Evaluate a rational polynomial at a complex point (Horner's method). *)
let eval_complex f z =
  let cs = P.to_coeffs f in
  List.fold_right (fun c acc ->
    Complex.add { Complex.re = Rational.to_float c; im = 0.0 } (Complex.mul z acc)
  ) cs Complex.zero

(** Formal derivative of a rational polynomial. *)
let diff_poly f =
  let cs = P.to_coeffs f in
  match cs with
  | [] | [_] -> P.zero
  | _ :: rest ->
    P.of_coeffs (List.mapi (fun i c ->
      Rational.mul (Rational.of_int (i + 1)) c
    ) rest)

(** Approximate all complex roots via Aberth-Ehrlich. *)
let complex_roots_of f =
  let d = P.degree f in
  if d <= 0 then []
  else if d = 1 then begin
    let cs = P.to_coeffs f in
    match cs with
    | [c0; c1] ->
      let v = -. (Rational.to_float c0) /. (Rational.to_float c1) in
      [{ Complex.re = v; im = 0.0 }]
    | _ -> []
  end else begin
    let r = cauchy_bound f in
    let n = d in
    let pi = Float.pi in
    (* Initial guesses on a circle *)
    let zs = Array.init n (fun k ->
      let angle = 2.0 *. pi *. Float.of_int k /. Float.of_int n +. 0.37 in
      { Complex.re = r *. Float.cos angle; im = r *. Float.sin angle }
    ) in
    let f' = diff_poly f in
    let max_iter = 500 in
    let converged = ref false in
    let iter = ref 0 in
    while not !converged && !iter < max_iter do
      incr iter;
      let max_shift = ref 0.0 in
      let new_zs = Array.init n (fun i ->
        let z = zs.(i) in
        let fz = eval_complex f z in
        let f'z = eval_complex f' z in
        if Complex.norm f'z < 1e-300 then z
        else
          let w = Complex.div fz f'z in
          let s = ref Complex.zero in
          for j = 0 to n - 1 do
            if j <> i then
              s := Complex.add !s (Complex.div Complex.one (Complex.sub z zs.(j)))
          done;
          let denom = Complex.sub Complex.one (Complex.mul w !s) in
          let new_z =
            if Complex.norm denom < 1e-300 then
              Complex.sub z w
            else
              Complex.sub z (Complex.div w denom)
          in
          let shift = Complex.norm (Complex.sub new_z z) in
          if shift > !max_shift then max_shift := shift;
          new_z
      ) in
      Array.blit new_zs 0 zs 0 n;
      if !max_shift < 1e-15 then converged := true
    done;
    Array.to_list zs
  end

(** {2 Rational approximation} *)

(** Approximate a float as a small-denominator rational. *)
let approx_rat x =
  let best = ref (Rational.of_int (Float.to_int (Float.round x))) in
  let best_err = ref (Float.abs (x -. Float.of_int (Float.to_int (Float.round x)))) in
  for d = 1 to 10000 do
    let fd = Float.of_int d in
    let n = Float.to_int (Float.round (x *. fd)) in
    let err = Float.abs (Float.of_int n /. fd -. x) in
    if err < 1e-6 && err < !best_err then begin
      best := Rational.of_ints n d;
      best_err := err
    end
  done;
  !best

(** {2 Discriminant} *)

(** Discriminant of a polynomial over Q, computed numerically from roots
    and then rounded to rational.

    disc(f) = a_n^{2n-2} * prod_{i<j} (r_i - r_j)^2
    We compute this numerically and round. *)
let discriminant_of f =
  let n = P.degree f in
  if n <= 0 then Rational.zero
  else if n = 1 then Rational.one
  else begin
    let roots = complex_roots_of f in
    let lc = match P.lead_coeff f with Some c -> c | None -> Rational.one in
    (* prod_{i<j} (r_i - r_j)^2 *)
    let prod = ref Complex.one in
    let nr = List.length roots in
    for i = 0 to nr - 2 do
      for j = i + 1 to nr - 1 do
        let diff = Complex.sub (List.nth roots i) (List.nth roots j) in
        prod := Complex.mul !prod (Complex.mul diff diff)
      done
    done;
    (* Multiply by lc^{2n-2} *)
    let lc_pow = Rational.pow_int lc (2 * n - 2) in
    let disc_val = Complex.mul
      { Complex.re = Rational.to_float lc_pow; im = 0.0 } !prod in
    (* Round: should be rational.
       disc(f) = a_n^{2n-2} prod_{i<j} (r_i - r_j)^2
       This is always the product form - no sign needed. *)
    approx_rat disc_val.Complex.re
  end

(** {2 Rationality tests} *)

(** Integer square root via Newton's method. *)
let integer_sqrt n =
  if n <= 0 then 0
  else
    let rec go x =
      let x' = (x + n / x) / 2 in
      if x' >= x then x
      else go x'
    in
    go (max 1 (int_of_float (sqrt (Float.of_int n))))

(** Test whether a non-negative integer is a perfect square. *)
let is_square_integer n =
  if n < 0 then false
  else if n = 0 then true
  else
    let s = integer_sqrt n in
    s * s = n

(** Test whether a rational number is a perfect square in Q. *)
let is_square_rational r =
  if Rational.is_neg r then false
  else if Rational.is_zero r then true
  else
    let n = Z.to_int (Rational.num (Rational.abs r)) in
    let d = Z.to_int (Rational.den r) in
    is_square_integer (abs n) && is_square_integer d

(** Check whether a polynomial has a rational root,
    using the rational root theorem.
    For polynomials with large coefficients, clears denominators
    and uses Z arithmetic to find integer numerators/denominators. *)
let has_rational_root f =
  try
    let cs = P.to_coeffs f in
    match cs with
    | [] -> true
    | _ ->
      let a0 = List.hd cs in
      let _an = List.nth cs (List.length cs - 1) in
      if Rational.is_zero a0 then true
      else begin
        let found = ref false in
        (* Clear all denominators to get integer coefficients *)
        let lcm_den = List.fold_left (fun acc c ->
          let d = Rational.den c in
          Z.lcm acc d
        ) Z.one cs in
        let int_cs = List.map (fun c ->
          Z.to_int (Rational.num (Rational.mul c (Rational.of_z lcm_den)))
        ) cs in
        let a0_int = abs (List.hd int_cs) in
        let an_int = abs (List.nth int_cs (List.length int_cs - 1)) in
        if a0_int = 0 then found := true
        else begin
          (* Divisors of a0 *)
          let p_divs = ref [] in
          let limit_a0 = min a0_int 10000 in
          for d = 1 to limit_a0 do
            if a0_int mod d = 0 then
              p_divs := d :: (-d) :: !p_divs
          done;
          (* Divisors of an *)
          let q_divs = ref [] in
          let limit_an = min an_int 10000 in
          for d = 1 to limit_an do
            if an_int mod d = 0 then
              q_divs := d :: !q_divs
          done;
          List.iter (fun p ->
            List.iter (fun q ->
              if not !found && q <> 0 then begin
                let r = Rational.of_ints p q in
                let v = P.eval f r in
                if Rational.is_zero v then found := true
              end
            ) !q_divs
          ) !p_divs
        end;
        !found
      end
  with Z.Overflow ->
    (* For very large coefficients, use Z arithmetic directly *)
    let cs = P.to_coeffs f in
    if Rational.is_zero (List.hd cs) then true
    else begin
      (* Clear denominators to get integer coefficients *)
      let lcm_den = List.fold_left (fun acc c ->
        Z.lcm acc (Rational.den c)
      ) Z.one cs in
      let z_cs = List.map (fun c ->
        Z.mul (Rational.num c) (Z.div lcm_den (Rational.den c))
      ) cs in
      let a0 = Z.abs (List.hd z_cs) in
      let an = Z.abs (List.nth z_cs (List.length z_cs - 1)) in
      let found = ref false in
      (* Try rational root theorem with Z arithmetic *)
      (* Divisors of a0: iterate up to min(a0, 10000) *)
      let limit_a0 = Z.min a0 (Z.of_int 10000) in
      let d = ref Z.one in
      while Z.leq !d limit_a0 && not !found do
        if Z.equal (Z.rem a0 !d) Z.zero then begin
          let limit_an = Z.min an (Z.of_int 10000) in
          let q = ref Z.one in
          while Z.leq !q limit_an && not !found do
            if Z.equal (Z.rem an !q) Z.zero then begin
              (* Try p/q and -p/q *)
              List.iter (fun sign ->
                if not !found then begin
                  let r = Rational.of_zs (Z.mul sign !d) !q in
                  let v = P.eval f r in
                  if Rational.is_zero v then found := true
                end
              ) [Z.one; Z.minus_one]
            end;
            q := Z.succ !q
          done
        end;
        d := Z.succ !d
      done;
      !found
    end

(** {2 Resolvent construction} *)

(** Build prod(x - r_i) from complex values and round to rational. *)
let round_to_rat_poly roots =
  let poly_c = List.fold_left (fun cs r ->
    let shifted = Complex.zero :: cs in
    let scaled = List.map (fun c -> Complex.mul (Complex.neg r) c) cs @ [Complex.zero] in
    let n = max (List.length shifted) (List.length scaled) in
    let get lst i = if i < List.length lst then List.nth lst i else Complex.zero in
    List.init n (fun i -> Complex.add (get shifted i) (get scaled i))
  ) [Complex.one] roots
  in
  let rounded = List.map (fun c ->
    if Float.abs c.Complex.im > 1e-4 *. Float.max 1.0 (Float.abs c.Complex.re) then
      None
    else
      Some (approx_rat c.Complex.re)
  ) poly_c in
  if List.exists Option.is_none rounded then None
  else Some (P.of_coeffs (List.filter_map Fun.id rounded))

(** Compute a resolvent polynomial from numerical roots and an invariant.

    [resolvent_from_roots roots theta perms] evaluates [theta] at
    each permutation of [roots] given by [perms], then builds the
    product polynomial and rounds to rational coefficients. *)
let resolvent_from_roots roots theta perms =
  let values = List.map (fun sigma ->
    theta (List.map (fun j -> List.nth roots j) sigma)
  ) perms in
  round_to_rat_poly values

(** {2 F_p polynomial arithmetic for Frobenius test} *)

(** Trim trailing zeros from a coefficient list. *)
let fp_trim cs =
  let rec go = function
    | [] -> []
    | x :: rest ->
      let rest' = go rest in
      if rest' = [] && x = 0 then []
      else x :: rest'
  in
  go cs

let fp_deg cs = List.length (fp_trim cs) - 1

let fp_add a b p =
  let n = max (List.length a) (List.length b) in
  let get lst i = if i < List.length lst then List.nth lst i else 0 in
  fp_trim (List.init n (fun i -> (get a i + get b i) mod p))

let fp_sub a b p =
  let n = max (List.length a) (List.length b) in
  let get lst i = if i < List.length lst then List.nth lst i else 0 in
  fp_trim (List.init n (fun i -> ((get a i - get b i) mod p + p) mod p))

let fp_mul a b p =
  match a, b with
  | [], _ | _, [] -> []
  | _ ->
    let na = List.length a in
    let nb = List.length b in
    let rlen = na + nb - 1 in
    fp_trim (List.init rlen (fun i ->
      let s = ref 0 in
      for j = 0 to i do
        if j < na && (i - j) < nb then
          s := (!s + List.nth a j * List.nth b (i - j)) mod p
      done;
      !s
    ))

(** Extended Euclidean algorithm for integers. *)
let rec e_gcd a b =
  if a = 0 then (b, 0, 1)
  else
    let (g, x, y) = e_gcd (b mod a) a in
    (g, y - (b / a) * x, x)

(** Modular multiplicative inverse. *)
let fp_inv a m =
  let (_, x, _) = e_gcd a m in
  ((x mod m) + m) mod m

let fp_make_monic cs p =
  match cs with
  | [] -> []
  | _ ->
    let lc = List.nth cs (List.length cs - 1) in
    let lc_inv = fp_inv lc p in
    List.map (fun c -> (c * lc_inv) mod p) cs

let rec fp_mod a b p =
  let ta = fp_trim a in
  let tb = fp_trim b in
  if fp_deg ta < fp_deg tb then
    fp_trim (List.map (fun x -> ((x mod p) + p) mod p) ta)
  else if tb = [] then failwith "fp_mod: division by zero"
  else
    let da = fp_deg ta in
    let _db = fp_deg tb in
    let lc_b = List.nth tb (List.length tb - 1) in
    let lc_b_inv = fp_inv lc_b p in
    let shift = da - fp_deg tb in
    let lc_a = List.nth ta (List.length ta - 1) in
    let fac = (lc_a * lc_b_inv) mod p in
    let n = List.length ta in
    let sub = List.init n (fun i ->
      let bi = if i >= shift && (i - shift) < List.length tb
        then List.nth tb (i - shift)
        else 0
      in
      (((List.nth ta i) - fac * bi) mod p + p) mod p
    ) in
    fp_mod (fp_trim sub) tb p

let fp_div a b p =
  let tb = fp_trim b in
  let db = fp_deg tb in
  let lc_b_inv = fp_inv (List.nth tb (List.length tb - 1)) p in
  let rec go q r =
    if fp_deg r < db then fp_trim q
    else
      let tr = fp_trim r in
      let dr = fp_deg tr in
      let shift = dr - db in
      let fac = (List.nth tr (List.length tr - 1) * lc_b_inv) mod p in
      let q' = fp_add q (List.init (shift + 1) (fun i -> if i = shift then fac else 0)) p in
      let sub = List.init (List.length tr) (fun i ->
        if i >= shift && (i - shift) < List.length tb
        then (fac * List.nth tb (i - shift)) mod p
        else 0
      ) in
      let r' = fp_trim (List.map2 (fun x y -> ((x - y) mod p + p) mod p) tr sub) in
      go q' r'
  in
  go [] (fp_trim a)

let rec fp_gcd a b p =
  let tb = fp_trim b in
  if tb = [] || fp_deg tb < 0 then fp_make_monic (fp_trim a) p
  else fp_gcd b (fp_mod a b p) p

(** Polynomial exponentiation modulo another polynomial over F_p. *)
let fp_pow_mod base expo modulus p =
  let rec go res b e =
    if e = 0 then res
    else
      let res' = if e mod 2 = 1 then fp_mod (fp_mul res b p) modulus p else res in
      let b' = fp_mod (fp_mul b b p) modulus p in
      go res' b' (e / 2)
  in
  go [1] base expo

(** Distinct-degree factorisation pattern of a polynomial over F_p. *)
let factor_pattern fcs p =
  let rec go degs k f h =
    if fp_deg f <= 0 then List.sort Int.compare degs
    else
      (* h <- h^p mod f *)
      let h' = fp_pow_mod h p f p in
      (* g = gcd(h' - x, f) mod p *)
      let hx = fp_sub h' [0; 1] p in
      let g = fp_gcd hx f p in
      let gd = fp_deg g in
      if gd = 0 then go degs (k + 1) f h'
      else
        let nf = gd / k in
        let f' = fp_div f g p in
        go (degs @ List.init nf (fun _ -> k)) (k + 1) f' h'
  in
  go [] 1 (fp_trim fcs) [0; 1]
