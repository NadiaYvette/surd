/// Galois group identification for polynomials of any prime degree.
///
/// For degree <= 4: uses discriminant and resolvent polynomials.
/// For degree 5: uses discriminant + sextic resolvent + Frobenius test.
/// For prime degree p >= 5: uses Frobenius/Chebotarev factorisation
///   patterns mod small primes to identify the group within the
///   lattice of transitive subgroups of S_p.
module Surd.Identify

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly
open Surd.Factoring
open Surd.Resolvent
open Surd.TransitiveGroup
open Surd.PrimeFactors

/// --------------------------------------------------------------------------
/// Helpers
/// --------------------------------------------------------------------------

/// Replicate a value n times.
let rec replicate_int (n: nat) (x: int) : Tot (list int) (decreases n) =
  if n = 0 then []
  else x :: replicate_int (n - 1) x

/// Check if a rational is a perfect square.
val is_perfect_square_rat : rational -> bool
let is_perfect_square_rat r =
  if rat_lt r rat_zero then false
  else if rat_eq r rat_zero then true
  else
    let n = Surd.Rational.abs_int r.num in
    let d = r.den in
    let rec isqrt (x k: nat) : Tot bool (decreases (x - k)) =
      if op_Multiply k k = x then true
      else if op_Multiply k k > x || k > x then false
      else isqrt x (k + 1)
    in
    isqrt n 0 && isqrt d 0

/// Small primes for Frobenius/Chebotarev test.
let small_primes : list int =
  [3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61;
   67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113]

/// --------------------------------------------------------------------------
/// F_p polynomial arithmetic (ascending coefficient lists)
/// --------------------------------------------------------------------------

/// Remove trailing zeros.
let fp_trim (cs: list int) : list int =
  let rec drop_leading_zeros (xs: list int) : Tot (list int) (decreases xs) =
    match xs with
    | [] -> []
    | x :: rest -> if x = 0 then drop_leading_zeros rest else xs
  in
  rev (drop_leading_zeros (rev cs))

/// Degree of a polynomial over F_p.
let fp_deg (cs: list int) : int =
  length (fp_trim cs) - 1

/// Pad a list to length n with zeros.
let pad_to (xs: list int) (n: nat) : list int =
  let diff = n - length xs in
  let diff' : nat = if diff > 0 then diff else 0 in
  xs @ replicate_int diff' 0

/// Zip two lists with a function, treating missing elements as 0.
let rec zip_with_zero (f: int -> int -> int) (a b: list int) : Tot (list int) (decreases (length a + length b)) =
  match a, b with
  | [], [] -> []
  | x :: xs, [] -> f x 0 :: zip_with_zero f xs []
  | [], y :: ys -> f 0 y :: zip_with_zero f [] ys
  | x :: xs, y :: ys -> f x y :: zip_with_zero f xs ys

/// Addition mod p.
let fp_add (a b: list int) (p: int) : list int =
  fp_trim (zip_with_zero (fun x y -> (x + y) % p) a b)

/// Subtraction mod p.
let fp_sub (a b: list int) (p: int) : list int =
  fp_trim (zip_with_zero (fun x y -> ((x - y) % p + p) % p) a b)

/// Multiplication mod p (schoolbook convolution).
val fp_mul : list int -> list int -> int -> Dv (list int)
let fp_mul a b p =
  let a' = fp_trim a in
  let b' = fp_trim b in
  let na = length a' in
  let nb = length b' in
  if na = 0 || nb = 0 then []
  else
    let rec build_coeff (i: nat) : Dv (list int) =
      if i >= na + nb - 1 then []
      else
        let rec sum_j (j: nat) (acc: int) : Dv int =
          if j > i then acc
          else
            let term =
              if j < na && i - j < nb then
                (match nth a' j, nth b' (i - j) with
                 | Some aj, Some bij -> op_Multiply aj bij % p
                 | _, _ -> 0)
              else 0
            in
            sum_j (j + 1) ((acc + term) % p)
        in
        sum_j 0 0 :: build_coeff (i + 1)
    in
    fp_trim (build_coeff 0)

/// Modular inverse via extended Euclidean algorithm.
val fp_inv_elem : int -> int -> int
let fp_inv_elem a m =
  let rec egcd (a' b' : int) : Tot (int & int & int) (decreases (if b' >= 0 then b' else 0 - b')) =
    if a' = 0 then (b', 0, 1)
    else
      let (g, x, y) = egcd (b' % a') a' in
      (g, y - op_Multiply (b' / a') x, x)
  in
  let a_pos = ((a % m) + m) % m in
  if a_pos = 0 then 0
  else
    let (_, x, _) = egcd a_pos m in
    ((x % m) + m) % m

/// Make polynomial monic over F_p.
let fp_make_monic (cs: list int) (p: int) : list int =
  let cs' = fp_trim cs in
  match rev cs' with
  | [] -> []
  | lc :: _ ->
    let lc_inv = fp_inv_elem lc p in
    map (fun c -> op_Multiply c lc_inv % p) cs'

/// Polynomial remainder over F_p.
val fp_mod : list int -> list int -> int -> Dv (list int)
let rec fp_mod a b p =
  let ta = fp_trim a in
  let tb = fp_trim b in
  let da = fp_deg ta in
  let db = fp_deg tb in
  if da < db then fp_trim (map (fun x -> ((x % p) + p) % p) ta)
  else if length tb = 0 then ta  // division by zero guard
  else
    match rev ta, rev tb with
    | lca :: _, lcb :: _ ->
      let lc_inv = fp_inv_elem lcb p in
      let shift = da - db in
      let fac = op_Multiply lca lc_inv % p in
      let sub_len = length ta in
      let rec build_sub (i: nat) : Tot (list int) (decreases (sub_len - i)) =
        if i >= sub_len then []
        else
          let ai = (match nth ta i with | Some v -> v | None -> 0) in
          let bi =
            if i >= shift && i - shift < length tb then
              (match nth tb (i - shift) with | Some v -> v | None -> 0)
            else 0
          in
          ((ai - op_Multiply fac bi) % p + p) % p :: build_sub (i + 1)
      in
      fp_mod (fp_trim (build_sub 0)) tb p
    | _, _ -> ta

/// GCD of two polynomials over F_p.
val fp_gcd : list int -> list int -> int -> Dv (list int)
let rec fp_gcd a b p =
  let tb = fp_trim b in
  if length tb = 0 || fp_deg tb < 0 then fp_make_monic (fp_trim a) p
  else fp_gcd b (fp_mod a b p) p

/// Polynomial power mod over F_p: base^expo mod modulus.
val fp_pow_mod : list int -> int -> list int -> int -> Dv (list int)
let rec fp_pow_mod base expo modulus p =
  if expo <= 0 then [1]
  else if expo = 1 then fp_mod base modulus p
  else
    let half = fp_pow_mod base (expo / 2) modulus p in
    let sq = fp_mod (fp_mul half half p) modulus p in
    if expo % 2 = 0 then sq
    else fp_mod (fp_mul sq base p) modulus p

/// Polynomial exact division over F_p.
val fp_div : list int -> list int -> int -> Dv (list int)
let fp_div a b p =
  let tb = fp_trim b in
  let db = fp_deg tb in
  let lcb_inv = match rev tb with
    | lc :: _ -> fp_inv_elem lc p
    | [] -> 1
  in
  let rec go (q: list int) (r: list int) : Dv (list int) =
    let tr = fp_trim r in
    let dr = fp_deg tr in
    if dr < db then fp_trim q
    else
      match rev tr with
      | lcr :: _ ->
        let shift = dr - db in
        let fac = op_Multiply lcr lcb_inv % p in
        let shift' : nat = if shift > 0 then shift else 0 in
        let q_term = replicate_int shift' 0 @ [fac] in
        let q' = fp_add q q_term p in
        let sub_len = length tr in
        let rec build_sub (i: nat) : Tot (list int) (decreases (sub_len - i)) =
          if i >= sub_len then []
          else
            let ri = (match nth tr i with | Some v -> v | None -> 0) in
            let bi =
              if i >= shift && i - shift < length tb then
                (match nth tb (i - shift) with | Some v -> v | None -> 0)
              else 0
            in
            ((ri - op_Multiply fac bi) % p + p) % p :: build_sub (i + 1)
        in
        go q' (fp_trim (build_sub 0))
      | [] -> fp_trim q
  in
  go [] (fp_trim a)

/// --------------------------------------------------------------------------
/// Factorisation pattern mod p (distinct-degree factorisation)
/// --------------------------------------------------------------------------

/// Compute the factorisation pattern of a polynomial over F_p.
/// Returns the sorted list of irreducible factor degrees.
val factor_pattern : list int -> int -> Dv (list int)
let factor_pattern fcs p =
  let rec go (degs: list int) (k: int) (f: list int) (h: list int) : Dv (list int) =
    if fp_deg f <= 0 then sortWith (fun (a: int) (b: int) -> a - b) degs
    else
      // h <- h^p mod f
      let h' = fp_pow_mod h p f p in
      // g = gcd(h' - x, f) mod p
      let hx = fp_sub h' [0; 1] p in
      let g = fp_gcd hx f p in
      let gd = fp_deg g in
      if gd = 0 then go degs (k + 1) f h'
      else
        let nf = gd / k in
        let f' = fp_div f g p in
        let rec make_degs (n: nat) : list int =
          if n = 0 then []
          else k :: make_degs (n - 1)
        in
        go (degs @ make_degs (if nf >= 0 then nf else 0)) (k + 1) f' h'
  in
  go [] 1 (fp_trim fcs) [0; 1]

/// --------------------------------------------------------------------------
/// Degree 2, 3, 4 identification (kept for backward compat)
/// --------------------------------------------------------------------------

let identify_degree_2 (f: poly rational) : Dv transitive_group_id =
  let disc = poly_discriminant f in
  if is_perfect_square_rat disc then TG_Cn 1
  else TG_Cn 2

val identify_degree_3 : poly rational -> Dv transitive_group_id
let identify_degree_3 f =
  let factors = factor_poly f in
  let has_linear = existsb (fun p -> degree p = 1) factors in
  if has_linear then TG_Cn 2
  else
    let disc = poly_discriminant f in
    if is_perfect_square_rat disc then TG_An 3
    else TG_Sn 3

val identify_degree_4 : poly rational -> Dv transitive_group_id
let identify_degree_4 f =
  let factors = factor_poly f in
  let has_linear = existsb (fun p -> degree p = 1) factors in
  if has_linear then TG_Sn 3
  else
    let disc = poly_discriminant f in
    match f with
    | [r; q; p; _; _] ->
      let resolvent = cubic_resolvent_quartic p q r in
      let res_factors = factor_poly resolvent in
      let n_linear = length (filter (fun p -> degree p = 1) res_factors) in
      if n_linear = 3 then
        if is_perfect_square_rat disc then TG_V4
        else TG_Dn 4
      else if n_linear = 1 then
        if is_perfect_square_rat disc then TG_An 4
        else TG_Sn 4
      else TG_Sn 4
    | _ -> TG_Sn 4

/// --------------------------------------------------------------------------
/// Degree 5 identification (fast path)
/// --------------------------------------------------------------------------

val identify_degree_5 : poly rational -> Dv transitive_group_id
let identify_degree_5 f =
  let factors = factor_poly f in
  let has_linear = existsb (fun p -> degree p = 1) factors in
  if has_linear then TG_Sn 4
  else
    let disc = poly_discriminant f in
    if is_perfect_square_rat disc then TG_An 5
    else
      let sex = sextic_resolvent f in
      let sex_factors = factor_poly sex in
      let has_sex_root = existsb (fun p -> degree p = 1) sex_factors in
      if has_sex_root then TG_F20
      else TG_Sn 5

/// --------------------------------------------------------------------------
/// General prime-degree Galois group identification
/// --------------------------------------------------------------------------

/// Identify the Galois group of an irreducible polynomial of prime degree p
/// via Frobenius/Chebotarev factorisation patterns.
///
/// Strategy:
/// 1. Compute factorisation patterns of f mod small primes.
/// 2. Check if all patterns are consistent with AGL(1,p).
///    - Translation: [p]
///    - Identity: [1,1,...,1]
///    - Non-translation: [1, k, k, ..., k] where k | p-1
/// 3. If not in AGL: either A_p or S_p (discriminant test).
/// 4. If in AGL: find the minimum stabiliser size d = lcm of observed
///    non-trivial cycle lengths. The group is Z/p ⋊ Z/d.
val identify_prime_degree : poly rational -> Dv transitive_group_id
let identify_prime_degree f =
  let n = degree f in
  let p : int = n in
  let disc = poly_discriminant f in
  let disc_sq = is_perfect_square_rat disc in

  // Prepare integer coefficients
  let cs = f in
  let rec lcm_dens (xs: list rational) (acc: int) : int =
    match xs with
    | [] -> acc
    | x :: rest ->
      let g = Surd.Rational.gcd_int acc x.den in
      let g' = if g = 0 then 1 else g in
      lcm_dens rest (op_Multiply acc (x.den / g'))
  in
  let lcm_den = lcm_dens cs 1 in
  let int_cs = map (fun (c: rational) ->
    op_Multiply c.num (lcm_den / c.den)) cs
  in
  let lc = match rev int_cs with | c :: _ -> c | [] -> 1 in
  let disc_n = disc.num in
  let disc_d = disc.den in

  let good_prime (pr: int) : bool =
    lc % pr <> 0 && disc_n % pr <> 0 && disc_d % pr <> 0
  in
  let test_primes = let rec take_n (xs: list int) (k: nat) : list int =
    if k = 0 then []
    else match xs with
      | [] -> []
      | x :: rest -> if good_prime x then x :: take_n rest (k - 1) else take_n rest k
  in
  take_n small_primes 50
  in

  // Collect factorisation patterns
  let patterns : list (list int) =
    map (fun (pr: int) ->
      let mod_cs = map (fun (c: int) -> ((c % pr) + pr) % pr) int_cs in
      factor_pattern mod_cs pr
    ) test_primes
  in

  // Check if a pattern is consistent with AGL(1,p)
  let all_equal (xs: list int) : bool =
    match xs with
    | [] -> true
    | x :: rest -> for_all (fun y -> y = x) rest
  in
  let is_agl_pattern (pat: list int) : bool =
    pat = [n]                                     // translation: [p]
    || pat = replicate_int n 1        // identity: [1,...,1]
    || (length pat >= 2
        && (match pat with | [] -> false | x :: _ -> x = 1)
        && length (filter (fun x -> x = 1) pat) = 1
        && all_equal (filter (fun x -> x <> 1) pat))
  in

  let inside_agl = for_all is_agl_pattern patterns in

  // Find minimum d: lcm of all observed non-trivial cycle lengths
  let non_triv_cycle_lengths : list int =
    let rec collect_pats (ps: list (list int)) : list int =
      match ps with
      | [] -> []
      | pat :: rest ->
        if pat = [n] || pat = replicate_int n 1 then
          collect_pats rest
        else
          filter (fun k -> k <> 1) pat @ collect_pats rest
    in
    collect_pats patterns
  in

  let rec lcm_list (xs: list int) : int =
    match xs with
    | [] -> 1
    | [x] -> if x > 0 then x else 1
    | x :: rest ->
      let l = lcm_list rest in
      let x' = if x > 0 then x else 1 in
      let g = Surd.Rational.gcd_int x' l in
      let g' = if g = 0 then 1 else g in
      op_Multiply x' (l / g')
  in

  let min_d = if length non_triv_cycle_lengths = 0 then 1
              else lcm_list non_triv_cycle_lengths
  in

  if not inside_agl then
    // Not in AGL(1,p): either A_p or S_p
    if disc_sq then TG_An n
    else TG_Sn n
  else
    // Inside AGL(1,p): find the smallest valid divisor d >= min_d
    let ds = divisors (p - 1) in
    let valid_ds = filter (fun d -> d >= min_d) ds in
    let group_d = match valid_ds with
      | d :: _ -> d
      | [] -> p - 1  // fallback to full AGL
    in
    if group_d = 1 then TG_Cn n
    else if group_d = 2 then TG_Dn n
    else TG_Aff n group_d

/// --------------------------------------------------------------------------
/// Main entry point
/// --------------------------------------------------------------------------

/// Identify the Galois group of an irreducible polynomial.
/// Supports all degrees <= 5 and all prime degrees.
val identify_galois_group : poly rational -> Dv transitive_group_id
let identify_galois_group f =
  let d = degree f in
  if d <= 1 then TG_Cn 1
  else if d = 2 then identify_degree_2 f
  else if d = 3 then identify_degree_3 f
  else if d = 4 then identify_degree_4 f
  else if d = 5 then identify_degree_5 f
  else if is_prime d then identify_prime_degree f
  else TG_Other d 0
