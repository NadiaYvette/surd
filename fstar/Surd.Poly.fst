/// Dense univariate polynomials over an arbitrary ring/field.
/// Representation: coefficient list, low-degree first.
/// Invariant: no trailing zeros (leading coefficient is nonzero).
/// The zero polynomial is [].
module Surd.Poly

open FStar.List.Tot
open Surd.Ring

/// A polynomial is a list of coefficients, low-degree first.
type poly (k:Type) = list k

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Strip trailing zeros (using ring equality).
/// Strategy: reverse, drop leading zeros, reverse back.
let strip_zeros (#k:Type) {| ring k |} (cs: list k) : list k =
  let rec drop_zeros (xs: list k) : Tot (list k) (decreases xs) =
    match xs with
    | [] -> []
    | x :: rest -> if r_eq x r_zero then drop_zeros rest else xs
  in
  rev (drop_zeros (rev cs))

/// Zip two lists with a binary operation, using a default for the shorter list.
let rec zip_with_default (#a:Type) (def: a) (f: a -> a -> a) (xs ys: list a)
  : Tot (list a) (decreases (length xs + length ys)) =
  match xs, ys with
  | [], [] -> []
  | x :: xs', [] -> f x def :: zip_with_default def f xs' []
  | [], y :: ys' -> f def y :: zip_with_default def f [] ys'
  | x :: xs', y :: ys' -> f x y :: zip_with_default def f xs' ys'

// ---------------------------------------------------------------------------
// Smart constructor
// ---------------------------------------------------------------------------

/// Build a polynomial from a coefficient list, stripping trailing zeros.
let mk_poly (#k:Type) {| ring k |} (cs: list k) : poly k =
  strip_zeros cs

/// The zero polynomial.
let zero_poly (#k:Type) : poly k = []

/// A constant polynomial.
let const_poly (#k:Type) {| ring k |} (c: k) : poly k =
  if r_eq c r_zero then [] else [c]

/// The polynomial x.
let mono_x (#k:Type) {| ring k |} : poly k = [r_zero; r_one]

// ---------------------------------------------------------------------------
// Inspection
// ---------------------------------------------------------------------------

/// Degree of a polynomial. Returns -1 for the zero polynomial.
let degree (#k:Type) (p: poly k) : int =
  length p - 1

/// Leading coefficient, or None for the zero polynomial.
let lead_coeff (#k:Type) (p: poly k) : option k =
  match rev p with
  | [] -> None
  | c :: _ -> Some c

// ---------------------------------------------------------------------------
// Arithmetic
// ---------------------------------------------------------------------------

/// Addition.
let add_poly (#k:Type) {| ring k |} (a b: poly k) : poly k =
  mk_poly (zip_with_default r_zero r_add a b)

/// Negation.
let neg_poly (#k:Type) {| ring k |} (p: poly k) : poly k =
  map r_neg p

/// Subtraction.
let sub_poly (#k:Type) {| ring k |} (a b: poly k) : poly k =
  add_poly a (neg_poly b)

/// Scalar multiplication.
let scale_poly (#k:Type) {| ring k |} (s: k) (p: poly k) : poly k =
  if r_eq s r_zero then zero_poly
  else mk_poly (map (r_mul s) p)

/// Evaluate at a point via Horner's method.
let eval_poly (#k:Type) {| ring k |} (p: poly k) (x: k) : k =
  fold_right (fun c acc -> r_add c (r_mul x acc)) p r_zero

/// Multiply by x^n (prepend n zeros).
let rec shift_poly (#k:Type) {| ring k |} (n: nat) (p: poly k) : Tot (poly k) (decreases n) =
  if n = 0 then p
  else r_zero :: shift_poly (n - 1) p

/// Polynomial multiplication via schoolbook algorithm.
let mul_poly (#k:Type) {| ring k |} (a b: poly k) : poly k =
  let rec go (a: list k) (i: nat) : Tot (poly k) (decreases a) =
    match a with
    | [] -> zero_poly
    | c :: rest ->
      add_poly (shift_poly i (scale_poly c b)) (go rest (i + 1))
  in
  go a 0

// ---------------------------------------------------------------------------
// Division (requires field)
// ---------------------------------------------------------------------------

/// Polynomial division with remainder: div_mod_poly f g = (q, r)
/// such that f = g*q + r and degree r < degree g.
/// Requires nonzero divisor.
val div_mod_poly : #k:Type -> {| field k |} -> poly k -> (g:poly k{Cons? g}) -> Dv (poly k & poly k)
let div_mod_poly #k #d f g =
  let rng : ring k = f_ring #k in
  let dg = degree g in
  let lc_g = match lead_coeff g with | Some c -> c | None -> rng.r_one in
  let rec go (q r : poly k) : Dv (poly k & poly k) =
    if degree r < dg then (q, r)
    else
      let dr = degree r in
      let lr = match lead_coeff r with | Some c -> c | None -> rng.r_zero in
      let c = f_div lr lc_g in
      let dd = dr - dg in
      let term : poly k = if dd >= 0 then shift_poly dd (const_poly #k #rng c) else const_poly #k #rng c in
      let r' = sub_poly #k #rng r (mul_poly #k #rng term g) in
      go (add_poly #k #rng q term) r'
  in
  go (zero_poly #k) f

/// Make a polynomial monic (leading coefficient 1).
let monic_poly (#k:Type) {| field k |} (p: poly k) : poly k =
  match lead_coeff p with
  | None -> zero_poly
  | Some lc -> map (fun c -> f_div c lc) p

/// GCD via Euclidean algorithm, made monic.
val gcd_poly : #k:Type -> {| field k |} -> poly k -> poly k -> Dv (poly k)
let rec gcd_poly #k #d a b =
  match b with
  | [] -> monic_poly a
  | _ -> let (_, r) = div_mod_poly a b in gcd_poly b r

/// Formal derivative: d/dx (a0 + a1*x + a2*x^2 + ...) = a1 + 2*a2*x + ...
let diff_poly (#k:Type) {| ring k |} (p: poly k) : poly k =
  match p with
  | [] -> []
  | _ :: rest ->
    let rec go (cs: list k) (i: int) : Tot (list k) (decreases cs) =
      match cs with
      | [] -> []
      | c :: cs' -> r_mul (r_from_int i) c :: go cs' (i + 1)
    in
    mk_poly (go rest 1)

/// Composition: compose_poly f g = f(g(x)).
let compose_poly (#k:Type) {| ring k |} (f g: poly k) : poly k =
  fold_right
    (fun c acc -> add_poly (const_poly c) (mul_poly g acc))
    f
    (zero_poly #k)

/// Coefficient-wise equality for polynomials using the ring's r_eq.
let rec poly_eq (#k:Type) {| ring k |} (a b: poly k) : Tot bool (decreases (length a + length b)) =
  match a, b with
  | [], [] -> true
  | x :: xs, y :: ys -> r_eq x y && poly_eq xs ys
  | _, _ -> false

/// Ring instance for polynomials (allows p + q, p * q syntax in typed contexts).
instance ring_poly (k:Type) {| ring k |} : ring (poly k) = {
  r_zero = zero_poly;
  r_one = [r_one];
  r_neg = neg_poly;
  r_eq = poly_eq;
  r_from_int = (fun n -> const_poly (r_from_int n));
  r_add = add_poly;
  r_mul = mul_poly;
}

/// Square-free factorisation via Yun's algorithm. Returns (factor, multiplicity) pairs.
/// Requires characteristic 0 field.
val square_free : #k:Type -> {| field k |} -> poly k -> Dv (list (poly k & int))
let square_free #k #d f =
  let rng : ring k = f_ring #k in
  match f with
  | [] -> []
  | _ ->
    let f' = diff_poly #k #rng f in
    let c = gcd_poly f f' in
    match c with
    | [] -> [(f, 1)]  (* gcd is zero means f' was zero, f is a constant — shouldn't happen for non-const f *)
    | _ ->
      let (w, _) = div_mod_poly f c in
      let rec go (w c: poly k) (i: int) : Dv (list (poly k & int)) =
        if degree w = 0 then
          if degree c > 0 then [(c, i)]
          else []
        else
          match c with
          | [] -> if degree w > 0 then [(w, i)] else []
          | _ ->
            let y = gcd_poly w c in
            match y with
            | [] -> if degree w > 0 then [(w, i)] else []
            | _ ->
              let (z, _) = div_mod_poly w y in
              let (c', _) = div_mod_poly c y in
              let rest = go y c' (i + 1) in
              if degree z > 0 then (z, i) :: rest
              else rest
      in
      go w c 1
