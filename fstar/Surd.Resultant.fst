/// Resultant computation, Lagrange interpolation, and composed-sum/product
/// polynomials over an arbitrary field k.
module Surd.Resultant

open FStar.List.Tot
open Surd.Ring
open Surd.Rational
open Surd.Poly

/// Resultant of two univariate polynomials via the Euclidean algorithm.
val poly_resultant : #k:Type -> {| field k |} -> poly k -> poly k -> Dv k
let rec poly_resultant #k #d f g =
  let rng : ring k = f_ring #k in
  let df = degree f in
  let dg = degree g in
  if df < 0 || dg < 0 then rng.r_zero
  else if dg = 0 then
    match lead_coeff g with
    | Some c -> r_pow #k #rng c (if df >= 0 then df else 0)
    | None -> rng.r_zero
  else
    let (_, r) = div_mod_poly f g in
    let lg = match lead_coeff g with | Some c -> c | None -> rng.r_one in
    let dr = degree r in
    if dr < 0 then rng.r_zero
    else
      let sign = if (op_Multiply df dg) % 2 = 1 then rng.r_neg rng.r_one else rng.r_one in
      let pow_exp : nat = if df - dr >= 0 then df - dr else 0 in
      rng.r_mul sign (rng.r_mul (r_pow #k #rng lg pow_exp) (poly_resultant g r))

/// Zip two lists into pairs (truncates to shorter).
let rec zip_lists (#a #b: Type) (xs: list a) (ys: list b) : Tot (list (a & b)) (decreases xs) =
  match xs, ys with
  | x :: xs', y :: ys' -> (x, y) :: zip_lists xs' ys'
  | _, _ -> []

/// Lagrange interpolation: given points (x_i, y_i), compute the unique
/// polynomial passing through all points.
val lagrange_interpolate : #k:Type -> {| field k |} -> list (k & k) -> Dv (poly k)
let lagrange_interpolate #k #d points =
  let rng : ring k = f_ring #k in
  let xs = map fst points in
  let rec basis (xi: k) (others: list k) : Dv (poly k) =
    match others with
    | [] -> [rng.r_one]
    | xj :: rest ->
      if rng.r_eq xi xj then basis xi rest
      else
        let denom = rng.r_add xi (rng.r_neg xj) in
        let inv_d = f_inv denom in
        let factor : poly k = [rng.r_mul inv_d (rng.r_neg xj); inv_d] in
        mul_poly #k #rng factor (basis xi rest)
  in
  let rec sum_terms (pts: list (k & k)) : Dv (poly k) =
    match pts with
    | [] -> zero_poly #k
    | (xi, yi) :: rest ->
      let b = basis xi xs in
      add_poly #k #rng (scale_poly #k #rng yi b) (sum_terms rest)
  in
  sum_terms points

/// Substitute a + b*y for x in polynomial p(x), yielding p(a + b*y) in y.
val substitute_linear : #k:Type -> {| field k |} -> poly k -> k -> k -> Dv (poly k)
let substitute_linear #k #d p a b =
  let rng : ring k = f_ring #k in
  let binomial : poly k = [a; b] in
  fold_right
    (fun c acc -> add_poly #k #rng (const_poly #k #rng c) (mul_poly #k #rng binomial acc))
    p
    (zero_poly #k)

/// Composed sum polynomial.
val composed_sum : #k:Type -> {| field k |} -> poly k -> poly k -> Dv (poly k)
let composed_sum #k #d p q =
  let rng : ring k = f_ring #k in
  let dp = degree p in
  let dq = degree q in
  let result_deg = op_Multiply dp dq in
  let rec mk_points (i: nat) : Tot (list k) (decreases (result_deg + 1 - i)) =
    if i > result_deg then []
    else rng.r_from_int i :: mk_points (i + 1)
  in
  let points = mk_points 0 in
  let rec eval_points (ps: list k) : Dv (list (k & k)) =
    match ps with
    | [] -> []
    | x0 :: rest ->
      let q_shifted = substitute_linear q x0 (rng.r_neg rng.r_one) in
      let val_ = poly_resultant p q_shifted in
      (x0, val_) :: eval_points rest
  in
  lagrange_interpolate (eval_points points)

/// Negate the variable: p(-x).
let negate_var (#k:Type) {| ring k |} (p: poly k) : poly k =
  let rec go (cs: list k) (i: nat) : Tot (list k) (decreases cs) =
    match cs with
    | [] -> []
    | c :: rest ->
      (if i % 2 = 1 then r_neg c else c) :: go rest (i + 1)
  in
  mk_poly (go p 0)

/// Reciprocal polynomial: x^n * p(1/x).
let reciprocal_poly (#k:Type) {| ring k |} (p: poly k) : poly k =
  mk_poly (rev p)

/// Substitute x^n for x in p.
let substitute_x_n (#k:Type) {| ring k |} (n: nat{n >= 1}) (p: poly k) : poly k =
  let rec go (cs: list k) (i: nat) : Tot (poly k) (decreases cs) =
    match cs with
    | [] -> zero_poly #k
    | [c] -> shift_poly (op_Multiply i n) (const_poly c)
    | c :: rest ->
      add_poly (shift_poly (op_Multiply i n) (const_poly c)) (go rest (i + 1))
  in
  go p 0

/// Shift a polynomial: f(x + a).
val shift_polynomial : #k:Type -> {| field k |} -> poly k -> k -> Dv (poly k)
let shift_polynomial #k #d p a =
  let rng : ring k = f_ring #k in
  let x_plus_a : poly k = [a; rng.r_one] in
  fold_right
    (fun c acc -> add_poly #k #rng (const_poly #k #rng c) (mul_poly #k #rng x_plus_a acc))
    p
    (zero_poly #k)
