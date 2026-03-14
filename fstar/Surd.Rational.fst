/// Exact rational numbers with GCD normalization.
module Surd.Rational

open Surd.Ring

/// Absolute value.
let abs_int (x: int) : nat = if x >= 0 then x else 0 - x

/// Sign: -1, 0, or 1.
let sign_int (x: int) : int =
  if x > 0 then 1
  else if x < 0 then 0 - 1
  else 0

/// GCD via Euclidean algorithm on natural numbers.
let rec gcd_nat (a b : nat) : Tot nat (decreases b) =
  if b = 0 then a
  else gcd_nat b (a % b)

/// GCD of two integers (result is non-negative).
let gcd_int (a b : int) : nat =
  gcd_nat (abs_int a) (abs_int b)

/// A rational number p/q with q > 0 and gcd(|p|, q) = 1.
/// The zero rational is 0/1.
noeq type rational = {
  num: int;
  den: d:int{d > 0};
}

/// gcd_nat a b <= b when b > 0.
let rec gcd_nat_le_second (a b : nat)
  : Lemma (requires b > 0)
          (ensures gcd_nat a b <= b)
          (decreases b) =
  (* gcd(a, b) = gcd(b, a%b). a%b < b.
     If a%b = 0: gcd(b, 0) = b, so gcd(a,b) = b <= b. Done.
     If a%b > 0: gcd(b, a%b) and by IH on (b, a%b) with second arg a%b < b:
       but that gives gcd(b, a%b) <= a%b, which is <= b-1 < b. But IH
       requires (a%b) > 0, and we're in that case. Actually the IH gives
       gcd(b, a%b) <= a%b. Since a%b < b, we get gcd(b, a%b) < b. *)
  if a % b = 0 then ()
  else begin
    assert (a % b > 0);
    assert (a % b < b);
    gcd_nat_le_second b (a % b);
    (* IH gives: gcd(b, a%b) <= a%b. Since a%b < b, done. *)
    assert (gcd_nat b (a % b) <= a % b);
    assert (a % b < b)
  end

/// sign_int q * q > 0 when q <> 0.
let sign_mul_pos (q: int{q <> 0})
  : Lemma (op_Multiply (sign_int q) q > 0) =
  ()

/// Normalize a fraction: ensure den > 0 and gcd = 1.
let mk_rational (p q : int) : Pure rational (requires q <> 0) (ensures fun _ -> True) =
  let g = gcd_int p q in
  let g' : int = if g = 0 then 1 else g in
  let sn = sign_int q in
  let p' = op_Multiply sn p / g' in
  let sq = op_Multiply sn q in
  sign_mul_pos q;
  assert (sq > 0);
  assert (g' >= 1);
  FStar.Math.Lemmas.nat_over_pos_is_nat sq g';
  (* gcd(|p|, |q|) <= |q| = sq, since q <> 0 implies |q| > 0 *)
  gcd_nat_le_second (abs_int p) (abs_int q);
  assert (g <= sq);
  assert (g' <= sq);
  FStar.Math.Lemmas.lemma_div_le 1 sq g';
  let q' = sq / g' in
  assert (q' >= 1);
  { num = p'; den = q' }

/// Zero rational.
let rat_zero : rational = { num = 0; den = 1 }

/// One rational.
let rat_one : rational = { num = 1; den = 1 }

/// From integer.
let rat_of_int (n: int) : rational = { num = n; den = 1 }

/// Negation.
let rat_neg (a: rational) : rational = { num = 0 - a.num; den = a.den }

/// Addition: a/b + c/d = (ad + bc) / bd.
let rat_add (a b : rational) : rational =
  mk_rational (op_Multiply a.num b.den + op_Multiply b.num a.den)
              (op_Multiply a.den b.den)

/// Multiplication: (a/b)(c/d) = ac/bd.
let rat_mul (a b : rational) : rational =
  mk_rational (op_Multiply a.num b.num) (op_Multiply a.den b.den)

/// Equality: a/b = c/d iff ad = bc (both normalized, so structural also works).
let rat_eq (a b : rational) : bool =
  a.num = b.num && a.den = b.den

/// Comparison: a/b < c/d iff ad < bc (denominators positive).
let rat_lt (a b : rational) : bool =
  op_Multiply a.num b.den < op_Multiply b.num a.den

let rat_le (a b : rational) : bool = rat_eq a b || rat_lt a b

let rat_gt (a b : rational) : bool = rat_lt b a

let rat_ge (a b : rational) : bool = rat_le b a

/// Subtraction.
let rat_sub (a b : rational) : rational = rat_add a (rat_neg b)

/// Multiplicative inverse (requires nonzero).
let rat_inv (a: rational{a.num <> 0}) : rational =
  mk_rational a.den a.num

/// Division (requires nonzero divisor).
let rat_div (a: rational) (b: rational{b.num <> 0}) : rational =
  rat_mul a (rat_inv b)

/// Absolute value.
let rat_abs (a: rational) : rational =
  if a.num >= 0 then a else rat_neg a

/// Integer power (non-negative exponent).
let rec rat_pow (a: rational) (n: nat) : Tot rational (decreases n) =
  if n = 0 then rat_one
  else if n = 1 then a
  else
    let half = rat_pow a (n / 2) in
    let sq = rat_mul half half in
    if n % 2 = 0 then sq
    else rat_mul sq a

/// Ring instance for rational.
instance ring_rational : ring rational = {
  r_zero = rat_zero;
  r_one = rat_one;
  r_neg = rat_neg;
  r_eq = rat_eq;
  r_from_int = rat_of_int;
  r_add = rat_add;
  r_mul = rat_mul;
}

/// Field instance for rational.
/// f_inv and f_div handle zero defensively (return 0/1) since
/// the typeclass field type is k -> k (not refined for nonzero).
let rat_inv_total (a: rational) : rational =
  if a.num = 0 then rat_zero
  else rat_inv a

let rat_div_total (a b : rational) : rational =
  if b.num = 0 then rat_zero
  else rat_div a b

instance field_rational : field rational = {
  f_ring = ring_rational;
  f_inv = rat_inv_total;
  f_div = rat_div_total;
}

/// Minimum of two rationals.
let rat_min (a b : rational) : rational = if rat_le a b then a else b

/// Maximum of two rationals.
let rat_max (a b : rational) : rational = if rat_ge a b then a else b
