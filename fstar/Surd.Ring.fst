/// Ring and field typeclasses for abstract algebra over coefficient types.
module Surd.Ring

/// A commutative ring with identity.
class ring (k:Type) = {
  r_zero: k;
  r_one: k;
  r_neg: k -> k;
  r_eq: k -> k -> bool;
  r_from_int: int -> k;
  r_add: k -> k -> k;
  r_mul: k -> k -> k;
}

/// Subtraction derived from addition and negation.
let r_sub (#k:Type) {| ring k |} (a b : k) : k = r_add a (r_neg b)

/// A field extends a ring with multiplicative inverse and division.
class field (k:Type) = {
  f_ring: ring k;
  f_inv: k -> k;
  f_div: k -> k -> k;
}

/// Convenience: extract the ring from a field instance.
let field_to_ring (#k:Type) {| field k |} : ring k = f_ring

/// Integer ring instance.
let int_neg (a:int) : int = 0 - a
let int_eq (a b : int) : bool = a = b
let int_add (a b : int) : int = a + b
let int_mul (a b : int) : int = op_Multiply a b

instance ring_int : ring int = {
  r_zero = 0;
  r_one = 1;
  r_neg = int_neg;
  r_eq = int_eq;
  r_from_int = (fun (n:int) -> n);
  r_add = int_add;
  r_mul = int_mul;
}

/// Sum a list of ring elements.
let r_sum (#k:Type) {| ring k |} (xs: list k) : k =
  FStar.List.Tot.fold_left r_add r_zero xs

/// Product of a list of ring elements.
let r_product (#k:Type) {| ring k |} (xs: list k) : k =
  FStar.List.Tot.fold_left r_mul r_one xs

/// Repeated squaring for non-negative integer powers.
let rec r_pow (#k:Type) {| ring k |} (base: k) (n: nat) : Tot k (decreases n) =
  if n = 0 then r_one
  else if n = 1 then base
  else
    let half = r_pow base (n / 2) in
    let sq = r_mul half half in
    if n % 2 = 0 then sq
    else r_mul sq base
