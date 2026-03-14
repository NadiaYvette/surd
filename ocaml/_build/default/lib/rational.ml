(** Exact rational arithmetic via Zarith.

    Invariant: [den > 0] and [gcd num den = 1] (canonical form).
    The zero rational is [{num = 0; den = 1}]. *)

type t = Q.t

let make n d = Q.make (Z.of_int n) (Z.of_int d)
let of_int n = Q.of_int n
let of_ints n d = Q.make (Z.of_int n) (Z.of_int d)
let of_z n = Q.of_bigint n
let of_zs n d = Q.make n d

let zero = Q.zero
let one = Q.one
let minus_one = Q.minus_one

let num t = Q.num t
let den t = Q.den t

let add = Q.add
let sub = Q.sub
let mul = Q.mul
let div = Q.div
let neg = Q.neg
let inv t = Q.div Q.one t
let abs = Q.abs

let equal = Q.equal
let compare = Q.compare

let is_zero t = Q.equal t Q.zero
let is_one t = Q.equal t Q.one
let is_neg t = Q.compare t Q.zero < 0
let is_pos t = Q.compare t Q.zero > 0
let is_integer t = Z.equal (Q.den t) Z.one

let to_int t =
  if is_integer t then Some (Z.to_int (Q.num t))
  else None

let to_float t = Q.to_float t

let to_string t =
  if is_integer t then Z.to_string (Q.num t)
  else
    let n = Q.num t in
    let d = Q.den t in
    if Z.sign n < 0 then
      Printf.sprintf "(%s/%s)" (Z.to_string n) (Z.to_string d)
    else
      Printf.sprintf "(%s/%s)" (Z.to_string n) (Z.to_string d)

let pp fmt t = Format.pp_print_string fmt (to_string t)

(** Exponentiation to a non-negative integer power. *)
let pow_int t n =
  if n < 0 then invalid_arg "Rational.pow_int: negative exponent"
  else
    let rec go acc k =
      if k = 0 then acc
      else go (Q.mul acc t) (k - 1)
    in
    go Q.one n

(** Exponentiation to any integer power (negative = invert first). *)
let pow_z t n =
  if n >= 0 then pow_int t n
  else inv (pow_int t (- n))

(** Floor of a rational. *)
let floor t = Z.fdiv (Q.num t) (Q.den t)

(** Ceiling of a rational. *)
let ceil t = Z.cdiv (Q.num t) (Q.den t)

(** {2 FIELD instance} *)

module Field : Field_sig.FIELD with type t = t = struct
  type nonrec t = t

  let zero = zero
  let one = one
  let add = add
  let sub = sub
  let mul = mul
  let neg = neg
  let inv = inv
  let div = div
  let equal = equal
  let compare = compare
  let of_int = of_int
  let to_string = to_string
end
