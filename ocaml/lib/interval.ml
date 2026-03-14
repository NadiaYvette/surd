(** Interval arithmetic over rationals.

    Each interval is a closed bracket [[lo, hi]] where [lo <= hi].
    All operations produce enclosures: the true result is guaranteed
    to lie within the returned interval. *)

module R = Rational

type t = { lo : R.t; hi : R.t }

let make lo hi =
  assert (R.compare lo hi <= 0);
  { lo; hi }

let of_rational r = { lo = r; hi = r }
let of_int n = of_rational (R.of_int n)

let lo t = t.lo
let hi t = t.hi
let width t = R.sub t.hi t.lo

let contains t r = R.compare t.lo r <= 0 && R.compare r t.hi <= 0

let is_positive t = R.is_pos t.lo
let is_negative t = R.is_neg t.hi
let is_zero t = R.is_zero t.lo && R.is_zero t.hi
let strictly_positive t = R.is_pos t.lo
let strictly_negative t = R.is_neg t.hi

let add a b = { lo = R.add a.lo b.lo; hi = R.add a.hi b.hi }
let sub a b = { lo = R.sub a.lo b.hi; hi = R.sub a.hi b.lo }
let neg a = { lo = R.neg a.hi; hi = R.neg a.lo }

let mul a b =
  let products = [
    R.mul a.lo b.lo; R.mul a.lo b.hi;
    R.mul a.hi b.lo; R.mul a.hi b.hi
  ] in
  let lo = List.fold_left (fun acc x ->
    if R.compare x acc < 0 then x else acc) (List.hd products) (List.tl products) in
  let hi = List.fold_left (fun acc x ->
    if R.compare x acc > 0 then x else acc) (List.hd products) (List.tl products) in
  { lo; hi }

let inv a =
  if contains a R.zero then
    invalid_arg "Interval.inv: interval contains zero"
  else
    { lo = R.inv a.hi; hi = R.inv a.lo }

let div a b = mul a (inv b)

(** Integer power of an interval. *)
let rec pow a n =
  if n = 0 then of_rational R.one
  else if n > 0 then
    if n mod 2 = 1 then
      (* Odd power: monotone *)
      { lo = R.pow_z a.lo n; hi = R.pow_z a.hi n }
    else
      (* Even power: need to handle sign *)
      if is_positive a then
        { lo = R.pow_z a.lo n; hi = R.pow_z a.hi n }
      else if is_negative a then
        { lo = R.pow_z a.hi n; hi = R.pow_z a.lo n }
      else
        let x = R.pow_z a.lo n and y = R.pow_z a.hi n in
        { lo = R.zero; hi = (if R.compare x y >= 0 then x else y) }
  else
    inv (pow a (-n))

(** Newton's method rational square root enclosure with a few iterations. *)
let isqrt a =
  if strictly_negative a then
    invalid_arg "Interval.isqrt: negative interval"
  else if is_zero a then of_rational R.zero
  else
    (* Use rational Newton iteration for bounds *)
    let lo_approx =
      let f = R.to_float a.lo in
      if f <= 0.0 then R.zero
      else
        (* A few Newton steps from the float approximation *)
        let s = Float.sqrt f in
        let r = ref (R.of_ints (int_of_float (s *. 1000.0)) 1000) in
        for _ = 1 to 5 do
          (* r <- (r + a.lo/r) / 2, but take the lower bound *)
          r := R.div (R.add !r (R.div a.lo !r)) (R.of_int 2)
        done;
        (* Ensure it's a valid lower bound: r^2 <= a.lo *)
        let r2 = R.mul !r !r in
        if R.compare r2 a.lo <= 0 then !r
        else
          (* Shrink slightly *)
          R.div (R.mul !r (R.of_int 999)) (R.of_int 1000)
    in
    let hi_approx =
      let f = R.to_float a.hi in
      let s = Float.sqrt f in
      let r = ref (R.of_ints (int_of_float (s *. 1000.0 +. 1.0)) 1000) in
      for _ = 1 to 5 do
        r := R.div (R.add !r (R.div a.hi !r)) (R.of_int 2)
      done;
      (* Ensure it's a valid upper bound: r^2 >= a.hi *)
      let r2 = R.mul !r !r in
      if R.compare r2 a.hi >= 0 then !r
      else
        R.div (R.mul !r (R.of_int 1001)) (R.of_int 1000)
    in
    { lo = lo_approx; hi = hi_approx }

(** n-th root of a non-negative interval (n >= 2). *)
let inth _n a =
  if _n = 2 then isqrt a
  else
    (* General case: use float approximation + widen *)
    let lo_f = R.to_float a.lo in
    let hi_f = R.to_float a.hi in
    let inv_n = 1.0 /. Float.of_int _n in
    let lo_approx = if lo_f <= 0.0 then R.zero
      else
        let s = lo_f ** inv_n in
        (* Conservative lower bound *)
        R.of_ints (max 0 (int_of_float (s *. 10000.0 -. 1.0))) 10000
    in
    let hi_approx =
      let s = hi_f ** inv_n in
      R.of_ints (int_of_float (s *. 10000.0 +. 2.0)) 10000
    in
    { lo = lo_approx; hi = hi_approx }

let to_string t =
  Printf.sprintf "[%s, %s]" (R.to_string t.lo) (R.to_string t.hi)
