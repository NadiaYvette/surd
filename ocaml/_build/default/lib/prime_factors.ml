(** Small prime factorisation and related utilities.

    Trial-division based: adequate for the integer sizes encountered
    in radical simplification (radicands, denominators, etc.). *)

(** Infinite-ish prime sequence via trial division.
    In practice we memoise a finite prefix. *)
let is_prime n =
  if n < 2 then false
  else if n < 4 then true
  else
    let rec check d =
      if d * d > n then true
      else if n mod d = 0 then false
      else check (d + if d = 2 then 1 else 2)
    in
    check 2

(** Generate primes up to a bound by trial division. *)
let primes_upto bound =
  let acc = ref [2] in
  let n = ref 3 in
  while !n <= bound do
    if is_prime !n then acc := !n :: !acc;
    n := !n + 2
  done;
  List.rev !acc

(** Factorise a positive integer into [(prime, exponent)] pairs.

    {[
      factorise (Positive.of_int_exn 360) = [(2, 3); (3, 2); (5, 1)]
    ]} *)
let factorise (pos : Positive.t) : (int * int) list =
  let n = Positive.to_int pos in
  if n = 1 then []
  else
    let factors = ref [] in
    let m = ref n in
    let d = ref 2 in
    while !d * !d <= !m do
      if !m mod !d = 0 then begin
        let count = ref 0 in
        while !m mod !d = 0 do
          m := !m / !d;
          incr count
        done;
        factors := (!d, !count) :: !factors
      end;
      d := !d + (if !d = 2 then 1 else 2)
    done;
    if !m > 1 then factors := (!m, 1) :: !factors;
    List.rev !factors

(** Distinct prime factors in ascending order. *)
let prime_factors pos = List.map fst (factorise pos)
