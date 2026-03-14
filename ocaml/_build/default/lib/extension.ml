(** Algebraic field extensions via functor.

    [Make(Base)(MinPoly)] builds the field [Base(alpha)] where [alpha]
    is a root of [MinPoly].  Elements are polynomials in [alpha] of
    degree less than [deg(MinPoly)], with arithmetic modulo [MinPoly].

    This replaces Haskell's [ExtElem] type that uses typeclass dispatch. *)

(** Minimal polynomial specification: coefficients of the irreducible
    polynomial, low-degree first.
    E.g., [x^2 + 1] is [[one; zero; one]]. *)
module type MINPOLY = sig
  type scalar
  val coeffs : scalar list
end

(** Output signature for extension field operations. *)
module type S = sig
  type scalar
  type t

  val of_base : scalar -> t
  val generator : t
  val to_coeffs : t -> scalar list
  val of_coeffs : scalar list -> t

  include Field_sig.FIELD with type t := t
end

(** Build a field extension [Base(alpha)] where [alpha] is a root of the
    irreducible polynomial given by [MP.coeffs]. *)
module Make (Base : Field_sig.FIELD) (MP : MINPOLY with type scalar = Base.t)
  : S with type scalar = Base.t
= struct
  module P = Poly.Make(Base)

  type scalar = Base.t

  (** An element is a polynomial in alpha of degree < deg(minpoly). *)
  type t = Base.t list

  let deg = List.length MP.coeffs - 1

  (** Reduce a polynomial modulo the minimal polynomial. *)
  let reduce cs =
    let p = P.of_coeffs cs in
    let mp = P.of_coeffs MP.coeffs in
    if P.degree p < deg then cs
    else
      let _, r = P.div_mod p mp in
      P.to_coeffs r

  (** Pad or truncate to exactly [deg] coefficients. *)
  let pad cs =
    let len = List.length cs in
    if len >= deg then List.filteri (fun i _ -> i < deg) cs
    else cs @ List.init (deg - len) (fun _ -> Base.zero)

  let of_base s = pad [s]

  let generator = pad (if deg >= 2 then [Base.zero; Base.one] else [Base.one])

  let to_coeffs t = pad t
  let of_coeffs cs = pad (reduce cs)

  let zero = pad []
  let one = pad [Base.one]

  let add a b =
    let a' = pad a and b' = pad b in
    pad (List.map2 Base.add a' b')

  let sub a b =
    let a' = pad a and b' = pad b in
    pad (List.map2 Base.sub a' b')

  let neg a = List.map Base.neg a

  let mul a b =
    let p = P.mul (P.of_coeffs a) (P.of_coeffs b) in
    pad (reduce (P.to_coeffs p))

  (** Extended Euclidean algorithm on polynomials to find the inverse.
      Given [a] and [minpoly], find [s] such that [s*a = 1 mod minpoly]. *)
  let inv a =
    let a_poly = P.of_coeffs a in
    let mp = P.of_coeffs MP.coeffs in
    (* Extended GCD *)
    let rec ext_gcd old_r r old_s s =
      if P.degree r < 0 then (old_r, old_s)
      else
        let q, rem = P.div_mod old_r r in
        ext_gcd r rem s (P.sub old_s (P.mul q s))
    in
    let (g, s) = ext_gcd mp a_poly P.zero P.one in
    (* g should be a nonzero constant (since minpoly is irreducible) *)
    match P.lead_coeff g with
    | None -> invalid_arg "Extension.inv: zero element"
    | Some lc ->
      let s_scaled = P.scale (Base.inv lc) s in
      pad (reduce (P.to_coeffs s_scaled))

  let div a b = mul a (inv b)

  let equal a b =
    let a' = pad a and b' = pad b in
    List.for_all2 Base.equal a' b'

  let compare a b =
    let a' = pad a and b' = pad b in
    let rec cmp xs ys = match xs, ys with
      | [], [] -> 0
      | [], _ | _, [] -> assert false  (* both padded to same length *)
      | x :: xs', y :: ys' ->
        let c = Base.compare x y in
        if c <> 0 then c else cmp xs' ys'
    in
    cmp a' b'

  let of_int n = pad [Base.of_int n]

  let to_string a =
    let cs = pad a in
    let terms = List.mapi (fun i c ->
      if Base.equal c Base.zero then None
      else
        let cs = Base.to_string c in
        let var = match i with
          | 0 -> ""
          | 1 -> "a"
          | n -> "a^" ^ string_of_int n
        in
        Some (if var = "" then cs
              else if Base.equal c Base.one then var
              else cs ^ "*" ^ var))
      cs
    in
    let non_empty = List.filter_map Fun.id terms in
    match non_empty with
    | [] -> "0"
    | _ -> String.concat " + " non_empty
end
