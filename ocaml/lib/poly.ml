(** Dense univariate polynomial arithmetic over an arbitrary field.

    Polynomials are represented as coefficient lists, low-degree first:
    [[a0; a1; ...; an]] means [a0 + a1*x + ... + an*x^n].
    Invariant: trailing zeros are stripped (leading coefficient is nonzero),
    except for the zero polynomial which is [[]].

    This module is a functor parameterised by a FIELD signature,
    replacing the Haskell typeclass approach. *)

(** Output signature for polynomial operations. *)
module type S = sig
  (** The coefficient type. *)
  type scalar
  (** The polynomial type. *)
  type t

  (** The zero polynomial. *)
  val zero : t
  (** The constant polynomial 1. *)
  val one : t
  (** The indeterminate x. *)
  val x : t
  (** Build a polynomial from a coefficient list (low-degree first). *)
  val of_coeffs : scalar list -> t
  (** Extract the coefficient list (low-degree first). *)
  val to_coeffs : t -> scalar list
  (** Embed a scalar as a constant polynomial. *)
  val of_scalar : scalar -> t
  (** Degree of the polynomial; returns -1 for the zero polynomial. *)
  val degree : t -> int
  (** Leading coefficient, or [None] for the zero polynomial. *)
  val lead_coeff : t -> scalar option
  (** Evaluate the polynomial at a point (Horner's method). *)
  val eval : t -> scalar -> scalar
  (** Multiply by a scalar. *)
  val scale : scalar -> t -> t
  (** Polynomial addition. *)
  val add : t -> t -> t
  (** Polynomial subtraction. *)
  val sub : t -> t -> t
  (** Polynomial multiplication (schoolbook). *)
  val mul : t -> t -> t
  (** Negate all coefficients. *)
  val neg : t -> t
  (** Division with remainder: [div_mod f g = (q, r)] where [f = g*q + r]
      and [degree r < degree g].
      @raise Invalid_argument if [g] is the zero polynomial. *)
  val div_mod : t -> t -> t * t
  (** Quotient of polynomial division. *)
  val div : t -> t -> t
  (** Remainder of polynomial division. *)
  val modulo : t -> t -> t
  (** Monic GCD via the Euclidean algorithm. *)
  val gcd : t -> t -> t
  (** Make a polynomial monic (divide by the leading coefficient). *)
  val monic : t -> t
  (** Formal derivative. *)
  val diff : t -> t
  (** Composition: [compose f g] returns [f(g(x))]. *)
  val compose : t -> t -> t
  (** Square-free factorisation via Yun's algorithm.
      Returns [(factor, multiplicity)] pairs. *)
  val square_free : t -> (t * int) list
  (** Structural equality of polynomials. *)
  val equal : t -> t -> bool
  (** Pretty-print, given a printer for scalars. *)
  val to_string : (scalar -> string) -> t -> string
end

(** Create polynomial operations over a field. *)
module Make (F : Field_sig.FIELD) : S with type scalar = F.t = struct
  type scalar = F.t
  type t = scalar list  (* low-degree first, trailing zeros stripped *)

  (** Strip trailing zeros. *)
  let strip cs =
    let rec go = function
      | [] -> []
      | x :: rest ->
        let rest' = go rest in
        if rest' = [] && F.equal x F.zero then []
        else x :: rest'
    in
    go cs

  let zero = []
  let one = [F.one]
  let x = [F.zero; F.one]

  let of_coeffs cs = strip cs
  let to_coeffs t = t
  let of_scalar c = if F.equal c F.zero then [] else [c]

  let degree = function
    | [] -> -1
    | cs -> List.length cs - 1

  let lead_coeff = function
    | [] -> None
    | cs -> Some (List.nth cs (List.length cs - 1))

  (** Evaluate using Horner's method. *)
  let eval t v = match t with
    | [] -> F.zero
    | cs -> List.fold_right (fun c acc -> F.add c (F.mul v acc)) cs F.zero

  let scale s = function
    | _ when F.equal s F.zero -> zero
    | cs -> of_coeffs (List.map (F.mul s) cs)

  let rec zip_with_default d f xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs', [] -> f x d :: zip_with_default d f xs' []
    | [], y :: ys' -> f d y :: zip_with_default d f [] ys'
    | x :: xs', y :: ys' -> f x y :: zip_with_default d f xs' ys'

  let add a b = of_coeffs (zip_with_default F.zero F.add a b)
  let sub a b = of_coeffs (zip_with_default F.zero F.sub a b)
  let neg cs = List.map F.neg cs

  let mul a b = match a, b with
    | [], _ | _, [] -> zero
    | _ ->
      let la = List.length a in
      let lb = List.length b in
      let rlen = la + lb - 1 in
      let result = Array.make rlen F.zero in
      List.iteri (fun i ai ->
        List.iteri (fun j bj ->
          result.(i + j) <- F.add result.(i + j) (F.mul ai bj)) b) a;
      of_coeffs (Array.to_list result)

  let div_mod f g = match g with
    | [] -> invalid_arg "Poly.div_mod: division by zero polynomial"
    | _ when degree f < degree g -> (zero, f)
    | _ ->
      let dg = degree g in
      let lc = match lead_coeff g with
        | Some c -> c | None -> assert false in
      let rec go q r =
        if degree r < dg then (q, r)
        else
          let dr = degree r in
          let lr = match lead_coeff r with
            | Some c -> c | None -> assert false in
          let c = F.div lr lc in
          let d = dr - dg in
          let term = of_coeffs (List.init (d + 1) (fun i ->
            if i = d then c else F.zero)) in
          let r' = sub r (mul term g) in
          go (add q term) r'
      in
      go zero f

  let div a b = fst (div_mod a b)
  let modulo a b = snd (div_mod a b)

  let monic = function
    | [] -> zero
    | cs -> match lead_coeff (of_coeffs cs) with
      | None -> zero
      | Some lc -> List.map (fun c -> F.div c lc) cs

  let gcd a b = match b with
    | [] -> of_coeffs (monic a)
    | _ -> let rec go a b = match b with
      | [] -> of_coeffs (monic a)
      | _ -> go b (snd (div_mod a b))
    in go a b

  let diff = function
    | [] -> zero
    | _ :: cs ->
      of_coeffs (List.mapi (fun i c -> F.mul (F.of_int (i + 1)) c) cs)

  let compose f g = match f with
    | [] -> zero
    | cs ->
      List.fold_right (fun c acc -> add (of_scalar c) (mul g acc)) cs zero

  (** Square-free factorisation via Yun's algorithm. *)
  let square_free f = match f with
    | [] -> []
    | _ ->
      let f' = diff f in
      let c = gcd f f' in
      let w = div f c in
      let rec go w c i =
        if degree w = 0 then
          if degree c > 0 then [(c, i)]
          else []
        else
          let y = gcd w c in
          let z = div w y in
          let c' = div c y in
          let rest = go y c' (i + 1) in
          if degree z > 0 then (z, i) :: rest else rest
      in
      go w c 1

  let equal a b =
    let la = List.length a in
    let lb = List.length b in
    la = lb && List.for_all2 F.equal a b

  let to_string scalar_str = function
    | [] -> "0"
    | cs ->
      let terms = List.mapi (fun i c ->
        if F.equal c F.zero then None
        else
          let cs = scalar_str c in
          let var = match i with
            | 0 -> ""
            | 1 -> "x"
            | n -> "x^" ^ string_of_int n
          in
          Some (if var = "" then cs
                else if F.equal c F.one then var
                else if F.equal c (F.neg F.one) then "-" ^ var
                else cs ^ "*" ^ var))
        cs
      in
      let non_empty = List.filter_map Fun.id terms in
      match non_empty with
      | [] -> "0"
      | _ -> String.concat " + " non_empty
end

(** Pre-instantiated polynomial arithmetic over the rationals. *)
module RatPoly = Make(Rational.Field)
