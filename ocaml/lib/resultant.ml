(** Resultant computation for polynomials.

    The resultant of two polynomials f and g is a scalar that vanishes
    iff they share a common root. Used in minimal polynomial computation
    and algebraic number arithmetic. *)

module Make (F : Field_sig.FIELD) = struct
  module P = Poly.Make(F)

  (** Compute the resultant of two polynomials via the subresultant
      (Euclidean) algorithm.

      For [f] of degree [m] and [g] of degree [n], the resultant is
      defined as the determinant of the [(m+n) x (m+n)] Sylvester matrix.
      We compute it via pseudo-remainder sequences for efficiency. *)
  let resultant f g =
    let m = P.degree f in
    let n = P.degree g in
    if m < 0 || n < 0 then F.zero
    else if m = 0 then
      let a0 = match P.lead_coeff f with Some c -> c | None -> F.zero in
      let rec ipow base exp =
        if exp = 0 then F.one
        else F.mul base (ipow base (exp - 1))
      in
      ipow a0 n
    else if n = 0 then
      let b0 = match P.lead_coeff g with Some c -> c | None -> F.zero in
      let rec ipow base exp =
        if exp = 0 then F.one
        else F.mul base (ipow base (exp - 1))
      in
      ipow b0 m
    else
      (* Euclidean algorithm for resultant *)
      let rec go f g sign =
        let df = P.degree f in
        let dg = P.degree g in
        if dg < 0 then F.zero
        else if dg = 0 then
          let bg = match P.lead_coeff g with Some c -> c | None -> F.zero in
          let rec ipow base exp =
            if exp = 0 then F.one
            else F.mul base (ipow base (exp - 1))
          in
          let r = ipow bg df in
          if sign then F.neg r else r
        else
          let _, rem = P.div_mod f g in
          let new_sign = (df * dg) mod 2 = 1 in
          go g rem (if new_sign then not sign else sign)
      in
      go f g false
end

module RatResultant = Make(Rational.Field)
