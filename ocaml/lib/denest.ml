(** Radical denesting algorithms.

    Implements Borodin's square root denesting and stubs for more
    general nth-root and Landau denesting. *)

module R = Rational

(** Try to denest a square root: sqrt(a + b*sqrt(c)) -> p + q*sqrt(c)
    when a^2 - b^2*c is a perfect square.

    This is the Borodin-Fagin-Hopcroft-Tompa algorithm for depth-1
    square root denesting. *)
let try_sqrt_denest (expr : R.t Rad_expr.t) : R.t Rad_expr.t option =
  (* Look for pattern: Root(2, Add(Lit a, Mul(Lit b, Root(2, Lit c)))) *)
  match expr with
  | Rad_expr.Root (2, Rad_expr.Add (Rad_expr.Lit a,
      Rad_expr.Mul (Rad_expr.Lit b, Rad_expr.Root (2, Rad_expr.Lit c)))) ->
    (* sqrt(a + b*sqrt(c)) = sqrt((a + d)/2) + sign(b)*sqrt((a - d)/2)
       where d = sqrt(a^2 - b^2*c) *)
    let disc = R.sub (R.mul a a) (R.mul (R.mul b b) c) in
    if R.is_neg disc then None
    else
      (* Check if disc is a perfect square *)
      let disc_num = Z.to_int (R.num disc) in
      let disc_den = Z.to_int (R.den disc) in
      let sqrt_num = Z.to_int (Z.sqrt (Z.of_int disc_num)) in
      let sqrt_den = Z.to_int (Z.sqrt (Z.of_int disc_den)) in
      if sqrt_num * sqrt_num = disc_num && sqrt_den * sqrt_den = disc_den then
        let d = R.of_ints sqrt_num sqrt_den in
        let two = R.of_int 2 in
        let half_plus = R.div (R.add a d) two in
        let half_minus = R.div (R.sub a d) two in
        if R.is_neg half_plus || R.is_neg half_minus then None
        else
          let sign_b = if R.is_pos b then R.one else R.minus_one in
          Some (Rad_expr.Add (
            Rad_expr.Root (2, Rad_expr.Lit half_plus),
            Rad_expr.Mul (Rad_expr.Lit sign_b,
              Rad_expr.Root (2, Rad_expr.Lit half_minus))))
      else None
  | _ -> None

(** Attempt to denest a radical expression.
    Tries available denesting strategies in order. *)
let denest expr =
  match try_sqrt_denest expr with
  | Some result -> result
  | None -> expr
