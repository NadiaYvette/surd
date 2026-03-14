(** Normal form canonicalisation for radical expressions.

    Decomposes a radical expression into a sum of monomials where each
    monomial is a product of atoms (rational roots, imaginary unit,
    nested roots) raised to integer powers, with a rational coefficient. *)

module R = Rational

(** An atom in the normal form. *)
type atom =
  | RatRoot of int * R.t    (** n-th root of a positive rational *)
  | ImagUnit               (** sqrt(-1) *)
  | NestedRoot of int * R.t Rad_expr.t  (** n-th root of a multi-term radicand *)

let compare_atom a b = match a, b with
  | RatRoot (n1, r1), RatRoot (n2, r2) ->
    let c = Int.compare n1 n2 in
    if c <> 0 then c else R.compare r1 r2
  | RatRoot _, _ -> -1 | _, RatRoot _ -> 1
  | ImagUnit, ImagUnit -> 0
  | ImagUnit, _ -> -1 | _, ImagUnit -> 1
  | NestedRoot (n1, e1), NestedRoot (n2, e2) ->
    let c = Int.compare n1 n2 in
    if c <> 0 then c else Rad_expr.compare R.compare e1 e2

module AtomMap = Map.Make(struct
  type t = atom
  let compare = compare_atom
end)

(** A monomial: a product of atoms raised to integer powers. *)
type monomial = int AtomMap.t

let compare_monomial = AtomMap.compare Int.compare

module MonomialMap = Map.Make(struct
  type t = monomial
  let compare = compare_monomial
end)

(** A normal-form expression: a sum of monomials with rational coefficients. *)
type norm_expr = R.t MonomialMap.t

(** The zero normal-form expression. *)
let zero_nf : norm_expr = MonomialMap.empty

(** A literal rational. *)
let lit_nf r : norm_expr =
  if R.is_zero r then zero_nf
  else MonomialMap.singleton AtomMap.empty r

(** Convert a radical expression to normal form. *)
let rec to_norm_expr : R.t Rad_expr.t -> norm_expr = function
  | Rad_expr.Lit r -> lit_nf r
  | Rad_expr.Neg a -> nf_neg (to_norm_expr a)
  | Rad_expr.Add (a, b) -> nf_add (to_norm_expr a) (to_norm_expr b)
  | Rad_expr.Mul (a, b) -> nf_mul (to_norm_expr a) (to_norm_expr b)
  | Rad_expr.Inv a -> nf_inv (to_norm_expr a)
  | Rad_expr.Root (n, a) -> nf_root n a
  | Rad_expr.Pow (a, n) -> nf_pow (to_norm_expr a) n

and nf_neg nf =
  MonomialMap.map R.neg nf

and nf_add a b =
  MonomialMap.union (fun _k x y ->
    let s = R.add x y in
    if R.is_zero s then None else Some s) a b

and nf_mul a b =
  MonomialMap.fold (fun ma ca acc ->
    MonomialMap.fold (fun mb cb acc ->
      let m = AtomMap.union (fun _k ea eb -> Some (ea + eb)) ma mb in
      let c = R.mul ca cb in
      if R.is_zero c then acc
      else
        let cur = match MonomialMap.find_opt m acc with
          | Some v -> v | None -> R.zero in
        let s = R.add cur c in
        if R.is_zero s then MonomialMap.remove m acc
        else MonomialMap.add m s acc)
      b acc)
    a MonomialMap.empty

and nf_inv nf =
  (* Single-monomial inversion: negate all exponents *)
  if MonomialMap.cardinal nf = 1 then
    let (m, c) = MonomialMap.min_binding nf in
    let m' = AtomMap.map (fun e -> -e) m in
    MonomialMap.singleton m' (R.inv c)
  else
    (* Multi-term: fall back to keeping as-is for now *)
    (* A full implementation would rationalise the denominator *)
    let expr = from_norm_expr nf in
    let atom = NestedRoot (1, Rad_expr.Inv expr) in
    MonomialMap.singleton (AtomMap.singleton atom 1) R.one

and nf_root n expr =
  (* Try to decompose the radicand *)
  let nf = to_norm_expr expr in
  if MonomialMap.cardinal nf = 1 then
    let (m, c) = MonomialMap.min_binding nf in
    if AtomMap.is_empty m then
      (* Root of a pure rational *)
      if R.is_neg c && n mod 2 = 0 then
        (* Even root of negative: factor out i *)
        let inner = nf_root n (Rad_expr.Lit (R.neg c)) in
        let i_mono = AtomMap.singleton ImagUnit 1 in
        nf_mul (MonomialMap.singleton i_mono R.one) inner
      else
        let atom = RatRoot (n, c) in
        MonomialMap.singleton (AtomMap.singleton atom 1) R.one
    else
      (* Root of a single monomial: distribute root over factors *)
      let atom = NestedRoot (n, expr) in
      MonomialMap.singleton (AtomMap.singleton atom 1) R.one
  else
    let atom = NestedRoot (n, expr) in
    MonomialMap.singleton (AtomMap.singleton atom 1) R.one

and nf_pow nf n =
  if n = 0 then lit_nf R.one
  else if n = 1 then nf
  else if n < 0 then nf_pow (nf_inv nf) (-n)
  else
    (* Exponentiation by squaring *)
    let half = nf_pow nf (n / 2) in
    let sq = nf_mul half half in
    if n mod 2 = 0 then sq
    else nf_mul sq nf

(** Convert a normal form back to a radical expression. *)
and from_norm_expr (nf : norm_expr) : R.t Rad_expr.t =
  let terms = MonomialMap.fold (fun m c acc ->
    let factors = AtomMap.fold (fun atom exp fs ->
      let base = match atom with
        | RatRoot (n, r) -> Rad_expr.Root (n, Rad_expr.Lit r)
        | ImagUnit -> Rad_expr.Root (2, Rad_expr.Lit R.minus_one)
        | NestedRoot (n, e) ->
          if n = 1 then e
          else Rad_expr.Root (n, e)
      in
      let powered = if exp = 1 then base
        else if exp = 0 then Rad_expr.Lit R.one
        else Rad_expr.Pow (base, exp)
      in
      powered :: fs)
      m []
    in
    let mono_expr = match factors with
      | [] -> Rad_expr.Lit c
      | _ ->
        let product = List.fold_left (fun acc f ->
          Rad_expr.Mul (acc, f)) (List.hd factors) (List.tl factors) in
        if R.is_one c then product
        else Rad_expr.Mul (Rad_expr.Lit c, product)
    in
    mono_expr :: acc)
    nf []
  in
  match terms with
  | [] -> Rad_expr.Lit R.zero
  | [x] -> x
  | x :: rest -> List.fold_left (fun acc t -> Rad_expr.Add (acc, t)) x rest

(** Simplify a radical expression via NF round-trip. *)
let simplify_via_nf expr =
  let nf = to_norm_expr expr in
  from_norm_expr nf
