(** Radical expression AST, parameterised by the coefficient type.

    Normalization is explicit: these constructors are "dumb".
    Use {!Normalize} for simplification. *)

(** A radical expression over a coefficient field ['k]. *)
type 'k t =
  | Lit of 'k
  | Neg of 'k t
  | Add of 'k t * 'k t
  | Mul of 'k t * 'k t
  | Inv of 'k t
  | Root of int * 'k t    (** [Root (n, x)] = n-th root of x, n >= 2 *)
  | Pow of 'k t * int

(** {2 Comparison}

    Structural comparison — required for use as map/set keys.
    This is NOT mathematical equality (use {!Equality} for that). *)

let rec compare cmp_k a b =
  match a, b with
  | Lit x, Lit y -> cmp_k x y
  | Lit _, _ -> -1 | _, Lit _ -> 1
  | Neg x, Neg y -> compare cmp_k x y
  | Neg _, _ -> -1 | _, Neg _ -> 1
  | Add (a1, a2), Add (b1, b2) ->
    let c = compare cmp_k a1 b1 in
    if c <> 0 then c else compare cmp_k a2 b2
  | Add _, _ -> -1 | _, Add _ -> 1
  | Mul (a1, a2), Mul (b1, b2) ->
    let c = compare cmp_k a1 b1 in
    if c <> 0 then c else compare cmp_k a2 b2
  | Mul _, _ -> -1 | _, Mul _ -> 1
  | Inv x, Inv y -> compare cmp_k x y
  | Inv _, _ -> -1 | _, Inv _ -> 1
  | Root (n1, x), Root (n2, y) ->
    let c = Int.compare n1 n2 in
    if c <> 0 then c else compare cmp_k x y
  | Root _, _ -> -1 | _, Root _ -> 1
  | Pow (x, n1), Pow (y, n2) ->
    let c = compare cmp_k x y in
    if c <> 0 then c else Int.compare n1 n2

let rec equal eq_k a b =
  match a, b with
  | Lit x, Lit y -> eq_k x y
  | Neg x, Neg y -> equal eq_k x y
  | Add (a1, a2), Add (b1, b2) -> equal eq_k a1 b1 && equal eq_k a2 b2
  | Mul (a1, a2), Mul (b1, b2) -> equal eq_k a1 b1 && equal eq_k a2 b2
  | Inv x, Inv y -> equal eq_k x y
  | Root (n1, x), Root (n2, y) -> n1 = n2 && equal eq_k x y
  | Pow (x, n1), Pow (y, n2) -> equal eq_k x y && n1 = n2
  | _ -> false

(** {2 Construction helpers} *)

(** Wrap a coefficient as a literal. *)
let lit k = Lit k
(** Negate an expression. *)
let neg e = Neg e
(** Sum of two expressions. *)
let add a b = Add (a, b)
(** Subtraction: [sub a b = Add (a, Neg b)]. *)
let sub a b = Add (a, Neg b)
(** Product of two expressions. *)
let mul a b = Mul (a, b)
(** Division: [div' a b = Mul (a, Inv b)]. *)
let div' a b = Mul (a, Inv b)
(** Multiplicative inverse. *)
let inv e = Inv e
(** Principal [n]-th root of [e] (n >= 2). *)
let root n e = Root (n, e)
(** Square root shorthand. *)
let sqrt' e = Root (2, e)
(** Integer power. *)
let pow e n = Pow (e, n)

(** Lift an integer into a radical expression over rationals. *)
let of_int n = Lit (Rational.of_int n)
(** Lift a rational into a radical expression. *)
let of_rational r = Lit r

(** {2 Functor map (fmap)} *)

(** Map a function over all coefficients in the expression. *)
let rec map f = function
  | Lit k -> Lit (f k)
  | Neg a -> Neg (map f a)
  | Add (a, b) -> Add (map f a, map f b)
  | Mul (a, b) -> Mul (map f a, map f b)
  | Inv a -> Inv (map f a)
  | Root (n, a) -> Root (n, map f a)
  | Pow (a, n) -> Pow (map f a, n)

(** {2 Structural queries} *)

(** Nesting depth of the expression tree.  Literals have depth 0. *)
let rec depth = function
  | Lit _ -> 0
  | Neg a -> depth a
  | Add (a, b) -> 1 + max (depth a) (depth b)
  | Mul (a, b) -> 1 + max (depth a) (depth b)
  | Inv a -> 1 + depth a
  | Root (_, a) -> 1 + depth a
  | Pow (a, _) -> 1 + depth a

(** Number of nodes in the expression tree. *)
let rec size = function
  | Lit _ -> 1
  | Neg a -> 1 + size a
  | Add (a, b) -> 1 + size a + size b
  | Mul (a, b) -> 1 + size a + size b
  | Inv a -> 1 + size a
  | Root (_, a) -> 1 + size a
  | Pow (a, _) -> 1 + size a

(** Check if all coefficients satisfy a predicate. *)
let rec for_all_coeffs p = function
  | Lit k -> p k
  | Neg a -> for_all_coeffs p a
  | Add (a, b) -> for_all_coeffs p a && for_all_coeffs p b
  | Mul (a, b) -> for_all_coeffs p a && for_all_coeffs p b
  | Inv a -> for_all_coeffs p a
  | Root (_, a) -> for_all_coeffs p a
  | Pow (a, _) -> for_all_coeffs p a

(** Collect distinct [(root_index, radicand)] pairs. *)
let collect_radicals eq_k expr =
  let mem pair acc =
    List.exists (fun (n2, e2) ->
      let (n1, e1) = pair in
      n1 = n2 && equal eq_k e1 e2) acc
  in
  let rec go acc = function
    | Lit _ -> acc
    | Neg a -> go acc a
    | Add (a, b) -> go (go acc a) b
    | Mul (a, b) -> go (go acc a) b
    | Inv a -> go acc a
    | Pow (a, _) -> go acc a
    | Root (n, a) ->
      let acc' = go acc a in
      let pair = (n, a) in
      if mem pair acc' then acc' else acc' @ [pair]
  in
  go [] expr
