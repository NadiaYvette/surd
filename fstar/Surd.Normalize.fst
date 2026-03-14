/// Normalization passes for radical expressions.
///
/// Explicit transformations: flatten, fold constants, simplify powers,
/// extract perfect powers, distribute, collect coefficients, collect terms.
/// Composed into `normalize` which iterates to a fixed point (or fuel).
module Surd.Normalize

open Surd.Ring
open Surd.Rational
open Surd.Positive
open Surd.PrimeFactors
open Surd.Types
open FStar.List.Tot

// ---------------------------------------------------------------------------
// Dv-compatible list map
// ---------------------------------------------------------------------------

/// Map with Dv effect (FStar.List.Tot.map requires Tot).
val map_dv : #a:Type -> #b:Type -> (a -> Dv b) -> list a -> Dv (list b)
let rec map_dv #a #b f xs =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map_dv f rest

// ---------------------------------------------------------------------------
// Association-list helpers (no Map in F* stdlib)
// ---------------------------------------------------------------------------

/// Structural equality on rad_expr rational (decidable, used for term grouping).
/// We compare trees structurally, with rat_eq for coefficients.
let rec expr_eq (a b : rad_expr rational) : Tot bool (decreases a) =
  match a, b with
  | Lit x, Lit y -> rat_eq x y
  | Neg x, Neg y -> expr_eq x y
  | Add x1 x2, Add y1 y2 -> expr_eq x1 y1 && expr_eq x2 y2
  | Mul x1 x2, Mul y1 y2 -> expr_eq x1 y1 && expr_eq x2 y2
  | Inv x, Inv y -> expr_eq x y
  | Root n1 x, Root n2 y -> n1 = n2 && expr_eq x y
  | Pow x n1, Pow y n2 -> n1 = n2 && expr_eq x y
  | _, _ -> false

/// Structural comparison on rad_expr rational for sorting.
/// Returns negative, zero, or positive.
let rec expr_cmp (a b : rad_expr rational) : Tot int (decreases a) =
  match a, b with
  | Lit x, Lit y ->
    if rat_lt x y then 0 - 1
    else if rat_eq x y then 0
    else 1
  | Lit _, _ -> 0 - 1
  | _, Lit _ -> 1
  | Neg x, Neg y -> expr_cmp x y
  | Neg _, _ -> 0 - 1
  | _, Neg _ -> 1
  | Add x1 x2, Add y1 y2 ->
    let c = expr_cmp x1 y1 in
    if c <> 0 then c else expr_cmp x2 y2
  | Add _ _, _ -> 0 - 1
  | _, Add _ _ -> 1
  | Mul x1 x2, Mul y1 y2 ->
    let c = expr_cmp x1 y1 in
    if c <> 0 then c else expr_cmp x2 y2
  | Mul _ _, _ -> 0 - 1
  | _, Mul _ _ -> 1
  | Inv x, Inv y -> expr_cmp x y
  | Inv _, _ -> 0 - 1
  | _, Inv _ -> 1
  | Root n1 x, Root n2 y ->
    if n1 < n2 then 0 - 1
    else if n1 > n2 then 1
    else expr_cmp x y
  | Root _ _, _ -> 0 - 1
  | _, Root _ _ -> 1
  | Pow x n1, Pow y n2 ->
    let c = expr_cmp x y in
    if c <> 0 then c
    else if n1 < n2 then 0 - 1
    else if n1 > n2 then 1
    else 0

/// Look up a key in an association list using expr_eq.
let rec assoc_lookup (key: rad_expr rational) (xs: list (rad_expr rational & rational))
  : Tot (option rational) (decreases xs) =
  match xs with
  | [] -> None
  | (k, v) :: rest -> if expr_eq k key then Some v else assoc_lookup key rest

/// Insert or add to an association list.
let rec assoc_insert_with (f: rational -> rational -> rational)
                          (key: rad_expr rational)
                          (val_: rational)
                          (xs: list (rad_expr rational & rational))
  : Tot (list (rad_expr rational & rational)) (decreases xs) =
  match xs with
  | [] -> [(key, val_)]
  | (k, v) :: rest ->
    if expr_eq k key then (k, f v val_) :: rest
    else (k, v) :: assoc_insert_with f key val_ rest

// ---------------------------------------------------------------------------
// Flatten nested Add/Mul and cancel double negations
// ---------------------------------------------------------------------------

let rec flatten_arith (#k:Type) (e: rad_expr k) : Tot (rad_expr k) (decreases e) =
  match e with
  | Neg (Neg a) -> flatten_arith a
  | Neg a -> Neg (flatten_arith a)
  | Add a b -> Add (flatten_arith a) (flatten_arith b)
  | Mul a b -> Mul (flatten_arith a) (flatten_arith b)
  | Inv (Inv a) -> flatten_arith a
  | Inv a -> Inv (flatten_arith a)
  | Root n a -> Root n (flatten_arith a)
  | Pow a n -> Pow (flatten_arith a) n
  | e -> e

// ---------------------------------------------------------------------------
// Fold constant subexpressions
// ---------------------------------------------------------------------------

/// Rational power for arbitrary integer exponent (positive or negative).
let rat_pow_int (r: rational) (n: int) : rational =
  if n >= 0 then rat_pow r n
  else if r.num <> 0 then rat_pow (rat_inv r) (0 - n)
  else rat_zero

let fold_constants (e: rad_expr rational) : Tot (rad_expr rational) (decreases e) =
  let rec go (e: rad_expr rational) : Tot (rad_expr rational) (decreases e) =
    match e with
    | Lit r -> Lit r
    | Neg a ->
      (match go a with
       | Lit r -> Lit (rat_neg r)
       | Neg a' -> a'
       | a' -> Neg a')
    | Add a b ->
      let a' = go a in
      let b' = go b in
      (match a', b' with
       | Lit r, Lit s -> Lit (rat_add r s)
       | Lit r, _ -> if rat_eq r rat_zero then b' else Add a' b'
       | _, Lit s -> if rat_eq s rat_zero then a' else Add a' b'
       | _, _ -> Add a' b')
    | Mul a b ->
      let a' = go a in
      let b' = go b in
      (match a', b' with
       | Lit r, Lit s -> Lit (rat_mul r s)
       | Lit r, _ ->
         if rat_eq r rat_zero then Lit rat_zero
         else if rat_eq r rat_one then b'
         else if rat_eq r (rat_neg rat_one) then Neg b'
         else Mul a' b'
       | _, Lit s ->
         if rat_eq s rat_zero then Lit rat_zero
         else if rat_eq s rat_one then a'
         else if rat_eq s (rat_neg rat_one) then Neg a'
         else Mul a' b'
       | _, _ -> Mul a' b')
    | Inv a ->
      (match go a with
       | Lit r -> if r.num <> 0 then Lit (rat_inv r) else Inv (Lit r)
       | Inv a' -> a'
       | a' -> Inv a')
    | Root n a ->
      (match go a with
       | Lit r ->
         if rat_eq r rat_zero then Lit rat_zero
         else if rat_eq r rat_one then Lit rat_one
         else Root n (Lit r)
       | a' -> Root n a')
    | Pow a n ->
      (match go a with
       | Lit r -> Lit (rat_pow_int r n)
       | a' ->
         if n = 0 then Lit rat_one
         else if n = 1 then a'
         else Pow a' n)
  in go e

// ---------------------------------------------------------------------------
// Simplify powers
// ---------------------------------------------------------------------------

val simplify_powers : e:rad_expr rational -> Tot (rad_expr rational) (decreases (expr_size e))
let rec simplify_powers (e: rad_expr rational) =
  match e with
  | Mul (Root 2 a) (Root 2 b) ->
    if expr_eq a b then simplify_powers a
    else Mul (Root 2 (simplify_powers a)) (Root 2 (simplify_powers b))
  | Pow (Pow a m) n -> simplify_powers (Pow a (op_Multiply m n))
  | Pow (Root n a) m ->
    if m = n then simplify_powers a
    else Pow (Root n (simplify_powers a)) m
  | Root m (Pow a n) ->
    if m = n then simplify_powers a
    else Root m (Pow (simplify_powers a) n)
  | Root m (Root n a) -> Root (op_Multiply m n) (simplify_powers a)
  | Neg a -> Neg (simplify_powers a)
  | Add a b -> Add (simplify_powers a) (simplify_powers b)
  | Mul a b -> Mul (simplify_powers a) (simplify_powers b)
  | Inv a -> Inv (simplify_powers a)
  | Root n a -> Root n (simplify_powers a)
  | Pow a n -> Pow (simplify_powers a) n
  | e -> e

// ---------------------------------------------------------------------------
// Extract perfect nth powers from under radicals
// ---------------------------------------------------------------------------

/// Given n (>= 2) and a positive integer m, extract the largest k such that k^n divides m.
/// Returns (extracted, remainder) as rationals so m = extracted^n * remainder.
val extract_nth_power : n:int{n >= 2} -> positive -> Dv (rational & rational)
let extract_nth_power n m =
  let fs = factorise m in
  let to_nat (x:int) : nat = if x >= 0 then x else 0 in
  let extracted = fold_left (fun (acc:int) ((p, e) : int & int) ->
    op_Multiply acc (pow_int p (to_nat (e / n)))) 1 fs in
  let remainder = fold_left (fun (acc:int) ((p, e) : int & int) ->
    op_Multiply acc (pow_int p (to_nat (e % n)))) 1 fs in
  (rat_of_int extracted, rat_of_int remainder)

val extract_perfect_powers : rad_expr rational -> Dv (rad_expr rational)
let rec extract_perfect_powers (e: rad_expr rational) : Dv (rad_expr rational) =
  match e with
  | Root n (Lit r) ->
    if rat_gt r rat_zero then
      let num = abs_int r.num in
      let den = r.den in
      (* rat_gt r rat_zero means r.num * 1 > 0 * r.den, i.e. r.num > 0,
         so abs_int r.num = r.num > 0. den > 0 by the rational type's refinement. *)
      assert (r.num > 0);
      assert (num > 0);
      assert (den > 0);
      let (num_out, num_in) = extract_nth_power n num in
      let (den_out, den_in) = extract_nth_power n den in
      let outer_coeff =
        if rat_eq den_in rat_one then rat_div_total num_out den_out
        else
          let n_minus_1 : nat = if n - 1 >= 0 then n - 1 else 0 in
          let den_in_pow = rat_pow den_in n_minus_1 in
          let new_inner = rat_mul num_in den_in_pow in
          let new_outer_den = rat_mul den_out den_in in
          let outer = rat_div_total num_out new_outer_den in
          (* Re-extract from new_inner *)
          let ni = abs_int new_inner.num in
          if ni > 0 then
            (let (num_out2, _) = extract_nth_power n ni in
             rat_mul outer num_out2)
          else outer
      in
      let inner_rat =
        if rat_eq den_in rat_one then num_in
        else
          let n_minus_1 : nat = if n - 1 >= 0 then n - 1 else 0 in
          let den_in_pow = rat_pow den_in n_minus_1 in
          let new_inner = rat_mul num_in den_in_pow in
          let ni = abs_int new_inner.num in
          if ni > 0 then
            (let (_, num_in2) = extract_nth_power n ni in
             num_in2)
          else new_inner
      in
      if rat_eq outer_coeff rat_one && rat_eq inner_rat rat_one then Lit rat_one
      else if rat_eq outer_coeff rat_one then Root n (Lit inner_rat)
      else if rat_eq inner_rat rat_one then Lit outer_coeff
      else Mul (Lit outer_coeff) (Root n (Lit inner_rat))
    else if rat_lt r rat_zero && n % 2 = 1 then
      Neg (extract_perfect_powers (Root n (Lit (rat_neg r))))
    else Root n (Lit r)
  | Root n a -> Root n (extract_perfect_powers a)
  | Neg a -> Neg (extract_perfect_powers a)
  | Add a b -> Add (extract_perfect_powers a) (extract_perfect_powers b)
  | Mul a b -> Mul (extract_perfect_powers a) (extract_perfect_powers b)
  | Inv a -> Inv (extract_perfect_powers a)
  | Pow a n -> Pow (extract_perfect_powers a) n
  | e -> e

// ---------------------------------------------------------------------------
// Collect rational coefficients in products
// ---------------------------------------------------------------------------

/// Flatten a Mul tree into a list of factors.
let rec flatten_mul (e: rad_expr rational) : Tot (list (rad_expr rational)) (decreases e) =
  match e with
  | Mul a b -> flatten_mul a @ flatten_mul b
  | e -> [e]

/// Partition a factor list into (rational coefficients, non-literal factors).
let rec partition_lits (xs: list (rad_expr rational))
  : Tot (list rational & list (rad_expr rational)) (decreases xs) =
  match xs with
  | [] -> ([], [])
  | Lit r :: rest ->
    let (ls, rs) = partition_lits rest in (r :: ls, rs)
  | Inv (Lit r) :: rest ->
    if r.num <> 0 then
      let (ls, rs) = partition_lits rest in (rat_inv r :: ls, rs)
    else
      let (ls, rs) = partition_lits rest in (ls, Inv (Lit r) :: rs)
  | x :: rest ->
    let (ls, rs) = partition_lits rest in (ls, x :: rs)

/// Build a Mul tree from a list.
let rec build_mul (xs: list (rad_expr rational)) : Tot (rad_expr rational) (decreases xs) =
  match xs with
  | [] -> Lit rat_one
  | [x] -> x
  | x :: rest -> Mul x (build_mul rest)

/// Apply a coefficient to a body.
let apply_coeff_mul (c: rational) (body: rad_expr rational) : rad_expr rational =
  if rat_eq c rat_zero then Lit rat_zero
  else if rat_eq c rat_one then body
  else if rat_eq c (rat_neg rat_one) then Neg body
  else match body with
    | Lit r -> Lit (rat_mul c r)
    | _ -> Mul (Lit c) body

val collect_coefficients : rad_expr rational -> Dv (rad_expr rational)
let rec collect_coefficients (e: rad_expr rational) : Dv (rad_expr rational) =
  match e with
  | Mul _ _ ->
    let factors = flatten_mul e in
    let processed = map_dv collect_coefficients factors in
    let (lits, rest) = partition_lits processed in
    let coeff = fold_left rat_mul rat_one lits in
    let body = build_mul rest in
    apply_coeff_mul coeff body
  | Neg a ->
    (match collect_coefficients a with
     | Lit r -> Lit (rat_neg r)
     | a' -> Neg a')
  | Add a b -> Add (collect_coefficients a) (collect_coefficients b)
  | Inv a -> Inv (collect_coefficients a)
  | Root n a -> Root n (collect_coefficients a)
  | Pow a n -> Pow (collect_coefficients a) n
  | e -> e

// ---------------------------------------------------------------------------
// Collect like terms in sums
// ---------------------------------------------------------------------------

/// Flatten an Add tree into a list of summands.
let rec flatten_add (e: rad_expr rational) : Tot (list (rad_expr rational)) (decreases e) =
  match e with
  | Add a b -> flatten_add a @ flatten_add b
  | e -> [e]

/// Split a term into (coefficient, base).
let split_coeff (e: rad_expr rational) : (rational & rad_expr rational) =
  match e with
  | Mul (Lit c) body -> (c, body)
  | Neg (Mul (Lit c) body) -> (rat_neg c, body)
  | Neg (Lit r) -> (rat_neg r, Lit rat_one)
  | Neg body -> (rat_neg rat_one, body)
  | Lit r -> (r, Lit rat_one)
  | _ -> (rat_one, e)

/// Group terms by base, summing coefficients. Uses association list.
let group_terms (terms: list (rad_expr rational))
  : list (rad_expr rational & rational) =
  fold_left
    (fun (acc: list (rad_expr rational & rational)) (term: rad_expr rational) ->
      let (c, base) = split_coeff term in
      assoc_insert_with rat_add base c acc)
    []
    terms

/// Apply a coefficient to a base (for rebuilding terms).
let apply_coeff_add (c: rational) (base: rad_expr rational) : rad_expr rational =
  if rat_eq c rat_one then
    (if expr_eq base (Lit rat_one) then Lit rat_one else base)
  else if rat_eq c (rat_neg rat_one) then
    (if expr_eq base (Lit rat_one) then Lit (rat_neg rat_one) else Neg base)
  else if expr_eq base (Lit rat_one) then Lit c
  else Mul (Lit c) base

/// Build an Add tree from a list.
let build_add (xs: list (rad_expr rational)) : rad_expr rational =
  match xs with
  | [] -> Lit rat_zero
  | [x] -> x
  | x :: rest -> fold_left Add x rest

val collect_terms : rad_expr rational -> Dv (rad_expr rational)
let rec collect_terms (e: rad_expr rational) : Dv (rad_expr rational) =
  match e with
  | Add _ _ ->
    let terms = flatten_add e in
    let processed = map_dv collect_terms terms in
    let grouped = group_terms processed in
    (* Filter out zero-coefficient terms *)
    let nonzero = filter (fun ((_,c) : rad_expr rational & rational) ->
                           not (rat_eq c rat_zero)) grouped in
    let rebuilt = map (fun ((base,c) : rad_expr rational & rational) ->
                        apply_coeff_add c base) nonzero in
    build_add rebuilt
  | Neg a -> Neg (collect_terms a)
  | Mul a b -> Mul (collect_terms a) (collect_terms b)
  | Inv a -> Inv (collect_terms a)
  | Root n a -> Root n (collect_terms a)
  | Pow a n -> Pow (collect_terms a) n
  | e -> e

// ---------------------------------------------------------------------------
// Sort commutative operators
// ---------------------------------------------------------------------------

/// Insertion sort on rad_expr rational using expr_cmp.
let rec insert_sorted (x: rad_expr rational) (xs: list (rad_expr rational))
  : Tot (list (rad_expr rational)) (decreases xs) =
  match xs with
  | [] -> [x]
  | y :: ys ->
    if expr_cmp x y <= 0 then x :: y :: ys
    else y :: insert_sorted x ys

let rec insertion_sort (xs: list (rad_expr rational))
  : Tot (list (rad_expr rational)) (decreases xs) =
  match xs with
  | [] -> []
  | x :: rest -> insert_sorted x (insertion_sort rest)

/// Flatten a Mul tree for sorting.
let rec flatten_mul_sort (e: rad_expr rational) : Tot (list (rad_expr rational)) (decreases e) =
  match e with
  | Mul a b -> flatten_mul_sort a @ flatten_mul_sort b
  | e -> [e]

/// Build a Mul from sorted factors.
let build_mul_sorted (xs: list (rad_expr rational)) : rad_expr rational =
  match xs with
  | [] -> Lit rat_one
  | [x] -> x
  | x :: rest -> fold_left Mul x rest

/// Flatten an Add tree for sorting.
let rec flatten_add_sort (e: rad_expr rational) : Tot (list (rad_expr rational)) (decreases e) =
  match e with
  | Add a b -> flatten_add_sort a @ flatten_add_sort b
  | e -> [e]

/// Build an Add from sorted terms.
let build_add_sorted (xs: list (rad_expr rational)) : rad_expr rational =
  match xs with
  | [] -> Lit rat_zero
  | [x] -> x
  | x :: rest -> fold_left Add x rest

val sort_commutative : rad_expr rational -> Dv (rad_expr rational)
let rec sort_commutative (e: rad_expr rational) : Dv (rad_expr rational) =
  match e with
  | Add _ _ ->
    let terms = flatten_add_sort e in
    let sorted = insertion_sort (map_dv sort_commutative terms) in
    build_add_sorted sorted
  | Mul _ _ ->
    let factors = flatten_mul_sort e in
    let sorted = insertion_sort (map_dv sort_commutative factors) in
    build_mul_sorted sorted
  | Neg a -> Neg (sort_commutative a)
  | Inv a -> Inv (sort_commutative a)
  | Root n a -> Root n (sort_commutative a)
  | Pow a n -> Pow (sort_commutative a) n
  | e -> e

// ---------------------------------------------------------------------------
// Distribute scalar multiplication over addition
// ---------------------------------------------------------------------------

/// Check if an expression is a sum.
let rec is_sum (#k:Type) (e: rad_expr k) : Tot bool (decreases e) =
  match e with
  | Add _ _ -> true
  | Neg a -> is_sum a
  | _ -> false

/// Flatten a sum into a list, pushing Neg through.
let rec flatten_sum (#k:Type) (e: rad_expr k) : Tot (list (rad_expr k)) (decreases e) =
  match e with
  | Add a b -> flatten_sum a @ flatten_sum b
  | Neg a -> map Neg (flatten_sum a)
  | e -> [e]

/// flatten_sum always returns a non-empty list.
let rec flatten_sum_nonempty (#k:Type) (e: rad_expr k)
  : Lemma (ensures Cons? (flatten_sum e))
          (decreases e) =
  match e with
  | Add a b ->
    flatten_sum_nonempty a;
    FStar.List.Tot.Properties.append_l_cons (List.Tot.hd (flatten_sum a))
                                            (List.Tot.tl (flatten_sum a))
                                            (flatten_sum b)
  | Neg a ->
    flatten_sum_nonempty a;
    FStar.List.Tot.Properties.map_lemma Neg (flatten_sum a)
  | _ -> ()

/// map preserves non-emptiness.
let map_cons (#a #b:Type) (f: a -> b) (xs: list a{Cons? xs})
  : Lemma (ensures Cons? (map f xs)) =
  ()

/// Build Add from a possibly-empty list, with a fallback for the empty case.
let rebuild_add_safe (#k:Type) {| ring k |} (xs: list (rad_expr k)) : rad_expr k =
  match xs with
  | [] -> Lit r_zero
  | [x] -> x
  | x :: rest -> fold_left Add x rest

/// Build Add from a non-empty list.
let rebuild_add (#k:Type) (xs: list (rad_expr k){Cons? xs}) : rad_expr k =
  match xs with
  | [x] -> x
  | x :: rest -> fold_left Add x rest

/// Count addends (for "tiny sum" check).
let rec count_addends (#k:Type) (e: rad_expr k) : Tot nat (decreases e) =
  match e with
  | Add a b -> count_addends a + count_addends b
  | Neg a -> count_addends a
  | _ -> 1

val distribute : rad_expr rational -> Dv (rad_expr rational)
let rec distribute (e: rad_expr rational) : Dv (rad_expr rational) =
  match e with
  (* Lit * (a + b + ...) -> distribute *)
  | Mul (Lit c) r ->
    if is_sum r then begin
      let terms = flatten_sum r in
      flatten_sum_nonempty r;
      let distributed = map (fun t -> Mul (Lit c) t) terms in
      map_cons (fun t -> Mul (Lit c) t) terms;
      rebuild_add distributed
    end
    else Mul (Lit c) (distribute r)
  (* (a + b + ...) * Lit -> distribute *)
  | Mul l (Lit c) ->
    if is_sum l then begin
      let terms = flatten_sum l in
      flatten_sum_nonempty l;
      let distributed = map (fun t -> Mul t (Lit c)) terms in
      map_cons (fun t -> Mul t (Lit c)) terms;
      rebuild_add distributed
    end
    else Mul (distribute l) (Lit c)
  (* (a + b) * x when sum is tiny *)
  | Mul l r ->
    if is_sum l && count_addends l <= 2 && not (is_sum r) then begin
      let terms = flatten_sum l in
      let distributed = map_dv (fun t -> distribute (Mul t r)) terms in
      rebuild_add_safe #rational #ring_rational distributed
    end
    else if is_sum r && count_addends r <= 2 && not (is_sum l) then begin
      let terms = flatten_sum r in
      let distributed = map_dv (fun t -> distribute (Mul l t)) terms in
      rebuild_add_safe #rational #ring_rational distributed
    end
    else Mul (distribute l) (distribute r)
  (* c * (-a) -> -(c*a) handled via general recursion *)
  | Neg a -> Neg (distribute a)
  | Add a b -> Add (distribute a) (distribute b)
  | Inv a -> Inv (distribute a)
  | Root n a -> Root n (distribute a)
  | Pow a n -> Pow (distribute a) n
  | e -> e

// ---------------------------------------------------------------------------
// Composed normalization
// ---------------------------------------------------------------------------

/// A single normalization pass (all sub-passes composed once).
val normalize_once : rad_expr rational -> Dv (rad_expr rational)
let normalize_once e =
  collect_terms
    (collect_coefficients
      (distribute
        (sort_commutative
          (extract_perfect_powers
            (simplify_powers
              (fold_constants
                (flatten_arith e)))))))

/// Iterate a function until fixed point or fuel runs out.
val fix_n : nat -> (rad_expr rational -> Dv (rad_expr rational)) -> rad_expr rational -> Dv (rad_expr rational)
let rec fix_n fuel f x =
  if fuel = 0 then x
  else
    let x' = f x in
    if expr_eq x' x then x
    else fix_n (fuel - 1) f x'

/// Apply all normalization passes, iterated to a fixed point (max 10 iterations).
val normalize : rad_expr rational -> Dv (rad_expr rational)
let normalize e = fix_n 10 normalize_once e
