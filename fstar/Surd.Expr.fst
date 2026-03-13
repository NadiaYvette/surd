/// Structural queries on radical expressions.
///
/// depth, expr_size, free_of, collect_radicals, topo_sort_radicals.
module Surd.Expr

open Surd.Ring
open Surd.Rational
open Surd.Types
open FStar.List.Tot

// ---------------------------------------------------------------------------
// Structural queries
// ---------------------------------------------------------------------------

/// Nesting depth (same as expr_depth in Types, re-exported for API symmetry).
let depth (#k:Type) (e: rad_expr k) : nat = expr_depth e

/// Number of nodes (same as expr_size in Types).
let size (#k:Type) (e: rad_expr k) : nat = expr_size e

/// Check if all coefficients satisfy a predicate.
/// (Named `free_of` after the Haskell version, which checks if the
/// expression is "free of" certain coefficient values.)
let rec free_of (#k:Type) (p: k -> bool) (e: rad_expr k) : Tot bool (decreases e) =
  match e with
  | Lit c -> p c
  | Neg a -> free_of p a
  | Add a b -> free_of p a && free_of p b
  | Mul a b -> free_of p a && free_of p b
  | Inv a -> free_of p a
  | Root _ a -> free_of p a
  | Pow a _ -> free_of p a

/// Check if an expression contains any Root nodes.
let rec has_roots (#k:Type) (e: rad_expr k) : Tot bool (decreases e) =
  match e with
  | Root _ _ -> true
  | Lit _ -> false
  | Neg a -> has_roots a
  | Add a b -> has_roots a || has_roots b
  | Mul a b -> has_roots a || has_roots b
  | Inv a -> has_roots a
  | Pow a _ -> has_roots a

// ---------------------------------------------------------------------------
// Radical collection
// ---------------------------------------------------------------------------

/// Structural equality for rad_expr rational.
let rec expr_eq_r (a b : rad_expr rational) : Tot bool (decreases a) =
  match a, b with
  | Lit x, Lit y -> rat_eq x y
  | Neg x, Neg y -> expr_eq_r x y
  | Add x1 x2, Add y1 y2 -> expr_eq_r x1 y1 && expr_eq_r x2 y2
  | Mul x1 x2, Mul y1 y2 -> expr_eq_r x1 y1 && expr_eq_r x2 y2
  | Inv x, Inv y -> expr_eq_r x y
  | Root n1 x, Root n2 y -> n1 = n2 && expr_eq_r x y
  | Pow x n1, Pow y n2 -> n1 = n2 && expr_eq_r x y
  | _, _ -> false

/// Check if a pair (n, e) is in the list.
let rec pair_mem (n: int) (e: rad_expr rational)
                 (xs: list (int & rad_expr rational))
  : Tot bool (decreases xs) =
  match xs with
  | [] -> false
  | (m, f) :: rest -> (n = m && expr_eq_r e f) || pair_mem n e rest

/// Collect all (rootIndex, radicand) pairs, deduplicating.
let rec collect_radicals_go (e: rad_expr rational)
  : Tot (list (int & rad_expr rational)) (decreases e) =
  match e with
  | Lit _ -> []
  | Neg a -> collect_radicals_go a
  | Add a b -> collect_radicals_go a @ collect_radicals_go b
  | Mul a b -> collect_radicals_go a @ collect_radicals_go b
  | Inv a -> collect_radicals_go a
  | Pow a _ -> collect_radicals_go a
  | Root n a -> collect_radicals_go a @ [(n, a)]

/// Remove duplicate (int, rad_expr rational) pairs preserving order (nub).
let rec nub_pairs (xs: list (int & rad_expr rational))
  (seen: list (int & rad_expr rational))
  : Tot (list (int & rad_expr rational)) (decreases xs) =
  match xs with
  | [] -> []
  | (n, e) :: rest ->
    if pair_mem n e seen then nub_pairs rest seen
    else (n, e) :: nub_pairs rest ((n, e) :: seen)

/// Collect distinct (rootIndex, radicand) pairs from an expression.
let collect_radicals (e: rad_expr rational) : list (int & rad_expr rational) =
  nub_pairs (collect_radicals_go e) []

// ---------------------------------------------------------------------------
// Topological sort of radicals
// ---------------------------------------------------------------------------

/// Check whether all Root subexpressions in a radicand appear in the resolved set.
let rec all_roots_resolved (resolved: list (int & rad_expr rational))
                           (e: rad_expr rational)
  : Tot bool (decreases e) =
  match e with
  | Lit _ -> true
  | Neg a -> all_roots_resolved resolved a
  | Add a b -> all_roots_resolved resolved a && all_roots_resolved resolved b
  | Mul a b -> all_roots_resolved resolved a && all_roots_resolved resolved b
  | Inv a -> all_roots_resolved resolved a
  | Pow a _ -> all_roots_resolved resolved a
  | Root n a -> pair_mem n a resolved

/// Topologically sort radicals: radicals with rational radicands come first,
/// then radicals whose radicands depend only on earlier radicals.
/// Uses Dv because the fixed-point loop may not terminate in general.
val topo_sort_radicals : list (int & rad_expr rational) -> Dv (list (int & rad_expr rational))
let topo_sort_radicals rads =
  let rec go (sorted: list (int & rad_expr rational))
             (remaining: list (int & rad_expr rational))
    : Dv (list (int & rad_expr rational)) =
    match remaining with
    | [] -> sorted
    | _ ->
      let ready = filter (fun ((_, e) : int & rad_expr rational) ->
                            all_roots_resolved sorted e) remaining in
      let not_ready = filter (fun ((n, e) : int & rad_expr rational) ->
                                not (all_roots_resolved sorted e)) remaining in
      match ready with
      | [] -> sorted @ remaining  (* can't resolve more; append as-is *)
      | _ -> go (sorted @ ready) not_ready
  in
  go [] rads

// ---------------------------------------------------------------------------
// Map over coefficients
// ---------------------------------------------------------------------------

/// Map a function over all coefficient literals.
let map_coeffs (#a #b: Type) (f: a -> b) (e: rad_expr a) : rad_expr b =
  map_expr f e

/// Count the number of distinct radicals in an expression.
let radical_count (e: rad_expr rational) : nat =
  length (collect_radicals e)
