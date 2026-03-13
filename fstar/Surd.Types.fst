/// Core AST for radical expressions, parameterised by the coefficient type.
/// Normalization is explicit: these constructors are "dumb".
module Surd.Types

open Surd.Ring
open Surd.Rational

/// A radical expression over a coefficient field k.
///
/// Lit k      — coefficient literal
/// Neg e      — negation
/// Add e1 e2  — sum
/// Mul e1 e2  — product
/// Inv e      — multiplicative inverse (1/e)
/// Root n e   — principal nth root of e (n >= 2)
/// Pow e n    — integer power (n may be negative)
noeq type rad_expr (k:Type) =
  | Lit  : k -> rad_expr k
  | Neg  : rad_expr k -> rad_expr k
  | Add  : rad_expr k -> rad_expr k -> rad_expr k
  | Mul  : rad_expr k -> rad_expr k -> rad_expr k
  | Inv  : rad_expr k -> rad_expr k
  | Root : n:int{n >= 2} -> rad_expr k -> rad_expr k
  | Pow  : rad_expr k -> int -> rad_expr k

/// Subtraction as Add a (Neg b).
let sub (#k:Type) (a b : rad_expr k) : rad_expr k = Add a (Neg b)

/// Division as Mul a (Inv b).
let rdiv (#k:Type) (a b : rad_expr k) : rad_expr k = Mul a (Inv b)

/// Square root shorthand: Root 2 x.
let sqrt (#k:Type) (x: rad_expr k) : rad_expr k = Root 2 x

/// Lift a rational number into a radical expression.
let rat_e (r: rational) : rad_expr rational = Lit r

/// Lift an integer into a radical expression over rationals.
let int_e (n: int) : rad_expr rational = Lit (rat_of_int n)

/// Map a function over the coefficient type.
let rec map_expr (#a #b: Type) (f: a -> b) (e: rad_expr a) : Tot (rad_expr b) (decreases e) =
  match e with
  | Lit k -> Lit (f k)
  | Neg e1 -> Neg (map_expr f e1)
  | Add e1 e2 -> Add (map_expr f e1) (map_expr f e2)
  | Mul e1 e2 -> Mul (map_expr f e1) (map_expr f e2)
  | Inv e1 -> Inv (map_expr f e1)
  | Root n e1 -> Root n (map_expr f e1)
  | Pow e1 n -> Pow (map_expr f e1) n

/// Fold over all coefficient literals in an expression.
let rec fold_expr (#k #a: Type) (f: k -> a -> a) (init: a) (e: rad_expr k)
  : Tot a (decreases e) =
  match e with
  | Lit c -> f c init
  | Neg e1 -> fold_expr f init e1
  | Add e1 e2 -> fold_expr f (fold_expr f init e2) e1
  | Mul e1 e2 -> fold_expr f (fold_expr f init e2) e1
  | Inv e1 -> fold_expr f init e1
  | Root _ e1 -> fold_expr f init e1
  | Pow e1 _ -> fold_expr f init e1

/// Count the number of nodes in an expression tree.
let rec expr_size (#k:Type) (e: rad_expr k) : Tot nat (decreases e) =
  match e with
  | Lit _ -> 1
  | Neg e1 -> 1 + expr_size e1
  | Add e1 e2 -> 1 + expr_size e1 + expr_size e2
  | Mul e1 e2 -> 1 + expr_size e1 + expr_size e2
  | Inv e1 -> 1 + expr_size e1
  | Root _ e1 -> 1 + expr_size e1
  | Pow e1 _ -> 1 + expr_size e1

/// Depth of an expression tree.
let rec expr_depth (#k:Type) (e: rad_expr k) : Tot nat (decreases e) =
  match e with
  | Lit _ -> 0
  | Neg e1 -> 1 + expr_depth e1
  | Add e1 e2 -> 1 + (let d1 = expr_depth e1 in let d2 = expr_depth e2 in if d1 > d2 then d1 else d2)
  | Mul e1 e2 -> 1 + (let d1 = expr_depth e1 in let d2 = expr_depth e2 in if d1 > d2 then d1 else d2)
  | Inv e1 -> 1 + expr_depth e1
  | Root _ e1 -> 1 + expr_depth e1
  | Pow e1 _ -> 1 + expr_depth e1

/// Collect all coefficient literals in an expression (left-to-right).
let collect_lits (#k:Type) (e: rad_expr k) : list k =
  FStar.List.Tot.rev (fold_expr (fun c acc -> c :: acc) [] e)

/// Check if an expression is a simple literal.
let is_lit (#k:Type) (e: rad_expr k) : bool =
  match e with
  | Lit _ -> true
  | _ -> false

/// Extract the literal value if the expression is a Lit.
let get_lit (#k:Type) (e: rad_expr k) : option k =
  match e with
  | Lit c -> Some c
  | _ -> None
