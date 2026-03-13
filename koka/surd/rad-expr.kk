// Core AST for radical expressions, parameterised by the coefficient type.
//
// Normalization is explicit: these constructors are "dumb".
// Call normalization/denesting functions separately to simplify.
module surd/rad-expr

import std/core/undiv
import surd/rational

// A radical expression over a coefficient field k.
pub type rad-expr<k>
  // Coefficient literal
  Lit(value : k)
  // Negation
  Neg(expr : rad-expr<k>)
  // Sum
  RAdd(left : rad-expr<k>, right : rad-expr<k>)
  // Product
  RMul(left : rad-expr<k>, right : rad-expr<k>)
  // Multiplicative inverse (1/expr)
  Inv(expr : rad-expr<k>)
  // Principal nth root of expr, where n >= 2
  Root(index : int, radicand : rad-expr<k>)
  // Integer power (may be negative)
  Pow(base : rad-expr<k>, exponent : int)

// Subtraction as RAdd(a, Neg(b)).
pub fun rsub(a : rad-expr<k>, b : rad-expr<k>) : rad-expr<k>
  RAdd(a, Neg(b))

// Division as RMul(a, Inv(b)).
pub fun rdiv(a : rad-expr<k>, b : rad-expr<k>) : rad-expr<k>
  RMul(a, Inv(b))

// Square root shorthand.
pub fun sqrt(x : rad-expr<k>) : rad-expr<k>
  Root(2, x)

// Cube root shorthand.
pub fun cbrt(x : rad-expr<k>) : rad-expr<k>
  Root(3, x)

// Lift a rational number into a radical expression.
pub fun rat-expr(r : rational) : rad-expr<rational>
  Lit(r)

// Lift an integer into a radical expression over rationals.
pub fun int-expr(n : int) : rad-expr<rational>
  Lit(from-int(n))

// Map over the coefficient type.
pub fun rad-map(e : rad-expr<a>, f : (a) -> b) : div rad-expr<b>
  match e
    Lit(v)      -> Lit(f(v))
    Neg(x)      -> Neg(rad-map(x, f))
    RAdd(a, b)  -> RAdd(rad-map(a, f), rad-map(b, f))
    RMul(a, b)  -> RMul(rad-map(a, f), rad-map(b, f))
    Inv(x)      -> Inv(rad-map(x, f))
    Root(n, x)  -> Root(n, rad-map(x, f))
    Pow(x, n)   -> Pow(rad-map(x, f), n)

// Fold over all coefficient values in the expression.
pub fun rad-fold(e : rad-expr<a>, init : b, f : (b, a) -> b) : div b
  match e
    Lit(v)      -> f(init, v)
    Neg(x)      -> rad-fold(x, init, f)
    RAdd(a, b)  -> rad-fold(b, rad-fold(a, init, f), f)
    RMul(a, b)  -> rad-fold(b, rad-fold(a, init, f), f)
    Inv(x)      -> rad-fold(x, init, f)
    Root(_, x)  -> rad-fold(x, init, f)
    Pow(x, _)   -> rad-fold(x, init, f)

// Show a radical expression (basic, unambiguous output).
pub fun rad-expr/show(e : rad-expr<k>, showk : (k) -> string) : div string
  match e
    Lit(v)      -> showk(v)
    Neg(x)      -> "-(" ++ x.show(showk) ++ ")"
    RAdd(a, b)  -> "(" ++ a.show(showk) ++ " + " ++ b.show(showk) ++ ")"
    RMul(a, b)  -> "(" ++ a.show(showk) ++ " * " ++ b.show(showk) ++ ")"
    Inv(x)      -> "1/(" ++ x.show(showk) ++ ")"
    Root(n, x)  -> "root(" ++ n.show ++ ", " ++ x.show(showk) ++ ")"
    Pow(x, n)   -> "(" ++ x.show(showk) ++ ")^" ++ n.show

// Show a radical expression over rationals.
pub fun rad-rational/show(e : rad-expr<rational>) : div string
  e.show(fn(r : rational) r.show)

// Check structural equality (requires equality on coefficients).
pub fun rad-eq(a : rad-expr<k>, b : rad-expr<k>, eq : (k, k) -> bool) : div bool
  match a
    Lit(x) -> match b
      Lit(y) -> eq(x, y)
      _      -> False
    Neg(x) -> match b
      Neg(y) -> rad-eq(x, y, eq)
      _      -> False
    RAdd(a1, a2) -> match b
      RAdd(b1, b2) -> rad-eq(a1, b1, eq) && rad-eq(a2, b2, eq)
      _            -> False
    RMul(a1, a2) -> match b
      RMul(b1, b2) -> rad-eq(a1, b1, eq) && rad-eq(a2, b2, eq)
      _            -> False
    Inv(x) -> match b
      Inv(y) -> rad-eq(x, y, eq)
      _      -> False
    Root(n1, x) -> match b
      Root(n2, y) -> n1 == n2 && rad-eq(x, y, eq)
      _           -> False
    Pow(x, n1) -> match b
      Pow(y, n2) -> n1 == n2 && rad-eq(x, y, eq)
      _          -> False

// Count the number of nodes in the expression tree.
pub fun node-count(e : rad-expr<k>) : div int
  match e
    Lit(_)      -> 1
    Neg(x)      -> 1 + node-count(x)
    RAdd(a, b)  -> 1 + node-count(a) + node-count(b)
    RMul(a, b)  -> 1 + node-count(a) + node-count(b)
    Inv(x)      -> 1 + node-count(x)
    Root(_, x)  -> 1 + node-count(x)
    Pow(x, _)   -> 1 + node-count(x)

// Depth of the expression tree.
pub fun depth(e : rad-expr<k>) : div int
  match e
    Lit(_)      -> 0
    Neg(x)      -> 1 + depth(x)
    RAdd(a, b)  -> 1 + max(depth(a), depth(b))
    RMul(a, b)  -> 1 + max(depth(a), depth(b))
    Inv(x)      -> 1 + depth(x)
    Root(_, x)  -> 1 + depth(x)
    Pow(x, _)   -> 1 + depth(x)
