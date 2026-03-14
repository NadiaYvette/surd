// Polymorphic evaluation of radical expressions via algebraic effects.
//
// Instead of writing separate eval-double, eval-complex, etc., we define
// a single `eval-expr` that performs effect operations. Callers install
// the appropriate handler for their target type (float64, complex, etc.).
module surd/eval-effect

import std/core/undiv
import std/num/float64
import surd/rational
import surd/rad-expr
import surd/eval

// ---------------------------------------------------------------
// Coefficient evaluation effect
// ---------------------------------------------------------------

pub effect coeff-eval<v>
  fun coeff-from-rational(r : rational) : v
  fun coeff-add(a : v, b : v) : v
  fun coeff-mul(a : v, b : v) : v
  fun coeff-neg(x : v) : v
  fun coeff-inv(x : v) : v
  fun coeff-nthroot(n : int, x : v) : v
  fun coeff-pow(x : v, n : int) : v

// ---------------------------------------------------------------
// Generic evaluation: one function, any target type
// ---------------------------------------------------------------

pub fun eval-expr(e : rad-expr<rational>) : <coeff-eval<v>,div> v
  match e
    Lit(r)     -> coeff-from-rational(r)
    Neg(a)     -> coeff-neg(eval-expr(a))
    RAdd(a, b) -> coeff-add(eval-expr(a), eval-expr(b))
    RMul(a, b) -> coeff-mul(eval-expr(a), eval-expr(b))
    Inv(a)     -> coeff-inv(eval-expr(a))
    Root(n, a) -> coeff-nthroot(n, eval-expr(a))
    Pow(a, n)  -> coeff-pow(eval-expr(a), n)

// ---------------------------------------------------------------
// Handler: float64 evaluation
// ---------------------------------------------------------------

fun float64-pow-int(x : float64, n : int) : float64
  if n >= 0 then float64/pow(x, n.float64)
  else float64/(/)(1.0, float64/pow(x, (0 - n).float64))

pub fun with-double-eval(action : () -> <coeff-eval<float64>|e> a) : e a
  with handler
    fun coeff-from-rational(r) r.num.float64 / r.den.float64
    fun coeff-add(a, b) float64/(+)(a, b)
    fun coeff-mul(a, b) float64/(*)(a, b)
    fun coeff-neg(x) float64/(-)(0.0, x)
    fun coeff-inv(x) float64/(/)(1.0, x)
    fun coeff-nthroot(n, x) float64/pow(x, float64/(/)(1.0, n.float64))
    fun coeff-pow(x, n) float64-pow-int(x, n)
  action()

// ---------------------------------------------------------------
// Handler: complex evaluation
// ---------------------------------------------------------------

pub fun with-complex-eval(action : () -> <coeff-eval<complex>|e> a) : e a
  with handler
    fun coeff-from-rational(r) complex-from-real(float64/(/)(r.num.float64, r.den.float64))
    fun coeff-add(a, b) complex/(+)(a, b)
    fun coeff-mul(a, b) complex/(*)(a, b)
    fun coeff-neg(x) complex/negate(x)
    fun coeff-inv(x) complex/inv(x)
    fun coeff-nthroot(n, x) complex-nth-root(n, x)
    fun coeff-pow(x, n) complex/pow-int(x, n)
  action()

// ---------------------------------------------------------------
// Backward-compatible wrappers
// ---------------------------------------------------------------

// Evaluate a radical expression to float64 via the effect system.
pub fun eval-double-e(e : rad-expr<rational>) : div float64
  with-double-eval{ eval-expr(e) }

// Evaluate a radical expression to complex via the effect system.
pub fun eval-complex-e(e : rad-expr<rational>) : div complex
  with-complex-eval{ eval-expr(e) }
