package surd

/** Radical expression AST, parameterised by coefficient type K.
  *
  * Normalization is explicit: these constructors are "dumb".
  * Call normalization/simplification functions separately.
  */
enum RadExpr[+K]:
  /** Coefficient literal. */
  case Lit(value: K)
  /** Negation. */
  case Neg(expr: RadExpr[K])
  /** Sum. */
  case Add(left: RadExpr[K], right: RadExpr[K])
  /** Product. */
  case Mul(left: RadExpr[K], right: RadExpr[K])
  /** Multiplicative inverse. */
  case Inv(expr: RadExpr[K])
  /** Principal nth root (n >= 2). */
  case Root(n: Int, expr: RadExpr[K])
  /** Integer power (may be negative). */
  case Pow(expr: RadExpr[K], n: Int)

object RadExpr:
  import RadExpr.*

  /** Subtraction as Add(a, Neg(b)). */
  def sub[K](a: RadExpr[K], b: RadExpr[K]): RadExpr[K] = Add(a, Neg(b))

  /** Division as Mul(a, Inv(b)). */
  def div[K](a: RadExpr[K], b: RadExpr[K]): RadExpr[K] = Mul(a, Inv(b))

  /** Square root shorthand. */
  def sqrt[K](x: RadExpr[K]): RadExpr[K] = Root(2, x)

  /** Lift a Rational into a radical expression. */
  def ratE(r: Rational): RadExpr[Rational] = Lit(r)

  /** Lift an integer into a radical expression over Rational. */
  def intE(n: Int): RadExpr[Rational] = Lit(Rational(n))

  /** Lift a BigInt into a radical expression over Rational. */
  def intE(n: BigInt): RadExpr[Rational] = Lit(Rational(n))

  /** Bottom-up expression transformer combinator.
    *
    * Recursively transforms all children first, then applies f to the result.
    * Useful for normalization passes that work on already-transformed subtrees.
    */
  def transform[K](f: RadExpr[K] => RadExpr[K])(expr: RadExpr[K]): RadExpr[K] =
    val transformed = expr match
      case Lit(_)      => expr
      case Neg(a)      => Neg(transform(f)(a))
      case Add(a, b)   => Add(transform(f)(a), transform(f)(b))
      case Mul(a, b)   => Mul(transform(f)(a), transform(f)(b))
      case Inv(a)      => Inv(transform(f)(a))
      case Root(n, a)  => Root(n, transform(f)(a))
      case Pow(a, n)   => Pow(transform(f)(a), n)
    f(transformed)

  /** Map over coefficients (Functor). */
  def map[A, B](expr: RadExpr[A])(f: A => B): RadExpr[B] = expr match
    case Lit(v)      => Lit(f(v))
    case Neg(e)      => Neg(map(e)(f))
    case Add(l, r)   => Add(map(l)(f), map(r)(f))
    case Mul(l, r)   => Mul(map(l)(f), map(r)(f))
    case Inv(e)      => Inv(map(e)(f))
    case Root(n, e)  => Root(n, map(e)(f))
    case Pow(e, n)   => Pow(map(e)(f), n)

  // --- Convenience operator extensions ---

  extension [K](a: RadExpr[K])
    def +(b: RadExpr[K]): RadExpr[K] = Add(a, b)
    def -(b: RadExpr[K]): RadExpr[K] = RadExpr.sub(a, b)
    def *(b: RadExpr[K]): RadExpr[K] = Mul(a, b)
    def /(b: RadExpr[K]): RadExpr[K] = RadExpr.div(a, b)
    def unary_- : RadExpr[K] = Neg(a)
    def **(n: Int): RadExpr[K] = Pow(a, n)
