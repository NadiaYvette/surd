// Galois group identification for irreducible polynomials over Q.
//
// For degree 5: discriminant + sextic resolvent + Frobenius test.
// For any prime degree p: generalized Stauduhar descent via
// factorisation pattern analysis modulo small primes.
module surd/identify

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/factoring
import surd/resolvent
import surd/eval
import surd/transitive-group
import surd/prime-factors

// Result of Galois group identification.
pub struct galois-result
  gr-group : transitive-group  // The identified transitive group
  gr-roots : list<complex>     // Approximate complex roots

// ---------------------------------------------------------------
// Entry points
// ---------------------------------------------------------------

// Identify the Galois group of an irreducible polynomial over Q.
// Uses the general prime-degree algorithm for all prime degrees >= 3.
pub fun identify-galois-group(f : poly<rational>) : <div,exn> maybe<galois-result>
  val d = f.degree
  if d >= 3 && is-prime(d) then identify-galois-group-prime(f)
  else Nothing

// Legacy degree-5 identification returning the enum type.
pub fun identify-galois-group5(f : poly<rational>) : exn transitive-group5
  if f.degree != 5 then throw("identify-galois-group5: degree must be 5")
  else
    val d = discriminant(f)
    val d-is-square = is-perfect-square-rational(d)
    val res = sextic-resolvent(f)
    val res-factors = factor-square-free(res)
    val n-factors = res-factors.length
    val factor-degs = res-factors.map(fn(p) p.degree)
    if d-is-square then
      if has-linear-factor(factor-degs) then
        if n-factors >= 4 then Z5
        else D5
      else A5Group
    else
      if has-linear-factor(factor-degs) then F20
      else S5Group

// Degree-5 identification returning the general transitive-group type.
fun identify-galois-group5-general(f : poly<rational>) : <div,exn> maybe<galois-result>
  val g5 = identify-galois-group5(f)
  val name = match g5
    Z5 -> "C5"
    D5 -> "D5"
    F20 -> "F20"
    A5Group -> "A5"
    S5Group -> "S5"
  val groups = trans-groups-of-degree(5)
  val group = groups.filter(fn(g) g.tg-name == name).head
  match group
    Just(tg) ->
      val roots = compute-complex-roots(f)
      Just(Galois-result(gr-group = tg, gr-roots = roots))
    Nothing -> Nothing

// ---------------------------------------------------------------
// General prime-degree identification
// ---------------------------------------------------------------

// Identify the Galois group of a prime-degree polynomial via
// generalized Frobenius/Chebotarev pattern analysis.
//
// Strategy:
// 1. Compute discriminant. Perfect square => G <= A_p.
// 2. Collect factorisation patterns mod small primes.
// 3. Check if all patterns are consistent with AGL(1,p).
// 4. If inside AGL(1,p), find the smallest stabiliser d
//    via lcm of observed non-trivial cycle lengths.
// 5. Otherwise, A_p or S_p based on discriminant.
fun identify-galois-group-prime(f : poly<rational>) : <div,exn> maybe<galois-result>
  val n = f.degree
  val p = n
  val disc = discriminant(f)
  val disc-sq = is-perfect-square-rational(disc)
  val roots = compute-complex-roots(f)
  val groups = trans-groups-of-degree(n)

  // Clear denominators to get integer coefficients
  val cs = f.coeffs
  val lcm-den = cs.foldl(1, fn(acc, c) lcm-int(acc, c.den))
  val int-cs = cs.map(fn(c) (c * from-int(lcm-den)).num)
  val lc = int-cs.last.default(1)
  val disc-n = disc.num
  val disc-d = disc.den

  // Collect good primes for testing
  val test-primes = small-primes.filter fn(pr)
    lc % pr != 0 && disc-n % pr != 0 && disc-d % pr != 0
  val test-primes' = test-primes.take(50)

  // Collect factorisation patterns
  val patterns = test-primes'.map fn(pr)
    val reduced = int-cs.map(fn(c) ((c % pr) + pr) % pr)
    fp-factor-pattern(reduced, pr)

  // Check if all patterns are consistent with AGL(1,p)
  val inside-agl = patterns.all(fn(pat) is-agl-pattern(pat, n))

  // Find the minimum stabiliser d
  val non-triv-cycle-lengths = patterns.flatmap fn(pat)
    if pat == [n] then []                          // translation
    elif pat == list(1, n).map(fn(_) 1) then []    // identity (all 1s)
    else pat.filter(fn(k) k != 1)

  val min-d = if non-triv-cycle-lengths.is-empty then 1
              else non-triv-cycle-lengths.foldl(1, fn(acc, k) lcm-int(acc, k))

  if !inside-agl then
    // Not in AGL(1,p): either A_p or S_p
    val final-group = if disc-sq then
        groups.filter(fn(g) g.tg-name == "A" ++ p.show).head.default(groups.last.default(groups.head.unjust))
      else
        groups.last.default(groups.head.unjust)
    Just(Galois-result(gr-group = final-group, gr-roots = roots))
  else
    // Inside AGL(1,p): find smallest d >= min-d that divides p-1
    val divs = groups.filter(fn(g) g.tg-solvable).map(fn(g) g.tg-order / p).sort-ints
    val group-d = match divs.filter(fn(d) d >= min-d)
      Cons(d, _) -> d
      [] -> p - 1
    val final-group = match groups.filter(fn(g) g.tg-order == p * group-d)
      Cons(g, _) -> g
      [] -> match groups.filter(fn(g) g.tg-name == "AGL(1," ++ p.show ++ ")")
        Cons(g, _) -> g
        [] -> groups.last.default(groups.head.unjust)
    Just(Galois-result(gr-group = final-group, gr-roots = roots))

// Check if a factorisation pattern is consistent with AGL(1,p).
// AGL patterns: [p] (translation), [1,1,...,1] (identity),
// or [1, k, k, ...k] (non-translation with one fixed point).
fun is-agl-pattern(pat : list<int>, n : int) : bool
  if pat == [n] then True                            // translation
  elif pat == list(1, n).map(fn(_) 1) then True      // identity
  elif pat.length >= 2 then
    val ones = pat.filter(fn(x) x == 1)
    val non-ones = pat.filter(fn(x) x != 1)
    ones.length == 1 && !non-ones.is-empty && non-ones.all(fn(x) x == non-ones.head.default(0))
  else False

// ---------------------------------------------------------------
// Factorisation patterns modulo p (Fp polynomial arithmetic)
// ---------------------------------------------------------------

// Distinct-degree factorisation pattern of a polynomial over F_p.
fun fp-factor-pattern(fcs : list<int>, p : int) : list<int>
  val f = fp-trim(fcs)
  pretend-no-div{ fp-factor-pattern-rec([], 1, f, [0, 1], p) }

fun fp-factor-pattern-rec(degs : list<int>, k : int, f : list<int>, h : list<int>, p : int) : div list<int>
  if fp-deg(f) <= 0 then degs.sort-ints
  else
    // h <- h^p mod f (computes x^{p^k} mod f)
    val h' = fp-pow-mod(h, p, f, p)
    // g = gcd(h' - x, f) mod p
    val hx = fp-sub(h', [0, 1], p)
    val g = fp-gcd(hx, f, p)
    val gd = fp-deg(g)
    if gd == 0 then fp-factor-pattern-rec(degs, k + 1, f, h', p)
    else
      val nf = gd / k
      val f' = fp-div(f, g, p)
      fp-factor-pattern-rec(degs ++ list(1, nf).map(fn(_) k), k + 1, f', h', p)

// Remove trailing zeros from coefficient list.
fun fp-trim(cs : list<int>) : list<int>
  cs.reverse.drop-while(fn(c) c == 0).reverse

// Degree of F_p polynomial.
fun fp-deg(cs : list<int>) : int
  fp-trim(cs).length - 1

// Addition over F_p.
fun fp-add(a : list<int>, b : list<int>, p : int) : list<int>
  pretend-no-div{ fp-add-rec(a, b, p) }

fun fp-add-rec(a : list<int>, b : list<int>, p : int) : div list<int>
  val n = max(a.length, b.length)
  val a' = a ++ list(1, n - a.length).map(fn(_) 0)
  val b' = b ++ list(1, n - b.length).map(fn(_) 0)
  fp-trim(a'.zip(b').map(fn(pair) (pair.fst + pair.snd) % p))

// Subtraction over F_p.
fun fp-sub(a : list<int>, b : list<int>, p : int) : list<int>
  pretend-no-div{ fp-sub-rec(a, b, p) }

fun fp-sub-rec(a : list<int>, b : list<int>, p : int) : div list<int>
  val n = max(a.length, b.length)
  val a' = a ++ list(1, n - a.length).map(fn(_) 0)
  val b' = b ++ list(1, n - b.length).map(fn(_) 0)
  fp-trim(a'.zip(b').map(fn(pair) ((pair.fst - pair.snd) % p + p) % p))

// Multiplication over F_p (schoolbook).
fun fp-mul(a : list<int>, b : list<int>, p : int) : list<int>
  if a.is-empty || b.is-empty then []
  else
    val na = a.length
    val nb = b.length
    val result = list(0, na + nb - 2).map fn(i)
      val s = list(0, i).foldl(0) fn(acc, j)
        if j < na && (i - j) < nb then
          val aj = a.drop(j).head.default(0)
          val bij = b.drop(i - j).head.default(0)
          acc + aj * bij
        else acc
      s % p
    fp-trim(result)

// Polynomial remainder over F_p.
fun fp-mod(a : list<int>, b : list<int>, p : int) : list<int>
  pretend-no-div{ fp-mod-rec(fp-trim(a), fp-trim(b), p) }

fun fp-mod-rec(a : list<int>, b : list<int>, p : int) : div list<int>
  val da = fp-deg(a)
  val db = fp-deg(b)
  if da < db then fp-trim(a.map(fn(x) ((x % p) + p) % p))
  elif fp-trim(b).is-empty then a // division by zero guard
  else
    val ta = fp-trim(a)
    val tb = fp-trim(b)
    val lc-b = tb.last.default(1)
    val lc-b-inv = fp-inv(lc-b, p)
    val shift = da - db
    val lc-a = ta.last.default(0)
    val fac = (lc-a * lc-b-inv) % p
    val sub = list(0, ta.length - 1).map fn(i)
      val ta-i = ta.drop(i).head.default(0)
      val bi = if i >= shift && (i - shift) < tb.length then
                 tb.drop(i - shift).head.default(0)
               else 0
      ((ta-i - fac * bi) % p + p) % p
    fp-mod-rec(fp-trim(sub), tb, p)

// Polynomial exact division over F_p.
fun fp-div(a : list<int>, b : list<int>, p : int) : list<int>
  pretend-no-div{ fp-div-rec([], fp-trim(a), fp-trim(b), p) }

fun fp-div-rec(q : list<int>, r : list<int>, b : list<int>, p : int) : div list<int>
  val db = fp-deg(b)
  val dr = fp-deg(r)
  if dr < db then fp-trim(q)
  else
    val tb = fp-trim(b)
    val tr = fp-trim(r)
    val lc-b-inv = fp-inv(tb.last.default(1), p)
    val shift = dr - db
    val fac = (tr.last.default(0) * lc-b-inv) % p
    val q-term = list(1, shift).map(fn(_) 0) ++ [fac]
    val q' = fp-add(q, q-term, p)
    val sub = list(0, tr.length - 1).map fn(i)
      if i >= shift && (i - shift) < tb.length then
        (fac * tb.drop(i - shift).head.default(0)) % p
      else 0
    val r' = fp-trim(tr.zip(sub).map(fn(pair) ((pair.fst - pair.snd) % p + p) % p))
    fp-div-rec(q', r', b, p)

// GCD over F_p, made monic.
fun fp-gcd(a : list<int>, b : list<int>, p : int) : list<int>
  pretend-no-div{ fp-gcd-rec(a, b, p) }

fun fp-gcd-rec(a : list<int>, b : list<int>, p : int) : div list<int>
  if fp-trim(b).is-empty || fp-deg(b) < 0 then fp-make-monic(fp-trim(a), p)
  else fp-gcd-rec(b, fp-mod(a, b, p), p)

// Make monic over F_p.
fun fp-make-monic(cs : list<int>, p : int) : list<int>
  match cs
    [] -> []
    _ ->
      val lc = cs.last.default(1)
      val lc-inv = fp-inv(lc, p)
      cs.map(fn(c) (c * lc-inv) % p)

// Modular exponentiation of polynomial over F_p.
fun fp-pow-mod(base : list<int>, expo : int, modulus : list<int>, p : int) : list<int>
  pretend-no-div{ fp-pow-mod-rec([1], base, expo, modulus, p) }

fun fp-pow-mod-rec(res : list<int>, b : list<int>, e : int, m : list<int>, p : int) : div list<int>
  if e == 0 then res
  else
    val res' = if is-odd(e) then fp-mod(fp-mul(res, b, p), m, p) else res
    val b' = fp-mod(fp-mul(b, b, p), m, p)
    fp-pow-mod-rec(res', b', e / 2, m, p)

// Modular inverse via extended Euclidean algorithm.
fun fp-inv(a : int, m : int) : int
  val (_, x, _) = extended-gcd(a, m)
  ((x % m) + m) % m

fun extended-gcd(a : int, b : int) : (int, int, int)
  pretend-no-div{ ext-gcd-rec(a, b) }

fun ext-gcd-rec(a : int, b : int) : div (int, int, int)
  if a == 0 then (b, 0, 1)
  else
    val (g, x, y) = ext-gcd-rec(b % a, a)
    (g, y - (b / a) * x, x)

// ---------------------------------------------------------------
// Frobenius/Chebotarev test for cyclic vs dihedral
// ---------------------------------------------------------------

// Frobenius test: returns True if all observed patterns are
// consistent with a cyclic Galois group (only [n] and [1,...,1]).
pub fun is-cyclic-by-frobenius(f : poly<rational>, n : int) : exn bool
  val cs = f.coeffs
  val lcm-den = cs.foldl(1, fn(acc, c) lcm-int(acc, c.den))
  val int-cs = cs.map(fn(c) (c * from-int(lcm-den)).num)
  val lc = int-cs.last.default(1)
  val disc = discriminant(f)
  val disc-n = disc.num
  val disc-d = disc.den
  val test-ps = small-primes.filter(fn(p)
    lc % p != 0 && disc-n % p != 0 && disc-d % p != 0
  ).take(20)
  !test-ps.any fn(p)
    val reduced = int-cs.map(fn(c) ((c % p) + p) % p)
    has-non-cyclic-pattern(reduced, p, n)

fun has-non-cyclic-pattern(int-cs : list<int>, p : int, n : int) : bool
  val pat = fp-factor-pattern(int-cs, p)
  pat != [n] && pat != list(1, n).map(fn(_) 1)

// ---------------------------------------------------------------
// Numerical root computation
// ---------------------------------------------------------------

// Compute approximate complex roots of a polynomial via Durand-Kerner iteration.
pub fun compute-complex-roots(f : poly<rational>) : <div,exn> list<complex>
  val n = f.degree
  if n <= 0 then []
  else
    val mp = monic-poly(f, rational-field)
    val cs = mp.coeffs
    // Initial guesses: equally spaced on a circle
    val radius = 1.0 + cs.map(fn(c) float64/abs(c.num.float64 / c.den.float64)).foldl(0.0, fn(a, b) max(a, b))
    val initial = list(0, n - 1).map fn(k)
      val angle = 2.0 * 3.14159265358979323846 * k.float64 / n.float64 + 0.4
      Complex(radius * cos(angle), radius * sin(angle))
    // Durand-Kerner iteration
    durand-kerner(cs, initial, 200)

// Durand-Kerner iteration for polynomial root-finding.
fun durand-kerner(cs : list<rational>, roots : list<complex>, iters : int) : <div> list<complex>
  if iters <= 0 then roots
  else
    val new-roots = dk-step(cs, roots, roots, 0)
    durand-kerner(cs, new-roots, iters - 1)

// One step of Durand-Kerner: update each root.
fun dk-step(cs : list<rational>, roots : list<complex>, current : list<complex>, idx : int) : <div> list<complex>
  match current
    [] -> []
    Cons(r, rest) ->
      // Evaluate polynomial at r
      val pr = eval-poly-complex(cs, r)
      // Denominator: product of (r - r_j) for j != idx
      val denom = roots.map-indexed(fn(j, rj)
        if j == idx then complex-one
        else r - rj
      ).foldl(complex-one, fn(a, b) a * b)
      val new-r = if magnitude(denom) < 1.0e-30 then r
                  else r - (pr / denom)
      Cons(new-r, dk-step(cs, roots, rest, idx + 1))

// Evaluate a monic polynomial (given as coefficient list) at a complex point.
fun eval-poly-complex(cs : list<rational>, z : complex) : complex
  // cs = [a0, a1, ..., a_{n-1}, 1] (monic)
  // Horner: (...((z + a_{n-1})*z + a_{n-2})*z + ... + a_0
  cs.reverse.foldl(complex-zero, fn(acc, c)
    acc * z + complex-from-real(c.num.float64 / c.den.float64)
  )

// ---------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------

fun has-linear-factor(degs : list<int>) : bool
  degs.any(fn(d) d == 1)

fun is-perfect-square-rational(r : rational) : bool
  if r.is-neg then False
  elif r.is-zero then True
  else
    match exact-int-sqrt(r.num)
      Just(_) -> match exact-int-sqrt(r.den)
        Just(_) -> True
        Nothing -> False
      Nothing -> False

fun exact-int-sqrt(n : int) : maybe<int>
  if n < 0 then Nothing
  elif n == 0 then Just(0)
  elif n == 1 then Just(1)
  else
    val s = float64/sqrt(n.float64).int
    if s * s == n then Just(s)
    elif (s + 1) * (s + 1) == n then Just(s + 1)
    else Nothing

fun lcm-int(a : int, b : int) : int
  if a == 0 || b == 0 then 0
  else std/core/int/abs(a * b) / gcd-int(a, b)

// Small primes for Frobenius/Chebotarev test.
// Small primes for Frobenius/Chebotarev test.
val small-primes : list<int> = [
  3, 5, 7, 11, 13, 17, 19, 23, 29, 31,
  37, 41, 43, 47, 53, 59, 61, 67, 71, 73,
  79, 83, 89, 97, 101, 103, 107, 109, 113
]

// Merge sort for int lists.
fun sort-ints(xs : list<int>) : list<int>
  pretend-no-div{ merge-sort-ints(xs) }

fun merge-sort-ints(xs : list<int>) : div list<int>
  match xs
    [] -> []
    [_] -> xs
    _ ->
      val n = xs.length / 2
      merge-ints(merge-sort-ints(xs.take(n)), merge-sort-ints(xs.drop(n)))

fun merge-ints(xs : list<int>, ys : list<int>) : list<int>
  match xs
    Nil -> ys
    Cons(x, xs') -> match ys
      Nil -> xs
      Cons(y, ys') ->
        if x > y then Cons(y, merge-ints(xs, ys'))
        else Cons(x, merge-ints(xs', ys))
