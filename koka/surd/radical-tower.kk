// Radical tower construction for solvable polynomials via Lagrange resolvents.
//
// Given an irreducible polynomial f(x) in Q[x] of prime degree n with
// solvable Galois group G, constructs radical expressions for its roots.
//
// 9-step pipeline:
// 1. Depress: eliminate the x^{n-1} term.
// 2. Find cyclic ordering: permute roots so Galois generator = n-cycle.
// 3. Lagrange resolvents: R_j = Σ_k ω_n^{jk} α_k.
// 4. DFT: compute d_s from R_j^n.
// 5. Coefficient matching: recognise d_s in the appropriate field.
// 6. Reconstruct R_j^n as RadExpr, take nth root.
// 7. Branch selection via numerical comparison.
// 8. Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j.
// 9. Un-depress and match to original root ordering.
module surd/radical-tower

import std/core/undiv
import std/num/float64
import surd/rational
import surd/ring
import surd/poly
import surd/rad-expr
import surd/eval
import surd/normalize
import surd/transitive-group
import surd/identify
import surd/trig-galois

// A step in a radical tower: adjoin the nth root of an expression.
pub struct tower-step
  ts-index : int                   // root index n
  ts-radicand : rad-expr<rational> // the radicand

// A radical tower: a sequence of adjunctions.
pub struct radical-tower
  rt-steps : list<tower-step>
  rt-roots : list<rad-expr<rational>>

// ---------------------------------------------------------------
// Entry points
// ---------------------------------------------------------------

// Solve a solvable polynomial of any supported prime degree.
// Returns Nothing if the group is not solvable or the degree is unsupported.
pub fun solve-via-tower-n(gr : galois-result, f : poly<rational>) : <div,exn> maybe<list<rad-expr<rational>>>
  val tg = gr.gr-group
  if !tg.tg-solvable then Nothing
  else
    val n = f.degree
    if n < 3 then Nothing
    else solve-solvable-prime(tg, f, gr.gr-roots)

// Legacy: solve a solvable quintic.
pub fun solve-via-tower(f : poly<rational>, g : transitive-group5) : <div,exn> maybe<list<rad-expr<rational>>>
  if !transitive-group5/is-solvable(g) then Nothing
  else
    // Convert to general flow
    val gr = identify-galois-group(f)
    match gr
      Just(result) -> solve-via-tower-n(result, f)
      Nothing -> Nothing

// ---------------------------------------------------------------
// Core solver: solvable polynomial of prime degree n
// ---------------------------------------------------------------

fun solve-solvable-prime(tg : transitive-group, f : poly<rational>, num-roots : list<complex>) : <div,exn> maybe<list<rad-expr<rational>>>
  val n = f.degree

  // Step 1: Depress (make monic and shift to eliminate x^{n-1} term)
  val cs = f.coeffs
  val lc = cs.last.default(surd/rational/one)
  val monic-cs = cs.map(fn(c) c / lc)
  val an1 = monic-cs.drop(n - 1).head.default(surd/rational/zero)
  val shift-val = an1.negate / from-int(n)
  val dep-roots = num-roots.map fn(r)
    val sv = shift-val.num.float64 / shift-val.den.float64
    Complex(r.re - sv, r.im)

  // Step 2: Find cyclic ordering
  val ordering = find-cyclic-ordering-n(dep-roots, n, tg)
  match ordering
    Nothing -> Nothing
    Just(ord) ->
      val ordered-roots = ord.map(fn(i) dep-roots.drop(i).head.default(complex-zero))

      // Step 3: Lagrange resolvents R_j = Σ_k ω_n^{jk} α_k
      val rj-vals = list(0, n - 1).map fn(j)
        list(0, n - 1).foldl(complex-zero) fn(acc, k)
          acc + omega-n-c(n, j * k) * ordered-roots.drop(k).head.default(complex-zero)
      val rj-pows = rj-vals.map(fn(r) complex/pow-int(r, n))

      // Step 4: DFT: d_s = (1/n) Σ_j ω_n^{-js} R_j^n
      val d-vals = list(0, n - 1).map fn(s)
        val sum = list(0, n - 1).foldl(complex-zero) fn(acc, j)
          acc + omega-n-c(n, n - (j * s) % n) * rj-pows.drop(j).head.default(complex-zero)
        complex-from-real(1.0 / n.float64) * sum

      // Step 5: Match d_s to radical expressions
      val d-exprs = match-ds-general(tg, d-vals, n)
      match d-exprs
        Nothing -> Nothing
        Just(ds) ->
          // Step 6: R_j^n = Σ_s d_s · ω_n^{js}
          val om-expr = omega-n-expr(n)
          val rj-pow-exprs = list(1, n - 1).map fn(j)
            val terms = list(0, n - 1).map fn(s)
              RMul(ds.drop(s).head.default(Lit(surd/rational/zero)),
                   omega-pow-n-expr(n, om-expr, (j * s) % n))
            foldl1-add(terms)

          // Step 7: Branch selection: R_j = ω_n^{b_j} · ⁿ√(R_j^n)
          val rj-exprs = list(1, n - 1).map fn(j)
            select-branch-n(n, om-expr,
              rj-pow-exprs.drop(j - 1).head.default(Lit(surd/rational/zero)),
              rj-vals.drop(j).head.default(complex-zero))

          // R_0 = sum of depressed roots = 0
          val all-r = Cons(Lit(surd/rational/zero), rj-exprs)

          // Step 8: Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j
          val root-exprs = list(0, n - 1).map fn(k)
            val terms = list(0, n - 1).map fn(j)
              RMul(omega-pow-n-expr(n, om-expr, (n - (j * k) % n) % n),
                   all-r.drop(j).head.default(Lit(surd/rational/zero)))
            val sum-expr = foldl1-add(terms)
            normalize(RMul(Inv(Lit(from-int(n))), sum-expr))

          // Step 9: Un-depress and match to original root ordering
          val final-exprs = root-exprs.map fn(e)
            normalize(RAdd(e, Lit(shift-val)))

          Just(match-to-original(final-exprs, num-roots))

// ---------------------------------------------------------------
// Cyclic ordering
// ---------------------------------------------------------------

// Find a cyclic ordering of n numerical roots such that the Galois
// generator acts as the n-cycle (0 1 2 ... n-1).
fun find-cyclic-ordering-n(roots : list<complex>, n : int, tg : transitive-group) : <div,exn> maybe<list<int>>
  if n <= 8 then
    // For small n: brute-force all (n-1)! orderings
    val rest = list(1, n - 1)
    val orderings = permutations(rest).map(fn(p) Cons(0, p))
    val scored = orderings.map(fn(o) (o, score-ordering-n(roots, o, n, tg)))
    val sorted = sort-scored(scored)
    match sorted
      Cons((best-o, best-score), rest') -> match rest'
        Cons((_, second-score), _) ->
          if best-score < 5.0 * n.float64 && best-score < 0.5 * second-score then Just(best-o)
          elif best-score < n.float64 then Just(best-o)
          else Nothing
        [] -> if best-score < 5.0 * n.float64 then Just(best-o) else Nothing
      [] -> Nothing
  else
    // For large n: try (n-1) rotations
    find-cyclic-ordering-by-rotation(roots, n, tg)

// For large primes: fix root 0 at position 0, try each candidate for
// position 1, then greedily build the rest.
fun find-cyclic-ordering-by-rotation(roots : list<complex>, n : int, tg : transitive-group) : <div,exn> maybe<list<int>>
  val candidates = list(1, n - 1).filter-map fn(next)
    val ordering = build-ordering(roots, n, 0, next)
    if ordering.length == n then
      Just((ordering, score-ordering-n(roots, ordering, n, tg)))
    else Nothing
  val sorted = sort-scored(candidates)
  match sorted
    Cons((best-o, best-score), _) ->
      if best-score < 5.0 * n.float64 then Just(best-o)
      else Nothing
    [] -> Nothing

// Build an ordering by choosing root `start` at position 0 and root
// `next` at position 1, then greedily assigning nearest-neighbour.
fun build-ordering(roots : list<complex>, n : int, start : int, next : int) : <div> list<int>
  val r-start = roots.drop(start).head.default(complex-zero)
  val r-next = roots.drop(next).head.default(complex-zero)
  val step = r-next - r-start
  pretend-no-div{ build-ordering-rec([start, next], next, step, roots, n) }

fun build-ordering-rec(acc : list<int>, pos : int, step : complex, roots : list<complex>, n : int) : div list<int>
  if acc.length >= n then acc
  else
    val target = roots.drop(pos).head.default(complex-zero) + step
    val unused = list(0, n - 1).filter(fn(i) !acc.any(fn(j) j == i))
    match unused
      [] -> acc
      _ ->
        val closest = unused.foldl(unused.head.default(0)) fn(best, i)
          val d-best = magnitude(roots.drop(best).head.default(complex-zero) - target)
          val d-i = magnitude(roots.drop(i).head.default(complex-zero) - target)
          if d-i < d-best then i else best
        build-ordering-rec(acc ++ [closest], closest, step, roots, n)

// Score a candidate ordering: lower = better fit.
// For cyclic groups all d_s should be rational; for dihedral, conjugate pairs.
pub fun score-ordering-n(roots : list<complex>, ordering : list<int>, n : int, tg : transitive-group) : <div> float64
  val ordered = ordering.map(fn(i) roots.drop(i).head.default(complex-zero))
  fun rj(j : int) : complex
    list(0, n - 1).foldl(complex-zero) fn(acc, k)
      val om = omega-n-c(n, j * k)
      val root-k = ordered.drop(k).head.default(complex-zero)
      acc + om * root-k
  val rj-pows = list(0, n - 1).map(fn(j) complex/pow-int(rj(j), n))
  val d-vals = list(0, n - 1).map fn(s)
    val sum = list(0, n - 1).foldl(complex-zero) fn(acc, j)
      acc + omega-n-c(n, n - (j * s) % n) * rj-pows.drop(j).head.default(complex-zero)
    complex-from-real(1.0 / n.float64) * sum
  val p = n
  val d = tg.tg-order / p
  if d == 1 then
    // Cyclic: all d_s should be rational
    d-vals.foldl(0.0, fn(acc, dv) acc + score-rational(dv))
  elif d == 2 then
    // Dihedral: d_0 rational, conjugate pairs {d_s, d_{n-s}}
    val d0-score = score-rational(d-vals.head.default(complex-zero))
    val pair-score = list(1, (n - 1) / 2).foldl(0.0) fn(acc, s)
      val ds = d-vals.drop(s).head.default(complex-zero)
      val dns = d-vals.drop(n - s).head.default(complex-zero)
      acc + score-rational(ds + dns) + score-rational(ds * dns)
    d0-score + pair-score
  else
    // General: d_0 should be rational, rest should be in Q(omega_n).
    // Score d_0 by rationality, rest by how well they decompose in
    // the cyclotomic basis (2-term decomposition quality).
    val d0-score = score-rational(d-vals.head.default(complex-zero))
    val rest-score = d-vals.drop(1).foldl(0.0) fn(acc, dv)
      acc + score-q-omega-n(n, dv)
    d0-score + rest-score

// How close is a complex number to a rational?
fun score-rational(z : complex) : float64
  float64/abs(z.im) + frac-part(z.re)

fun frac-part(x : float64) : float64
  float64/abs(x - x.round)

// Score how well a complex number decomposes in Q(omega_n).
// Lower = better. Fast heuristic: check single-term decomposition
// and quick two-term check for the most promising pair.
fun score-q-omega-n(n : int, d : complex) : float64
  // Try single-term: d ≈ r · ω_n^k
  val single-best = list(0, n - 1).foldl(1.0e10) fn(best, k)
    val quotient = d * omega-n-c(n, n - k)
    val sc = score-rational(quotient)
    if sc < best then sc else best
  if single-best < 0.01 then single-best
  else
    // Quick two-term check: try a few promising (j,k) pairs
    // Just check the 3 best pairs by determinant magnitude
    val two-best = list(0, n - 1).foldl(1.0e10) fn(best, j)
      list(j + 1, min(j + 3, n - 1)).foldl(best) fn(best', k)
        val wj = omega-n-c(n, j)
        val wk = omega-n-c(n, k)
        val det = wj.re * wk.im - wj.im * wk.re
        if float64/abs(det) < 1.0e-10 then best'
        else
          val a = (d.re * wk.im - d.im * wk.re) / det
          val b = (wj.re * d.im - wj.im * d.re) / det
          val a-frac = frac-part(a)
          val b-frac = frac-part(b)
          val sc = a-frac + b-frac
          if sc < best' then sc else best'
    // Also try the minimum-norm DFT projection quality
    val proj-score = list(0, n - 1).foldl(0.0) fn(acc, k)
      val wk-conj = omega-n-c(n, n - k)
      val proj = (d * wk-conj).re * (2.0 / n.float64)
      acc + frac-part(proj)
    val proj-avg = proj-score / n.float64
    min(two-best, proj-avg)

// ---------------------------------------------------------------
// DFT coefficient matching
// ---------------------------------------------------------------

// Match DFT coefficients for a general solvable group at prime degree.
fun match-ds-general(tg : transitive-group, d-vals : list<complex>, n : int) : <div,exn> maybe<list<rad-expr<rational>>>
  val p = n
  val d = tg.tg-order / p
  if d == 1 then match-ds-all-rational(d-vals)
  elif d == 2 then match-ds-dihedral(d-vals, n)
  else match-ds-via-orbits(tg, d-vals, n)

// All d_s must be rational (cyclic Galois group).
fun match-ds-all-rational(d-vals : list<complex>) : exn maybe<list<rad-expr<rational>>>
  val results = d-vals.map fn(dv)
    if float64/abs(dv.im) > 0.05 then Nothing
    else Just(Lit(approx-rat(dv.re)))
  if results.any(fn(r) r.is-nothing) then Nothing
  else Just(results.map(fn(r) r.unjust))

// Dihedral group: d_0 rational, conjugate pairs {d_s, d_{n-s}}.
fun match-ds-dihedral(d-vals : list<complex>, n : int) : <div,exn> maybe<list<rad-expr<rational>>>
  // d_0 must be rational
  val d0 = d-vals.head.default(complex-zero)
  val d0-expr = match-rat-strict(d0)
  match d0-expr
    Nothing -> Nothing
    Just(d0e) ->
      val half-n = (n - 1) / 2
      val pair-results = list(1, half-n).map fn(s)
        val ds = d-vals.drop(s).head.default(complex-zero)
        val dns = d-vals.drop(n - s).head.default(complex-zero)
        match-conj-pair(ds, dns)
      if pair-results.any(fn(r) r.is-nothing) then Nothing
      else
        // Assemble: d_0, d_1, ..., d_{n-1}
        val result = list(0, n - 1).map(fn(_) Lit(surd/rational/zero))
        val filled = fill-in(set-at(result, 0, d0e), 1, pair-results.map(fn(r) r.unjust), n)
        Just(filled)

// Fill in conjugate pair expressions into the result list.
fun fill-in(result : list<rad-expr<rational>>, s : int, pairs : list<(rad-expr<rational>, rad-expr<rational>)>, n : int) : list<rad-expr<rational>>
  match pairs
    [] -> result
    Cons((es, ens), rest) ->
      fill-in(set-at(set-at(result, s, es), n - s, ens), s + 1, rest, n)

// Set element at index in a list.
fun set-at(xs : list<a>, idx : int, val1 : a) : list<a>
  xs.map-indexed(fn(i, x) if i == idx then val1 else x)

// Match a conjugate pair {d_1, d_2} to quadratic expressions.
fun match-conj-pair(d1 : complex, d2 : complex) : <div,exn> maybe<(rad-expr<rational>, rad-expr<rational>)>
  val s = d1 + d2
  val p = d1 * d2
  val s-expr = match-rat-strict(s)
  val p-expr = match-rat-strict(p)
  match (s-expr, p-expr)
    (Just(se), Just(pe)) ->
      val sr = approx-rat(s.re)
      val pr = approx-rat(p.re)
      val disc-r = sr * sr - from-int(4) * pr
      val disc-expr = RAdd(RMul(se, se), Neg(RMul(Lit(from-int(4)), pe)))
      val sqrt-disc = Root(2, disc-expr)
      val e-plus = RMul(Inv(Lit(from-int(2))), RAdd(se, sqrt-disc))
      val e-minus = RMul(Inv(Lit(from-int(2))), RAdd(se, Neg(sqrt-disc)))
      // Determine which branch matches d1
      val disc-r-f = disc-r.num.float64 / disc-r.den.float64
      val sqrt-disc-val = if disc-r-f >= 0.0 then Complex(float64/sqrt(disc-r-f), 0.0)
                          else Complex(0.0, float64/sqrt(float64/abs(disc-r-f)))
      val d1-plus-val = (s + sqrt-disc-val) * complex-from-real(0.5)
      if magnitude(d1-plus-val - d1) < magnitude(d1-plus-val - d2) then
        Just((e-plus, e-minus))
      else
        Just((e-minus, e-plus))
    _ -> Nothing

// Match via orbit structure for larger stabiliser groups.
fun match-ds-via-orbits(tg : transitive-group, d-vals : list<complex>, n : int) : <div,exn> maybe<list<rad-expr<rational>>>
  // Try all-rational first
  match match-ds-all-rational(d-vals)
    Just(exprs) -> Just(exprs)
    Nothing ->
      // d_0 rational, rest matched via Q(omega_n)
      val d0-expr = match-rat-strict(d-vals.head.default(complex-zero))
      match d0-expr
        Nothing -> Nothing
        Just(d0e) ->
          val rest-results = d-vals.drop(1).map fn(dv)
            // Try single-term first, then two-term, then general n-term
            match match-q-omega-n(n, dv)
              Just(e) -> Just(e)
              Nothing -> match-general-q-omega-n(n, dv)
          if rest-results.any(fn(r) r.is-nothing) then Nothing
          else Just(Cons(d0e, rest-results.map(fn(r) r.unjust)))

// Match a complex number to a rational, strict tolerance.
fun match-rat-strict(z : complex) : maybe<rad-expr<rational>>
  if float64/abs(z.im) > 0.1 then Nothing
  else Just(Lit(approx-rat(z.re)))

// Match a complex number to an element of Q(omega_n).
// Tries single-term first, then two-term, then general n-term.
fun match-q-omega-n(n : int, d : complex) : <div,exn> maybe<rad-expr<rational>>
  // Try single-term: d ≈ r · ω_n^k
  match match-single-omega-n(n, d)
    Just(e) -> Just(e)
    Nothing ->
      // Try two-term: d = a·ω_n^j + b·ω_n^k
      match match-two-term-omega-n(n, d)
        Just(e) -> Just(e)
        Nothing -> Nothing

// General n-term matching: d = Σ_{k=0}^{n-1} a_k · ω_n^k
// Uses the minimum-norm DFT projection to find candidate rational coefficients,
// then verifies the reconstruction.
fun match-general-q-omega-n(n : int, d : complex) : <div,exn> maybe<rad-expr<rational>>
  val om-expr = omega-n-expr(n)
  // Project: a_k ≈ (2/n) Re(d · conj(ω^k))
  val raw-coeffs = list(0, n - 1).map fn(k)
    val wk-conj = omega-n-c(n, n - k) // ω^{-k} = conj(ω^k)
    val proj = d * wk-conj
    (2.0 / n.float64) * proj.re
  // Round to rationals
  val rat-coeffs = raw-coeffs.map(approx-rat)
  // Reconstruct and check error
  val recon = list(0, n - 1).foldl(complex-zero) fn(acc, k)
    val ak = rat-coeffs.drop(k).head.default(surd/rational/zero)
    val ak-f = ak.num.float64 / ak.den.float64
    acc + complex-from-real(ak-f) * omega-n-c(n, k)
  val err = magnitude(recon - d)
  if err < 0.01 then
    // Build expression: Σ a_k · ω^k
    val terms = list(0, n - 1).filter-map fn(k)
      val ak = rat-coeffs.drop(k).head.default(surd/rational/zero)
      if ak.is-zero then Nothing
      else
        val term = if k == 0 then Lit(ak)
                   else RMul(Lit(ak), omega-pow-n-expr(n, om-expr, k))
        Just(term)
    match terms
      [] -> Just(Lit(surd/rational/zero))
      _ -> Just(foldl1-add(terms))
  else
    // Try with larger denominator bound
    match match-general-q-omega-n-wide(n, d)
      Just(e) -> Just(e)
      Nothing -> Nothing

// General n-term matching with wider rational approximation (larger denominators).
fun match-general-q-omega-n-wide(n : int, d : complex) : <div,exn> maybe<rad-expr<rational>>
  val om-expr = omega-n-expr(n)
  // Use the redundant basis {ω^0, ..., ω^{n-1}} and try to find rational
  // coefficients. The minimum-norm solution may not give small-denominator
  // rationals, so try a sweep over common denominators.
  //
  // For each candidate denominator D, compute a_k = round((2/n)Re(d·conj(ω^k))·D)/D
  // and check reconstruction.
  val best = pretend-no-div{ search-denom-wide(n, d, 1, Nothing, 1.0) }
  match best
    (Just(rat-cs), _) ->
      val terms = list(0, n - 1).filter-map fn(k)
        val ak = rat-cs.drop(k).head.default(surd/rational/zero)
        if ak.is-zero then Nothing
        else
          val term = if k == 0 then Lit(ak)
                     else RMul(Lit(ak), omega-pow-n-expr(n, om-expr, k))
          Just(term)
      match terms
        [] -> Just(Lit(surd/rational/zero))
        _ -> Just(foldl1-add(terms))
    _ -> Nothing

// Search over denominators for general Q(omega_n) decomposition.
fun search-denom-wide(n : int, d : complex, denom : int,
                       best-cs : maybe<list<rational>>, best-err : float64) : div (maybe<list<rational>>, float64)
  if denom > 10000 then (best-cs, best-err)
  elif best-err < 1.0e-10 then (best-cs, best-err)
  else
    val rat-cs = list(0, n - 1).map fn(k)
      val wk-conj = omega-n-c(n, n - k)
      val proj = (d * wk-conj).re * (2.0 / n.float64)
      val numer = (proj * denom.float64).round.int
      Rational(numer, denom).reduce
    val recon = list(0, n - 1).foldl(complex-zero) fn(acc, k)
      val ak = rat-cs.drop(k).head.default(surd/rational/zero)
      acc + complex-from-real(ak.num.float64 / ak.den.float64) * omega-n-c(n, k)
    val err = magnitude(recon - d)
    if err < 1.0e-8 && err < best-err then
      search-denom-wide(n, d, denom + 1, Just(rat-cs), err)
    else
      search-denom-wide(n, d, denom + 1, best-cs, best-err)

// Try d = r · ω_n^k for rational r and 0 <= k < n.
fun match-single-omega-n(n : int, d : complex) : <div,exn> maybe<rad-expr<rational>>
  val om-expr = omega-n-expr(n)
  // Find k that minimises score-rational(d / ω_n^k)
  val result = list(0, n - 1).foldl((0, complex-zero, 1.0e10)) fn(acc, k)
    val quotient = d * omega-n-c(n, 0 - k)
    val sc = score-rational(quotient)
    match acc
      (_, _, best-sc) -> if sc < best-sc then (k, quotient, sc) else acc
  match result
    (best-k, best-val, best-sc) ->
      if best-sc < 0.01 then
        val r = approx-rat(best-val.re)
        if best-k == 0 then Just(Lit(r))
        else Just(RMul(Lit(r), omega-pow-n-expr(n, om-expr, best-k)))
      else Nothing

// A candidate for two-term omega matching.
struct two-term-candidate
  ttc-err : float64
  ttc-j : int
  ttc-a : rational
  ttc-k : int
  ttc-b : rational

// Try d = a·ω_n^j + b·ω_n^k for rational a, b and j < k.
fun match-two-term-omega-n(n : int, d : complex) : <div,exn> maybe<rad-expr<rational>>
  val om-expr = omega-n-expr(n)
  val candidates = list(0, n - 1).flatmap fn(j)
    list(j + 1, n - 1).filter-map fn(k)
      val wj = omega-n-c(n, j)
      val wk = omega-n-c(n, k)
      // Solve: a·wj + b·wk = d
      val det = wj.re * wk.im - wj.im * wk.re
      if float64/abs(det) < 1.0e-10 then Nothing
      else
        val a = (d.re * wk.im - d.im * wk.re) / det
        val b = (wj.re * d.im - wj.im * d.re) / det
        val ar = approx-rat(a)
        val br = approx-rat(b)
        val recon = complex-from-real(ar.num.float64 / ar.den.float64) * wj +
                    complex-from-real(br.num.float64 / br.den.float64) * wk
        val err = magnitude(recon - d)
        if err < 0.01 then Just(Two-term-candidate(err, j, ar, k, br))
        else Nothing
  val sorted = sort-ttc(candidates)
  match sorted
    Cons(best, _) ->
      val term-j = if best.ttc-j == 0 then Lit(best.ttc-a)
                   else RMul(Lit(best.ttc-a), omega-pow-n-expr(n, om-expr, best.ttc-j))
      val term-k = if best.ttc-k == 0 then Lit(best.ttc-b)
                   else RMul(Lit(best.ttc-b), omega-pow-n-expr(n, om-expr, best.ttc-k))
      Just(RAdd(term-j, term-k))
    [] -> Nothing

// ---------------------------------------------------------------
// ω_n expressions (radical form of roots of unity)
// ---------------------------------------------------------------

// ω_n = e^{2πi/n} as a radical expression.
// Uses allPeriodsViaGauss for the radical form when available.
pub fun omega-n-expr(n : int) : <div,exn> rad-expr<rational>
  if n == 5 then omega5-expr()
  else
    match all-periods-via-gauss(n)
      Just(periods) ->
        match lookup-period(periods, 1)
          Just(zeta) -> zeta  // ζ_n = e^{2πi/n}
          Nothing -> fallback-omega(n)
      Nothing -> fallback-omega(n)

// Degree-5 fast path: ω_5 = cos(2π/5) + i·sin(2π/5)
fun omega5-expr() : rad-expr<rational>
  val cos5 = RMul(Inv(Lit(from-int(4))), RAdd(Root(2, Lit(from-int(5))), Lit(Rational(-1, 1))))
  val sin5 = RMul(Inv(Lit(from-int(4))),
                  Root(2, RAdd(Lit(from-int(10)), RMul(Lit(from-int(2)), Root(2, Lit(from-int(5)))))))
  val i = Root(2, Lit(Rational(-1, 1)))
  RAdd(cos5, RMul(i, sin5))

// Fallback: ω_n = cos(2π/n) + i·sin(2π/n) as approximate rational literals.
fun fallback-omega(n : int) : rad-expr<rational>
  val theta = 2.0 * 3.14159265358979323846 / n.float64
  val cos-val = cos(theta)
  val sin-val = sin(theta)
  val i = Root(2, Lit(Rational(-1, 1)))
  RAdd(Lit(approx-rat(cos-val)), RMul(i, Lit(approx-rat(sin-val))))

// ω_n^k as a radical expression, reduced mod n.
pub fun omega-pow-n-expr(n : int, om-expr : rad-expr<rational>, k : int) : rad-expr<rational>
  val k' = k % n
  if k' == 0 then Lit(surd/rational/one)
  elif k' == 1 then om-expr
  else Pow(om-expr, k')

// ω_n^k = e^{2πi·k/n} as a complex number for numerical evaluation.
pub fun omega-n-c(n : int, k : int) : complex
  val angle = 2.0 * 3.14159265358979323846 * k.float64 / n.float64
  Complex(cos(angle), sin(angle))

// ---------------------------------------------------------------
// Branch selection
// ---------------------------------------------------------------

// Select the correct branch of the nth root of R_j^n.
// Tries all n candidates ω_n^k · ⁿ√(R_j^n) and picks the closest
// to the known numerical value.
fun select-branch-n(n : int, om-expr : rad-expr<rational>, rjn-expr : rad-expr<rational>, target-val : complex) : <div,exn> rad-expr<rational>
  val rjn-val = eval-complex(rjn-expr)
  val r = magnitude(rjn-val)
  val theta = atan2(rjn-val.im, rjn-val.re)
  val principal-val = Complex(
    float64/pow(r, 1.0 / n.float64) * cos(theta / n.float64),
    float64/pow(r, 1.0 / n.float64) * sin(theta / n.float64)
  )
  val principal-root = Root(n, rjn-expr)
  val scored = list(0, n - 1).map fn(k)
    (k, magnitude(omega-n-c(n, k) * principal-val - target-val))
  val best-k = scored.foldl(scored.head.default((0, 1.0e10))) fn(acc, s)
    if s.snd < acc.snd then s else acc
  if best-k.fst == 0 then principal-root
  else RMul(omega-pow-n-expr(n, om-expr, best-k.fst), principal-root)

// ---------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------

// Approximate a float64 as a small-denominator rational.
fun approx-rat(x : float64) : rational
  pretend-no-div{ approx-rat-search(x) }

fun approx-rat-search(x : float64) : div rational
  approx-rat-loop(x, 1, Rational(x.round.int, 1), float64/abs(x - x.round))

fun approx-rat-loop(x : float64, d : int, best : rational, best-err : float64) : div rational
  if d > 10000 then best
  else
    val n = (x * d.float64).round.int
    val err = float64/abs(n.float64 / d.float64 - x)
    if err < 1.0e-6 && err < best-err then
      approx-rat-loop(x, d + 1, Rational(n, d).reduce, err)
    else
      approx-rat-loop(x, d + 1, best, best-err)

// Reduce a rational to lowest terms.
fun reduce(r : rational) : rational
  if r.num == 0 then Rational(0, 1)
  else
    val g = gcd-int(std/core/int/abs(r.num), std/core/int/abs(r.den))
    val sign = if r.den < 0 then -1 else 1
    Rational(sign * r.num / g, sign * r.den / g)

// Match radical expressions to original numerical roots by proximity.
fun match-to-original(exprs : list<rad-expr<rational>>, num-roots : list<complex>) : <div,exn> list<rad-expr<rational>>
  val expr-vals = exprs.map(fn(e) (e, eval-complex(e)))
  num-roots.map fn(t)
    val best = expr-vals.foldl(expr-vals.head.default((Lit(surd/rational/zero), complex-zero))) fn(acc, ev)
      match (acc, ev)
        ((_, acc-v), (_, ev-v)) ->
          if magnitude(ev-v - t) < magnitude(acc-v - t) then ev else acc
    match best
      (expr, _) -> expr

// Fold a list of expressions into a sum via Add.
fun foldl1-add(xs : list<rad-expr<rational>>) : rad-expr<rational>
  match xs
    [] -> Lit(surd/rational/zero)
    [x] -> x
    Cons(x, rest) -> rest.foldl(x, fn(a, b) RAdd(a, b))

// All permutations of a list.
fun permutations(xs : list<int>) : list<list<int>>
  pretend-no-div{ perms(xs) }

fun perms(xs : list<int>) : div list<list<int>>
  match xs
    [] -> [[]]
    _ -> xs.flatmap fn(x)
      val rest = xs.filter(fn(y) y != x)
      perms(rest).map(fn(p) Cons(x, p))

// Float64 comparison for sorting.
fun cmp-float64(a : float64, b : float64) : order
  if a < b then Lt
  elif a > b then Gt
  else Eq

// Sort a list of (value, score) pairs by score (merge sort).
fun sort-scored(xs : list<(a, float64)>) : list<(a, float64)>
  pretend-no-div{ merge-sort-scored(xs) }

fun merge-sort-scored(xs : list<(a, float64)>) : div list<(a, float64)>
  match xs
    [] -> []
    [_] -> xs
    _ ->
      val n = xs.length / 2
      merge-scored(merge-sort-scored(xs.take(n)), merge-sort-scored(xs.drop(n)))

fun merge-scored(xs : list<(a, float64)>, ys : list<(a, float64)>) : list<(a, float64)>
  match xs
    Nil -> ys
    Cons(x, xs') -> match ys
      Nil -> xs
      Cons(y, ys') ->
        if x.snd > y.snd then Cons(y, merge-scored(xs, ys'))
        else Cons(x, merge-scored(xs', ys))

// Sort two-term candidates by error.
fun sort-ttc(xs : list<two-term-candidate>) : list<two-term-candidate>
  pretend-no-div{ merge-sort-ttc(xs) }

fun merge-sort-ttc(xs : list<two-term-candidate>) : div list<two-term-candidate>
  match xs
    [] -> []
    [_] -> xs
    _ ->
      val n = xs.length / 2
      merge-ttc(merge-sort-ttc(xs.take(n)), merge-sort-ttc(xs.drop(n)))

fun merge-ttc(xs : list<two-term-candidate>, ys : list<two-term-candidate>) : list<two-term-candidate>
  match xs
    Nil -> ys
    Cons(x, xs') -> match ys
      Nil -> xs
      Cons(y, ys') ->
        if x.ttc-err > y.ttc-err then Cons(y, merge-ttc(xs, ys'))
        else Cons(x, merge-ttc(xs', ys))

// Compute approximate rational roots for numerical fallback (legacy).
fun compute-approx-roots(p : poly<rational>) : list<rational>
  val pts = list(-100, 100).map(fn(i) from-int(i) * Rational(1, 10))
  val roots = find-roots-scan(p, pts)
  if roots.length < p.degree then
    roots ++ list(1, p.degree - roots.length).map(fn(_) surd/rational/zero)
  else roots.take(p.degree)

fun find-roots-scan(p : poly<rational>, pts : list<rational>) : list<rational>
  match pts
    [] -> []
    [_] -> []
    Cons(x1, rest) -> match rest
      Cons(x2, _) ->
        val f1 = eval-poly(p, x1, rational-ring)
        val f2 = eval-poly(p, x2, rational-ring)
        val tail-roots = find-roots-scan(p, rest)
        if (f1.is-pos && f2.is-neg) || (f1.is-neg && f2.is-pos) then
          val root = (x1 + x2) * Rational(1, 2)
          Cons(root, tail-roots)
        else tail-roots
      _ -> []
