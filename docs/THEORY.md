# Mathematical Foundations of the Surd Library

A comprehensive treatment of the algebraic, number-theoretic, and
Galois-theoretic algorithms underlying the **surd** symbolic radical
algebra system.

---

## Table of Contents

1. [Introduction and Motivation](#1-introduction-and-motivation)
2. [Radical Expressions](#2-radical-expressions)
3. [Normalization](#3-normalization)
4. [Denesting Nested Radicals](#4-denesting-nested-radicals)
5. [Exact Trigonometric Values](#5-exact-trigonometric-values)
6. [DAG-Based Computation](#6-dag-based-computation)
7. [Algebraic Numbers](#7-algebraic-numbers)
8. [Field Extensions](#8-field-extensions)
9. [Polynomial Operations](#9-polynomial-operations)
10. [Galois Theory and Solvable Quintics](#10-galois-theory-and-solvable-quintics)
11. [Symbolic Integration](#11-symbolic-integration)
12. [Arbitrary Precision Evaluation](#12-arbitrary-precision-evaluation)
13. [PSLQ and Integer Relations](#13-pslq-and-integer-relations)
14. [Bibliography](#14-bibliography)

---

## 1. Introduction and Motivation

### 1.1 The Problem Domain

The surd library addresses a cluster of interrelated problems in symbolic
computation:

1. **Radical simplification.** Given an expression built from rational
   numbers, addition, multiplication, and $n$-th roots, determine whether
   it can be written in a simpler form. For instance, is there a simpler
   way to write $\sqrt{3 + 2\sqrt{2}}$?

2. **Radical denesting.** When radicals are nested---a root applied to an
   expression already containing roots---can the nesting depth be reduced?
   The expression $\sqrt{3 + 2\sqrt{2}}$ has nesting depth 2, but it
   equals $1 + \sqrt{2}$, which has depth 1.

3. **Exact trigonometric values.** Every value $\cos(p\pi/q)$ and
   $\sin(p\pi/q)$ for $p, q \in \mathbb{Z}$ can be expressed in radicals.
   Computing these radical forms requires cyclotomic field theory and
   Gauss period descent.

4. **Equality testing.** Given two radical expressions, determine whether
   they represent the same real (or complex) number. This is a non-trivial
   decision problem related to the identity testing of radical expressions
   [Nosan et al. 2022].

5. **Galois-theoretic polynomial solving.** Given an irreducible polynomial
   $f(x) \in \mathbb{Q}[x]$ whose Galois group is solvable, express its
   roots in radicals. This extends the classical quadratic, cubic, and
   quartic formulas to degree 5 and beyond.

6. **Symbolic integration.** Evaluate integrals involving square roots of
   quadratics (via Euler substitution) and square roots of cubics and
   quartics (reduction to Legendre normal forms for elliptic integrals).

### 1.2 Casus Irreducibilis

A central theme in the library is the *casus irreducibilis*, which arises
when a real algebraic quantity can only be expressed in radicals via
complex intermediates.

The prototypical example is the cubic $x^3 - 3x - 1 = 0$, whose three
roots are all real:

$$
x = 2\cos\left(\frac{2\pi k}{9}\right), \quad k = 1, 2, 3.
$$

Cardano's formula gives

$$
x = \sqrt[3]{-\frac{1}{2} + \frac{1}{2}\sqrt{-3}}
    + \sqrt[3]{-\frac{1}{2} - \frac{1}{2}\sqrt{-3}},
$$

which involves $\sqrt{-3}$ despite the root being real. This is not an
artefact of the formula: it was proved by Wantzel (1843) and later by
others that **no** formula involving only real radicals can express these
roots. Any radical expression for a root of an irreducible cubic with
three real roots must pass through the complex numbers.

The surd library handles the casus irreducibilis throughout: the `RadExpr`
type permits complex intermediate expressions (such as $\sqrt{-3}$), and
evaluation routines work with complex arithmetic, extracting the real part
only at the final stage.

### 1.3 Overview of the Approach

The library follows a layered architecture:

- **Bottom layer:** Polynomial arithmetic---univariate and multivariate
  polynomials over $\mathbb{Q}$, GCD, factoring (Kronecker and Trager),
  cyclotomic polynomials, and resultants.

- **Middle layer:** Field extensions $K(\alpha)$ via quotient rings
  $K[x]/(m_\alpha)$, algebraic numbers as (minimal polynomial, isolating
  interval) pairs, and radical expression normalization.

- **Top layer:** Denesting algorithms (Borodin--Fagin--Hopcroft--Tompa for
  square roots, Landau via Trager factoring for general radicals), Gauss
  period descent for trigonometric values, Galois group identification and
  Lagrange resolvent towers for polynomial solving, and symbolic
  integration.

A critical engineering decision is the use of **explicit DAG
representation** for radical expressions. Gauss period descent produces
expressions with enormous amounts of structural sharing (common
subexpressions). Naive tree traversal leads to exponential blowup; the
DAG representation, detected via `StableName` and represented as an
`IntMap`, ensures all algorithms run in time linear in the number of
unique nodes.

---

## 2. Radical Expressions

### 2.1 The RadExpr AST

A radical expression over a coefficient field $k$ is defined by the
algebraic data type:

```haskell
data RadExpr k
  = Lit k              -- coefficient literal
  | Neg (RadExpr k)    -- additive negation
  | Add (RadExpr k) (RadExpr k)  -- addition
  | Mul (RadExpr k) (RadExpr k)  -- multiplication
  | Inv (RadExpr k)    -- multiplicative inverse
  | Root Int (RadExpr k)  -- principal nth root
  | Pow (RadExpr k) Int   -- integer power
```

This type is parameterised by $k$, which is typically `Rational` but can
be any field. The constructors are *unintelligent*: they build syntax trees
without performing any simplification. For instance, `Add (Lit 0) e` is
not simplified to `e`---that is the job of the normalization pipeline
(Section 3).

**Semantics.** The intended interpretation maps each `RadExpr k` to a
value in an algebraic closure of $k$:

- $\text{Lit}(r)$ denotes $r \in k$.
- $\text{Root}(n, e)$ denotes the *principal* $n$-th root of the value of
  $e$. For positive real radicands, this is the positive real root. For
  negative radicands under odd $n$, the real root. For complex radicands,
  the root with the smallest positive argument.
- Other constructors denote the obvious arithmetic operations.

**Pattern synonyms** provide derived forms:

$$
\text{Sub}(a, b) = \text{Add}(a, \text{Neg}(b)), \quad
\text{Div}(a, b) = \text{Mul}(a, \text{Inv}(b)), \quad
\text{Sqrt}(x) = \text{Root}(2, x).
$$

### 2.2 Coefficient Field Parameterisation

The parameterisation by $k$ serves several purposes:

1. **Rational coefficients** ($k = \mathbb{Q}$): the standard case for
   exact symbolic computation.

2. **Algebraic extension coefficients** ($k = \text{ExtElem}$): when
   working inside a field tower $\mathbb{Q}(\alpha)(\beta)\cdots$, the
   coefficients themselves live in an algebraic extension.

3. **Rational function coefficients** ($k = \text{RatFunc}$): for
   transcendental extensions $\mathbb{Q}(x_1, \ldots, x_n)$, enabling
   parametric radical expressions.

The `Functor` instance on `RadExpr` allows mapping over the coefficient
type, enabling conversions between different coefficient fields.

### 2.3 Normal Form Theory

Structural equality on `RadExpr` (the derived `Eq` instance) is
**syntactic**, not semantic. The expressions

$$
\sqrt{2} \cdot \sqrt{2} \quad\text{and}\quad 2
$$

are syntactically different (`Mul (Root 2 (Lit 2)) (Root 2 (Lit 2))` vs
`Lit 2`) but semantically equal. A canonical form is needed for reliable
equality testing.

The fundamental question is: *which canonical form?* There are several
options, each with trade-offs:

- **Tree normalization** (Section 3): a series of rewrite passes brings
  expressions into a *mostly* canonical form. Fast but not fully canonical:
  some semantically equal expressions may normalise to different trees.

- **NormalForm representation** (Section 2.6): a mathematically rigorous
  canonical form based on the $\mathbb{Q}$-vector space structure of
  radical extensions. Fully canonical modulo Besicovitch's theorem, but
  more expensive to compute.

- **Algebraic number conversion** (Section 7): the round-trip RadExpr
  $\to$ AlgNum $\to$ RadExpr is the most powerful simplification, but
  requires computing minimal polynomials (expensive for deeply nested
  expressions).

### 2.4 Besicovitch's Theorem

The theoretical foundation for the NormalForm representation is
Besicovitch's theorem on the linear independence of radicals.

**Theorem** [Besicovitch 1940].
Let $n_1, \ldots, n_r$ be positive
integers $\geq 2$ and let $a_1, \ldots, a_r$ be positive rationals such
that for each $i$, no $n_i$-th power of a rational (other than 1) divides
$a_i$ (that is, $a_i$ is $n_i$-th-power-free). If the radical monomials

$$
\prod_{i=1}^r a_i^{e_i / n_i}, \qquad 0 \leq e_i < n_i,
$$

are all distinct, then they are linearly independent over $\mathbb{Q}$.

This theorem guarantees that the representation of an element of
$\mathbb{Q}(\sqrt[n_1]{a_1}, \ldots, \sqrt[n_r]{a_r})$ as a
$\mathbb{Q}$-linear combination of radical monomials is **unique**,
provided the radicands are in $n_i$-th-power-free form.

**Example.** The set $\{1, \sqrt{2}, \sqrt{3}, \sqrt{6}\}$ is linearly
independent over $\mathbb{Q}$. Therefore, if

$$
a + b\sqrt{2} + c\sqrt{3} + d\sqrt{6} = a' + b'\sqrt{2} + c'\sqrt{3} + d'\sqrt{6},
$$

then $a = a'$, $b = b'$, $c = c'$, $d = d'$.

### 2.5 The Zippel Decidability Result

The identity testing problem for radical expressions asks: given an
expression $E$ built from rationals using $+$, $-$, $\times$, $\div$,
and $\sqrt[n]{\cdot}$, is $E = 0$?

**Theorem** [Zippel 1985].
The identity testing problem for expressions
involving $+$, $-$, $\times$, $\div$, and $n$-th roots of rationals is
decidable.

Zippel's proof is constructive: it provides an algorithm based on
computing the minimal polynomial of the expression. If the minimal
polynomial has degree 1 (i.e., the expression is rational), then $E = 0$
iff the rational value is 0. Otherwise, the minimal polynomial is
irreducible of degree $\geq 2$, and since 0 has minimal polynomial $x$,
the expression is nonzero.

The surd library implements this decision procedure via the
`simplifyViaCanonical` function, which computes the RadExpr $\to$ AlgNum
$\to$ RadExpr round-trip. The intermediate AlgNum carries the minimal
polynomial, which determines whether the expression is zero.

### 2.6 NormalForm Representation

The `NormalForm` module provides a mathematically rigorous canonical form
for elements of $\mathbb{Q}$-radical extensions. The representation has
three layers:

**Atoms.** An atom is an irreducible radical building block:

- $\text{RatRoot}(n, r)$: the $n$-th root of a positive rational $r$,
  where $r$ is $n$-th-power-free.
- $\text{ImagUnit}$: the imaginary unit $i = \sqrt{-1}$. Negative
  radicands are factored: $\sqrt{-r} = i \cdot \sqrt{r}$.
- $\text{NestedRoot}(n, e)$: the $n$-th root of a multi-term radical
  expression $e$ that cannot be decomposed into a product of simpler
  atoms. For example, $\sqrt{2 + \sqrt{3}}$ is a nested root because
  $2 + \sqrt{3}$ is a sum, not a product.

**Monomials.** A monomial is a product of atoms raised to bounded
exponents:

$$
m = \prod_i \text{atom}_i^{e_i},
$$

where exponents are reduced modulo the atom's degree:
- For $\text{RatRoot}(n, r)$: $0 \leq e < n$, since
  $(\sqrt[n]{r})^n = r$ reduces to a rational.
- For $\text{ImagUnit}$: $e \in \{0, 1\}$, since $i^2 = -1$.

Monomials are represented as `Map Atom Int` with canonical ordering.

**NormExpr.** A normalised expression is a $\mathbb{Q}$-linear
combination of monomials:

$$
\text{NormExpr} = \sum_j c_j \cdot m_j, \qquad c_j \in \mathbb{Q},
$$

represented as `Map Monomial Rational`. Zero coefficients are dropped.

**Integral radical canonicalization.** NestedRoot radicands are
canonicalized to coprime integer coefficients. Given a NestedRoot with
rational-coefficient radicand:

1. Clear the LCD (least common denominator) of all coefficients.
2. Extract the GCD content.
3. Factor out perfect $n$-th powers.

For example, $\sqrt[3]{-\tfrac{7}{432} + \tfrac{7}{144}\sqrt{3}}$ becomes
$\tfrac{1}{12} \cdot \sqrt[3]{28} \cdot \sqrt[3]{-1 + 3\sqrt{3}}$, where
$28$ is cube-free and $\{-1, 3\}$ are coprime.

This canonicalization ensures that structurally equivalent radicands are
identified, enabling like-term collection in the NormExpr representation.

**Negative radicand detection.** For even-root NestedRoot atoms, the
`isRadicandNegative` function detects negative radicands and factors out
$i$. It uses a Double evaluation fast path with an interval arithmetic
fallback (`evalInterval` + `strictlyNegative`). For example,
$\sqrt{-2\sqrt{5} - 10}$ becomes $i \cdot \sqrt{2\sqrt{5} + 10}$. This
enables $i$-cancellation in the NormalForm for sin formulas.

### 2.7 Arithmetic in NormalForm

Arithmetic operations on `NormExpr` maintain the canonical form:

- **Addition** ($\text{normAdd}$): merge the two maps, summing
  coefficients of matching monomials. Like-term collection is automatic.

- **Multiplication** ($\text{normMul}$): distribute and multiply each pair
  of monomials. When multiplying atoms, exponents are added and reduced:
  - $(\sqrt[n]{r})^a \cdot (\sqrt[n]{r})^b = (\sqrt[n]{r})^{(a+b) \bmod n} \cdot r^{\lfloor(a+b)/n\rfloor}$
  - $i^a \cdot i^b = i^{(a+b) \bmod 2} \cdot (-1)^{\lfloor(a+b)/2\rfloor}$

- **Inversion** ($\text{normInv}$): for single-monomial expressions, negate
  all exponents and apply $\text{reduceMonomial}$. For multi-term
  expressions, rationalise the denominator via extended GCD on polynomials
  with NormExpr coefficients.

- **Powers** ($\text{normPow}$): repeated squaring with exponent reduction
  at each step.

The conversion functions `toNormExpr` and `fromNormExpr` translate between
`RadExpr` and `NormExpr`, enabling a normalisation round-trip:

$$
\text{expr} \xrightarrow{\text{toNormExpr}} \text{NormExpr}
  \xrightarrow{\text{fromNormExpr}} \text{expr}'.
$$

The output $\text{expr}'$ is in a canonical form where like terms have
been collected and exponents reduced.

---

## 3. Normalization

### 3.1 The Eight-Pass Pipeline

The tree-based normalisation pipeline in `Surd.Radical.Normalize` applies
eight rewrite passes in sequence:

$$
\text{normalize} = \text{fix}_{10}\bigl(\text{collectTerms} \circ
  \text{collectCoefficients} \circ \text{distribute} \circ
  \text{sortCommutative} \circ \text{extractPerfectPowers} \circ
  \text{simplifyPowers} \circ \text{foldConstants} \circ
  \text{flattenArith}\bigr).
$$

Each pass addresses a specific class of simplifications:

**Pass 1: flattenArith.** Flatten nested associative operations:

$$
\text{Add}(\text{Add}(a, b), c) \;\longrightarrow\; \text{Add}(a, \text{Add}(b, c)).
$$

Similarly for nested `Mul` chains. This ensures that subsequent passes
see flat $n$-ary sums and products rather than arbitrary binary tree
shapes.

**Pass 2: foldConstants.** Evaluate purely rational sub-expressions:

$$
\text{Add}(\text{Lit}(3), \text{Lit}(4)) \;\longrightarrow\; \text{Lit}(7).
$$

Also simplifies:
- $\text{Neg}(\text{Lit}(r)) \to \text{Lit}(-r)$
- $\text{Inv}(\text{Lit}(r)) \to \text{Lit}(1/r)$ (for $r \neq 0$)
- $\text{Pow}(\text{Lit}(r), n) \to \text{Lit}(r^n)$
- $\text{Root}(n, \text{Lit}(r)) \to \text{Lit}(r^{1/n})$ when $r$ is a
  perfect $n$-th power

**Pass 3: simplifyPowers.** Reduce powers of roots:

$$
\text{Pow}(\text{Root}(n, e), n) \;\longrightarrow\; e.
$$

More generally, $(\sqrt[n]{e})^k$ with $k \geq n$ is reduced by
extracting copies of $e$:

$$
(\sqrt[n]{e})^k = e^{\lfloor k/n \rfloor} \cdot (\sqrt[n]{e})^{k \bmod n}.
$$

**Pass 4: extractPerfectPowers.** For $\text{Root}(n, e)$ where $e$ is a
literal $r \in \mathbb{Q}$, extract perfect $n$-th power factors from the
numerator and denominator:

$$
\sqrt[n]{a^n \cdot b} = a \cdot \sqrt[n]{b}.
$$

This uses prime factorisation of the numerator and denominator. The
factorisation is computed via trial division, with the `Positive` newtype
ensuring the argument is strictly positive.

**Pass 5: sortCommutative.** Impose a canonical ordering on the children
of `Add` and `Mul` nodes. Since addition and multiplication are
commutative, the order of operands should not affect the normalised form.
The ordering uses the derived `Ord` instance on `RadExpr`, which gives a
structural lexicographic order.

This pass is essential for CSE (common subexpression elimination) and
like-term collection: $\sqrt{2} + \sqrt{3}$ and $\sqrt{3} + \sqrt{2}$
must normalise to the same tree.

**Pass 6: distribute.** Apply the distributive law in controlled
circumstances:

$$
r \cdot (a + b) \;\longrightarrow\; r \cdot a + r \cdot b
\quad\text{(when $r$ is a literal)}.
$$

Distribution is not applied universally (that would cause exponential
blowup), but only when one factor is a literal coefficient and the other
is a small sum. This enables subsequent like-term collection.

**Pass 7: collectCoefficients.** Flatten `Mul` chains and merge rational
factors:

$$
\text{Mul}(\text{Lit}(2), \text{Mul}(\text{Lit}(3), e))
  \;\longrightarrow\; \text{Mul}(\text{Lit}(6), e).
$$

**Pass 8: collectTerms.** Flatten `Add` chains and group terms by their
non-coefficient part, summing the rational coefficients:

$$
2\sqrt{3} + 5\sqrt{3} \;\longrightarrow\; 7\sqrt{3}.
$$

Terms are grouped using a `Map` keyed by the *base expression* (the
expression with the rational coefficient factored out). This requires the
`Ord` instance established by `sortCommutative`.

### 3.2 Fixed-Point Iteration

A single application of the eight passes may not fully normalise an
expression, because one pass can create opportunities for another. For
example:

1. `foldConstants` may evaluate $\text{Root}(2, \text{Lit}(4))$ to
   $\text{Lit}(2)$.
2. This creates a new literal that `distribute` can use.
3. Distribution creates new sum terms that `collectTerms` can merge.

The `fixN` combinator iterates the pipeline up to 10 times, stopping
early if the expression stabilises:

```haskell
fixN 0 _ x = x
fixN n f x = let x' = f x
             in if x' == x then x else fixN (n-1) f x'
```

The equality test uses the derived structural `Eq`. In practice,
convergence occurs within 2--4 iterations for typical expressions.

### 3.3 Convergence

The pipeline is not *guaranteed* to reach a fixed point in bounded
iterations for all possible inputs. However, each pass either reduces the
expression size (by folding constants or collecting terms) or rearranges
it into a more canonical form (sorting, flattening). The fuel limit of 10
prevents pathological cycling.

The passes are designed to be *monotone* in a loose sense: the expression
never grows significantly. Distribution is the only pass that can increase
the number of nodes, and it is guarded by a size check on the sum being
distributed over.

### 3.4 Computational Complexity

Each pass has complexity at most $O(n \log n)$ where $n$ is the number of
nodes in the expression tree (the $\log n$ factor comes from map
operations in `collectTerms`). With at most 10 iterations, the total cost
is $O(n \log n)$.

However, this analysis assumes the expression is a *tree*. For
DAG-structured expressions with sharing, tree-based normalisation breaks
sharing and may cause exponential blowup (see Section 6). The
`dagFoldConstants` and `dagNormalize` operations in the DAG module handle
this case.

---

## 4. Denesting Nested Radicals

### 4.1 Problem Statement

A *nested radical* is an expression where a root is applied to an
expression that itself contains roots. The *nesting depth* is the maximum
number of nested root operations on any path from the root of the
expression tree to a leaf.

**Denesting** is the process of finding an equivalent expression with
lower nesting depth. Not all nested radicals can be denested:
$\sqrt{1 + \sqrt[3]{2}}$ has no simpler form. The question of *when*
denesting is possible, and *how* to compute the denested form, has a rich
mathematical theory.

### 4.2 Square Root Denesting: The Borodin--Fagin--Hopcroft--Tompa Algorithm

The simplest case is $\sqrt{a + b\sqrt{r}}$ where $a, b, r \in \mathbb{Q}$.

**Theorem** [Borodin et al. 1985].
The expression $\sqrt{a + b\sqrt{r}}$
can be denested over $\mathbb{Q}$ if and only if $a^2 - b^2 r$ is a
perfect square in $\mathbb{Q}$.

**Algorithm.** Given $a, b, r \in \mathbb{Q}$:

1. Compute the discriminant $\Delta = a^2 - b^2 r$.
2. If $\Delta < 0$, denesting is impossible. Return failure.
3. Test whether $\Delta$ is a perfect square in $\mathbb{Q}$:
   check if both `numerator` and `denominator` of $\Delta$ are perfect
   squares of integers.
4. If $\Delta = d^2$ with $d \in \mathbb{Q}$, set:

   $$x = \frac{a + d}{2}, \qquad y = \frac{a - d}{2}.$$

5. If $x \geq 0$ and $y \geq 0$, the denested form is:

   $$\sqrt{a + b\sqrt{r}} = \sqrt{x} + \mathrm{sgn}(b) \cdot \sqrt{y}.$$

**Proof sketch.** Suppose $\sqrt{a + b\sqrt{r}} = \sqrt{x} + \sqrt{y}$.
Squaring both sides: $a + b\sqrt{r} = x + y + 2\sqrt{xy}$. Matching
rational and irrational parts: $x + y = a$ and $4xy = b^2 r$, so $x$ and
$y$ are roots of $t^2 - at + b^2 r / 4 = 0$. The discriminant of this
quadratic is $a^2 - b^2 r$.

**Worked example: denesting $\sqrt{3 + 2\sqrt{2}}$.**

Here $a = 3$, $b = 2$, $r = 2$.

1. $\Delta = 9 - 4 \cdot 2 = 1 = 1^2$. Perfect square.
2. $x = (3 + 1)/2 = 2$, $y = (3 - 1)/2 = 1$.
3. Both non-negative, $b > 0$, so:

   $$\sqrt{3 + 2\sqrt{2}} = \sqrt{2} + \sqrt{1} = 1 + \sqrt{2}.$$

**Verification:** $(1 + \sqrt{2})^2 = 1 + 2\sqrt{2} + 2 = 3 + 2\sqrt{2}$. Correct.

**Non-denestable example: $\sqrt{1 + \sqrt{3}}$.**

Here $a = 1$, $b = 1$, $r = 3$. Then $\Delta = 1 - 3 = -2 < 0$, so
denesting is impossible over $\mathbb{Q}$.

### 4.3 Nth Root Denesting

For cube roots and higher, the situation is more complex. The library
implements direct cube root denesting for specific patterns and delegates
the general case to Landau's algorithm.

For cube roots of the form $\sqrt[3]{a + b\sqrt{r}}$ where $a, b, r \in \mathbb{Q}$, the approach is:

1. The minimal polynomial of $\sqrt[3]{a + b\sqrt{r}}$ over $\mathbb{Q}$
   has degree dividing 6 (since the expression lives in an extension of
   degree at most $3 \times 2 = 6$).
2. If the minimal polynomial factors into lower-degree pieces, the
   expression can be simplified.

### 4.4 Landau's Algorithm

Landau's algorithm [Landau 1992] provides a systematic approach to
denesting arbitrary nested radicals. The key insight is the connection
between denesting and polynomial factoring over algebraic extensions.

**Setup.** Given $\sqrt[n]{a}$ where $a$ lies in a radical extension
$K/\mathbb{Q}$, we want to determine if $\sqrt[n]{a}$ can be expressed
using radicals of lower nesting depth.

**Algorithm:**

1. **Build the extension.** Collect all radicals appearing in $a$ and
   construct the algebraic extension $K = \mathbb{Q}(\alpha_1, \ldots, \alpha_r)$ where each $\alpha_i = \sqrt[n_i]{r_i}$ is a radical atom
   in $a$.

2. **Factor $x^n - a$ over $K$.** Using Trager's algorithm (Section 4.5),
   factor the polynomial $x^n - a \in K[x]$.

3. **Extract a root from a factor.** If $x^n - a$ has a factor of degree
   $d < n$ over $K$, then $\sqrt[n]{a}$ can be expressed as a $d$-th root
   of an element of $K$, plus lower-order terms.

4. **Recursive denesting.** Apply the algorithm recursively to the result,
   since the extracted root may itself be denestable.

**Why this works.** The polynomial $x^n - a$ is irreducible over $K$ if
and only if $\sqrt[n]{a}$ generates an extension of degree $n$ over $K$.
If it factors, the extension degree is smaller, meaning $\sqrt[n]{a}$ can
be expressed in terms of lower-degree roots over $K$.

**Degree extraction.** When a factor of degree $d$ is found:

- $d = 1$: $\sqrt[n]{a}$ is already in $K$ (no root needed).
- $d = 2$: use the quadratic formula.
- $d = 3$: use Cardano's formula (with casus irreducibilis handling).
- $d = 4$: use Ferrari's method.
- $d \geq 5$: further analysis needed (Galois group solvability).

**Topological ordering.** When the radicand $a$ contains multiple
radicals with dependencies (e.g., $\sqrt{\sqrt{2} + \sqrt{3}}$), the
radicals must be processed in dependency order---inner radicals first.
The library uses topological sorting of the radical dependency graph.

### 4.5 Trager Factoring

Trager's algorithm [Trager 1984] factors a square-free polynomial $f(x)$
over an algebraic extension $K(\alpha) = K[t]/(m_\alpha(t))$.

**Algorithm** (over $\mathbb{Q}(\alpha)$):

1. Let $f(x) \in \mathbb{Q}(\alpha)[x]$ be the polynomial to factor,
   with $m(t)$ the minimal polynomial of $\alpha$ over $\mathbb{Q}$.

2. Compute the *norm*: $N(x) = \text{Res}_t(f(x - t\alpha), m(t))$, the
   resultant of $f(x - t\alpha)$ and $m(t)$ with respect to $t$. This
   produces a polynomial in $\mathbb{Q}[x]$.

3. Factor $N(x)$ over $\mathbb{Q}$ (using Kronecker or other methods).

4. For each irreducible factor $g(x)$ of $N(x)$, compute
   $\gcd(f(x), g(x + t\alpha))$ in $\mathbb{Q}(\alpha)[x]$. This GCD is
   an irreducible factor of $f$ over $\mathbb{Q}(\alpha)$.

**Key property.** The norm $N(x)$ has the same roots as $f$, but
"unfolded" over $\mathbb{Q}$. Each irreducible factor of $N$ over
$\mathbb{Q}$ corresponds to one or more irreducible factors of $f$ over
$\mathbb{Q}(\alpha)$.

The library implements this in `Math.Polynomial.TragerFactoring`, with
both a base version (`factorSFOverExtension` over $\mathbb{Q}(\alpha)$)
and a generalised version (`factorSFOverExtensionK` over $K(\alpha)$ for
arbitrary base field $K$), enabling factoring over towers of extensions.

### 4.6 Connection to Galois Theory

Denesting has a Galois-theoretic interpretation. A nested radical
$\sqrt[n]{a}$ with $a \in K$ generates a field extension $K(\sqrt[n]{a})$
of degree dividing $n$. Denesting corresponds to finding that this
extension is actually contained in a smaller extension.

More precisely, $\sqrt[n]{a}$ can be denested to depth $d < n$ over $K$
if and only if the extension $K(\sqrt[n]{a})/K$ can be decomposed into
a tower of extensions of degree $< n$. This decomposition corresponds
to factoring the Galois group of $x^n - a$ over $K$.

When the Galois group of $x^n - a$ over $K$ is a proper subgroup of the
symmetric group $S_n$, the polynomial has non-trivial factors, and
denesting is possible. The structure of the Galois group determines the
form of the denested expression.

---

## 5. Exact Trigonometric Values

### 5.1 Classical Results

The problem of expressing trigonometric values at rational multiples of
$\pi$ in closed form has a long history, beginning with the ancient
Greek construction of regular polygons.

**Theorem** (Gauss--Wantzel).
A regular $n$-gon is constructible by
compass and straightedge if and only if $n = 2^a p_1 p_2 \cdots p_k$
where the $p_i$ are distinct Fermat primes ($p = 2^{2^m} + 1$).

The known Fermat primes are $3, 5, 17, 257, 65537$. For constructible
angles, $\cos(2\pi/n)$ can be expressed using only square roots over
$\mathbb{Q}$.

However, the surd library goes far beyond constructible angles.
**Every** value $\cos(p\pi/q)$ with $p, q \in \mathbb{Z}$ can be
expressed in radicals, because cyclotomic extensions have abelian
(hence solvable) Galois groups. The resulting expressions may involve
$n$-th roots for $n > 2$ and may require complex intermediates (the
casus irreducibilis).

### 5.2 Cyclotomic Field Theory

The $n$-th **cyclotomic field** $\mathbb{Q}(\zeta_n)$ is the splitting
field of the $n$-th cyclotomic polynomial

$$
\Phi_n(x) = \prod_{\substack{1 \leq k \leq n \\ \gcd(k,n) = 1}}
  \bigl(x - e^{2\pi i k/n}\bigr).
$$

This polynomial has degree $\varphi(n)$ (Euler's totient) and is
irreducible over $\mathbb{Q}$.

**Key properties:**

1. The Galois group $\mathrm{Gal}(\mathbb{Q}(\zeta_n)/\mathbb{Q}) \cong (\mathbb{Z}/n\mathbb{Z})^{\ast}$, the multiplicative group of integers
   modulo $n$. An element $a \in (\mathbb{Z}/n\mathbb{Z})^{\ast}$ acts by $\sigma_a(\zeta_n) = \zeta_n^a$.

2. Since $(\mathbb{Z}/n\mathbb{Z})^{\ast}$ is abelian, the extension is
   **abelian**, and by the Kronecker--Weber theorem, every abelian
   extension of $\mathbb{Q}$ is contained in some cyclotomic field.

3. Since every abelian group is solvable, $\mathbb{Q}(\zeta_n)/\mathbb{Q}$
   can be decomposed into a tower of cyclic extensions, each of prime
   degree. This decomposition is the basis for expressing $\zeta_n$ in
   radicals.

4. The real subfield $\mathbb{Q}(\zeta_n + \zeta_n^{-1}) = \mathbb{Q}(\cos(2\pi/n))$ has degree $\varphi(n)/2$ over $\mathbb{Q}$.

### 5.3 Gauss Periods

**Gauss periods** are the key tool for descending from $\mathbb{Q}(\zeta_n)$
to $\mathbb{Q}$ through intermediate subfields.

**Definition.** Let $n$ be a prime, $g$ a primitive root modulo $n$, and
$H$ a subgroup of $(\mathbb{Z}/n\mathbb{Z})^{\ast}$ of index $e$. The **Gauss
periods** (of order $e$) are:

$$
\eta_j = \sum_{h \in H} \zeta_n^{g^j h}, \qquad j = 0, 1, \ldots, e-1.
$$

Each $\eta_j$ is a sum of $|H| = \varphi(n)/e$ roots of unity.

**Key property.** The periods $\eta_0, \ldots, \eta_{e-1}$ are the roots
of a degree-$e$ polynomial whose coefficients lie in the subfield fixed
by the coset representatives. When $e$ is prime, this polynomial is called
the **period equation** and its coefficients can be computed from the
elementary symmetric functions of the periods.

**Galois-theoretic meaning.** If $H \leq (\mathbb{Z}/n\mathbb{Z})^{\ast}$,
the fixed field of $H$ is $\mathbb{Q}(\eta_0)$. The Gauss periods
generate intermediate fields in the tower:

$$
\mathbb{Q} \subset \mathbb{Q}(\eta_{\text{coarse}}) \subset
  \mathbb{Q}(\eta_{\text{fine}}) \subset \cdots \subset \mathbb{Q}(\zeta_n).
$$

### 5.4 The Descent Algorithm

The Gauss period descent computes $\cos(2\pi/n)$ by descending through the
subgroup chain of $(\mathbb{Z}/n\mathbb{Z})^{\ast}$.

**Algorithm** (for prime $n$):

1. **Factor $\varphi(n) = n - 1$** into prime factors $q_1, q_2, \ldots, q_s$ (with multiplicity).

2. **Build the subgroup chain:**

   $$\{1\} = H_0 \subset H_1 \subset \cdots \subset H_s = (\mathbb{Z}/n\mathbb{Z})^{\ast},$$

   where $[H_{i+1} : H_i] = q_i$.

3. **At each step $i$**, the periods for $H_i$ split into $q_i$ periods
   for $H_{i-1}$. These $q_i$ sub-periods satisfy a degree-$q_i$
   polynomial whose coefficients are known (they are symmetric functions
   of the sub-periods, expressible in terms of the already-computed
   parent periods).

4. **Solve each period equation** by extracting $q_i$-th roots via
   Lagrange resolvents (Section 5.5).

5. **At the bottom**, the periods are individual roots of unity $\zeta_n^k$.
   Then $\cos(2\pi/n) = \frac{1}{2}(\zeta_n + \zeta_n^{-1})$.

### 5.5 Lagrange Resolvents

At each step of the descent, we must solve a degree-$q$ equation for $q$
sub-periods $\alpha_0, \ldots, \alpha_{q-1}$ that satisfy a known
polynomial relation. The **Lagrange resolvent** provides the solution.

**Definition.** Given $q$ values $\alpha_0, \ldots, \alpha_{q-1}$ and a
primitive $q$-th root of unity $\omega = e^{2\pi i/q}$, the Lagrange
resolvents are:

$$
R_j = \sum_{k=0}^{q-1} \omega^{jk} \alpha_k, \qquad j = 0, 1, \ldots, q-1.
$$

**Key property.** $R_j^q$ is a symmetric function of the $\alpha_k$
(invariant under the cyclic permutation $\alpha_k \mapsto \alpha_{k+1}$),
and therefore lies in the base field. Computing $R_j^q$ and taking the
$q$-th root gives $R_j$, from which the $\alpha_k$ are recovered by the
inverse DFT:

$$
\alpha_k = \frac{1}{q} \sum_{j=0}^{q-1} \omega^{-jk} R_j.
$$

**Branch selection.** Computing $R_j$ from $R_j^q$ requires choosing the
correct $q$-th root branch. The principal $q$-th root $\sqrt[q]{R_j^q}$ may
differ from the true $R_j$ by a power of $\omega$:

$$
R_j = \omega^{b_j} \cdot \sqrt[q]{R_j^q},
$$

where the branch index $b_j$ is determined by numerical comparison. The
library evaluates the expression $\sqrt[q]{R_j^q}$ using `evalComplex` on
the *expression* (not a precomputed Double value) to avoid phase wrapping
at the $\pm\pi$ branch cut.

### 5.6 DFT Matching: Recovering Coefficients

The values $R_j^q$ need to be expressed as radical expressions. Since
they are known to lie in the base field at that stage of the descent,
they can be written as $\mathbb{Q}$-linear combinations of the already-known
periods.

**The DFT relationship.** Write

$$
R_j^q = \sum_{s=0}^{q-1} d_s \, \omega^{js},
$$

where the $d_s$ are the "DFT coefficients". Then

$$
d_s = \frac{1}{q} \sum_{j=0}^{q-1} \omega^{-js} R_j^q.
$$

**Small period count ($\leq 2$).** The $d_s$ values are
$\mathbb{Q}$-linear in the periods. The library uses greedy coefficient
matching: sort the periods by $|\text{Im}|$ descending and assign greedily.
This $O(n)$ approach replaces an earlier $O(3^n)$ backtracking search.

**Large period count.** For 8+ periods (e.g., $p = 89$ with $q = 11$),
the $d_s$ are degree-$(q-2)$ polynomials in the periods, not linear.
The greedy approach may produce spurious solutions that violate coefficient
bounds. The library detects this and falls back to computing $d_s$ as
exact rationals via high-precision MPBall arithmetic (1000-bit precision),
converting to $\mathbb{Q}$ via continued fractions.

### 5.7 Worked Example: $\cos(2\pi/7)$

Let $n = 7$. Then $\varphi(7) = 6 = 2 \times 3$.

**Step 1: Factor and build subgroup chain.**

$g = 3$ is a primitive root mod 7. The powers of $g$ give
$3^0 = 1, 3^1 = 3, 3^2 = 2, 3^3 = 6, 3^4 = 4, 3^5 = 5 \pmod{7}$.

Factor $6 = 2 \times 3$. Subgroup chain:

$$
\{1\} \subset H_1 = \{1, 6\} \subset H_2 = \{1, 2, 3, 4, 5, 6\} = (\mathbb{Z}/7\mathbb{Z})^{\ast}.
$$

Here $[H_2 : H_1] = 3$ and $[H_1 : \{1\}] = 2$.

**Step 2: First descent (index 3).**

The three Gauss periods of order 3 are:

$$
\eta_0 = \zeta^1 + \zeta^6, \quad
\eta_1 = \zeta^3 + \zeta^4, \quad
\eta_2 = \zeta^2 + \zeta^5,
$$

where $\zeta = e^{2\pi i/7}$. Their sum is $\eta_0 + \eta_1 + \eta_2 = \sum_{k=1}^{6} \zeta^k = -1$ (the sum of all non-trivial 7th roots of
unity). The period equation is:

$$
x^3 + x^2 - 2x - 1 = 0.
$$

**Step 3: Solve the cubic via Lagrange resolvents.**

With $\omega = e^{2\pi i/3}$, form:

$$
R_1 = \eta_0 + \omega \eta_1 + \omega^2 \eta_2, \qquad
R_2 = \eta_0 + \omega^2 \eta_1 + \omega \eta_2.
$$

Then $R_1^3$ and $R_2^3$ lie in $\mathbb{Q}(\omega)$. Numerically,
$R_1^3 = -\tfrac{7}{2} + \tfrac{7}{2}\sqrt{-3}$ and
$R_2^3 = \overline{R_1^3}$ (complex conjugate).

**Step 4: Recover the periods.**

$$
\eta_k = \frac{1}{3}\left(-1 + \omega^{-k}\sqrt[3]{R_1^3} + \omega^k \sqrt[3]{R_2^3}\right).
$$

In particular:

$$
\cos(2\pi/7) = \frac{\eta_0}{2} = \frac{1}{2}\left(\zeta + \zeta^{-1}\right).
$$

The minimal polynomial of $\cos(2\pi/7)$ is $8x^3 + 4x^2 - 4x - 1 = 0$.

**Step 5: Simplification.**

The raw Gauss period expression for $\cos(2\pi/7)$ involves 5 radicals.
The `simplifyViaCanonical` pipeline computes the degree-3 minimal
polynomial and applies Cardano's formula to get a Cardano form with 3
radicals---a significant simplification.

### 5.8 Handling Primes, Prime Powers, and Composites

**Primes.** For a prime $p$, $(\mathbb{Z}/p\mathbb{Z})^{\ast}$ is cyclic of
order $p - 1$. The algorithm in Section 5.4 applies directly.

**Odd prime powers.** For $n = p^k$ with $p$ odd, $(\mathbb{Z}/p^k\mathbb{Z})^{\ast}$
is cyclic of order $\varphi(p^k) = p^{k-1}(p-1)$. The descent algorithm
applies with an important modification: the factor of $p$ in the subgroup
chain must come **first** (to avoid degenerate sub-period sums where all
periods in a coset collapse to the same value).

**Composites.** For general composite $n$, the Chinese Remainder Theorem
(CRT) gives

$$
(\mathbb{Z}/n\mathbb{Z})^{\ast} \cong \prod_i (\mathbb{Z}/p_i^{k_i}\mathbb{Z})^{\ast}
$$

when $n = \prod_i p_i^{k_i}$. The CRT decomposition expresses

$$
\zeta_n = \prod_i \zeta_{p_i^{k_i}}^{e_i}
$$

for appropriate CRT lift coefficients $e_i$. Each factor
$\cos(2\pi/p_i^{k_i})$ is computed independently via the prime/prime-power
algorithm, and $\cos(2\pi/n)$ is assembled using Chebyshev identities
(see Section 5.9).

### 5.9 The Chebyshev Connection

The **Chebyshev polynomials of the first kind** $T_k$ satisfy
$T_k(\cos\theta) = \cos(k\theta)$. This identity enables computing
$\cos(2\pi k/n)$ from $\cos(2\pi/n)$ without re-running the Gauss descent.

The first few Chebyshev polynomials are:

$$
T_0(x) = 1, \quad T_1(x) = x, \quad T_2(x) = 2x^2 - 1, \quad
T_3(x) = 4x^3 - 3x.
$$

The general recurrence is $T_{k+1}(x) = 2x \cdot T_k(x) - T_{k-1}(x)$.

For the CRT assembly, if $n = n_1 n_2$ with $\gcd(n_1, n_2) = 1$ and
$s n_1 + t n_2 = 1$ (Bezout coefficients), then

$$
\cos\left(\frac{2\pi}{n}\right)
  = 2\cos\left(\frac{2\pi s}{n_2}\right)\cos\left(\frac{2\pi t}{n_1}\right) - \cos\left(\frac{2\pi(s n_2 - t n_1)}{n}\right).
$$

More directly, the product-to-sum identity gives:

$$
\cos\alpha \cos\beta = \tfrac{1}{2}[\cos(\alpha - \beta) + \cos(\alpha + \beta)].
$$

The library applies NormalForm round-trips to Chebyshev outputs to
simplify the resulting expressions, but guards against blowup for complex
bases with more than 3 radicals.

### 5.10 Direct Period Sin Computation

For sine values, the library uses two approaches:

**1. Direct period sin** (`directPeriodSin`): For prime denominators with
small radical count ($\leq 10$ radicals per period), each period
$\eta_k$ is pre-simplified through the NF round-trip, and $\sin$ is
formed directly:

$$
\sin(2\pi k/n) = \frac{-i}{2}\bigl(\zeta_n^k - \zeta_n^{n-k}\bigr).
$$

The NF round-trip cancels $\text{ImagUnit}$ atoms in the result, yielding
a real expression.

**2. Fallback via cosine** (`sinReduced`):

$$
\sin(p\pi/q) = \pm\sqrt{1 - \cos^2(p\pi/q)},
$$

with the sign determined by the quadrant. This always works but produces
more complex expressions (an additional square root layer).

The direct path gives more compact results for denominators 5, 7, 11,
etc., where the Gauss periods are manageable.

### 5.11 Trig Display Simplification

The raw output of the Gauss period descent may be unnecessarily complex.
The `simplifyTrigResult` function applies two strategies in order:

1. **Canonical simplification** (`tryCanonicalSimplify`): the RadExpr $\to$
   AlgNum $\to$ RadExpr round-trip via Cardano/Ferrari. Guarded by DAG
   size $\leq 200$, radical count $\leq 5$, and a 5-second timeout.

2. **NF round-trip**: convert to NormalForm and back. This collects like
   terms and reduces exponents but does not change the algebraic form.

The ordering matters: NF on raw Gauss period output can explode (many
terms), but NF on compact Cardano output works well.

---

## 6. DAG-Based Computation

### 6.1 The Exponential Blowup Problem

The `RadExpr` type is a tree ADT, but Haskell's lazy evaluation creates
DAG-shaped expressions through *thunk sharing*. When the Gauss period
descent constructs an expression like:

```haskell
let eta = Add (Root 3 x) (Root 3 y)
in Add eta eta  -- shares the thunk for eta
```

the two occurrences of `eta` point to the same heap object. The `RadExpr`
tree has 5 nodes, but the explicit tree unfolding duplicates the shared
subexpression. For deeply nested sharing, the explicit tree can be
exponentially larger than the DAG.

**Concrete example.** Computing $\cos(2\pi/37)$ via Gauss period descent
produces an expression with approximately 1,500 unique nodes but an
explicit tree of roughly $10^{13}$ nodes. Tree-walking normalisation takes
143 seconds; DAG-based normalisation takes 35 milliseconds---a
4,000$\times$ speedup.

### 6.2 StableName-Based Sharing Detection

Haskell does not provide a built-in way to detect sharing in pure data
structures (referential transparency means the language semantics cannot
distinguish shared from unshared values). The library uses `StableName`
from `System.Mem.StableName` to detect thunk identity:

1. Force each subexpression (by pattern-matching on the constructor).
2. Obtain its `StableName` via `makeStableName`.
3. Look up the `StableName` in a hash map. If found, reuse the existing
   `NodeId`. If not, allocate a new `NodeId`.

This technique is safe but uses `unsafePerformIO` internally (as does
`StableName` itself). The key invariant is that the same thunk always
gets the same `StableName` within a single traversal, so shared
subexpressions are identified.

### 6.3 DAG Representation

The explicit DAG is represented as:

```haskell
data RadDAG = RadDAG
  { dagNodes :: IntMap RadNodeOp  -- node operations
  , dagRoot  :: NodeId            -- root node
  }

data RadNodeOp
  = DLit Rational
  | DNeg NodeId
  | DAdd NodeId NodeId
  | DMul NodeId NodeId
  | DInv NodeId
  | DRoot Int NodeId
  | DPow NodeId Int
```

Each `NodeId` is an `Int` key into the `IntMap`. Children are processed
before parents during `toDAG`, so node IDs are in topological order.

The reconstruction `fromDAG` uses lazy `IntMap` lookup to preserve
sharing: the same `NodeId` always returns the same Haskell thunk,
maintaining the DAG structure in the reconstructed `RadExpr`.

### 6.4 O(n) Algorithms on DAGs

With the DAG representation, algorithms that would be exponential on
trees become linear in the number of unique nodes $n$:

**dagFoldConstants.** Bottom-up constant folding: each node is visited
once. If both children of a `DAdd` are `DLit` nodes, the result is a
new `DLit`. This handles the same cases as the tree-based `foldConstants`
but in $O(n)$ time.

**dagEvalComplex.** Bottom-up evaluation to `Complex Double`. Each node is
evaluated once and the result cached in an `IntMap`. Total cost: $O(n)$.

**dagEvalComplexInterval.** Bottom-up evaluation to `ComplexInterval`
(pairs of rational `Interval` values). Provides rigorous error bounds.
Fast for constructible angles (approximately 5ms, interval width roughly
$10^{-18}$). Slower for complex intermediates where `cnthroot` requires
rational atan2/cos/sin.

**dagNormalize.** DAG-native versions of `collectTerms` and
`collectCoefficients` that operate on the flat node array, each in
$O(n \cdot k)$ where $k$ is the maximum fan-in. Available but not needed
for Gauss period output (which has no like terms after DAG fold).

**dagSize / dagDepth.** Structural metrics computed in a single bottom-up
pass.

### 6.5 Performance Results

| Angle           | Tree time | DAG time | Speedup |
|:----------------|----------:|---------:|--------:|
| $\cos(2\pi/23)$ | 0.3 s     | 8 ms     | 38x     |
| $\cos(2\pi/37)$ | 143 s     | 35 ms    | 4,000x  |
| $\cos(2\pi/41)$ | (OOM)     | 42 ms    | --      |
| $\cos(2\pi/61)$ | (OOM)     | 15 ms    | --      |
| $\cos(2\pi/97)$ | (OOM)     | 25 ms    | --      |

The DAG representation makes it practical to compute exact trigonometric
values for *any* denominator, limited only by the size of the radical
expression (which grows polynomially with $\varphi(n)$).

---

## 7. Algebraic Numbers

### 7.1 Definition and Representation

An **algebraic number** $\alpha$ is a root of a nonzero polynomial with
rational coefficients. The **minimal polynomial** $m_\alpha(x) \in \mathbb{Q}[x]$ is the unique monic irreducible polynomial satisfied by
$\alpha$.

In the library, a real algebraic number is represented as a pair:

$$
\text{AlgNum} = (m_\alpha,\, I_\alpha),
$$

where $m_\alpha$ is the minimal polynomial and $I_\alpha = [a, b]$ is a
rational isolating interval---an interval containing exactly one root of
$m_\alpha$.

Two algebraic numbers are equal if and only if they have the same minimal
polynomial and their isolating intervals (after sufficient refinement)
overlap. The refinement is done by bisection using Sturm's theorem
(Section 7.2) to count roots in sub-intervals.

### 7.2 Sturm's Theorem

**Sturm's theorem** provides an effective method for counting real roots
of a polynomial in an interval.

**Definition.** Given a polynomial $f(x)$, the **Sturm sequence** is
$f_0 = f$, $f_1 = f'$, and for $i \geq 2$:

$$
f_i = -\mathrm{rem}(f_{i-2}, f_{i-1}),
$$

where $\mathrm{rem}$ denotes the polynomial remainder. (Note the
negation: this ensures sign variation counting works correctly.)

**Theorem** (Sturm, 1829).
Let $f$ be a square-free polynomial and
$(f_0, f_1, \ldots, f_m)$ its Sturm sequence. For any $a < b$ not roots
of $f$, the number of distinct real roots of $f$ in $(a, b)$ equals

$$
V(a) - V(b),
$$

where $V(c) = \lvert\lbrace i : \mathrm{sign}(f_i(c)) \neq \mathrm{sign}(f_{i+1}(c))\rbrace\rvert$
is the number of sign variations in the sequence evaluated at $c$.

The library uses Sturm's theorem in `Surd.Algebraic.RootIsolation` for:
- **Root isolation:** starting with a large interval and bisecting until
  each sub-interval contains exactly one root.
- **Interval refinement:** given an isolating interval, narrow it to
  any desired precision by bisection with Sturm root counting.
- **Root comparison:** to determine the ordering of two algebraic numbers,
  refine their isolating intervals until they are disjoint.

### 7.3 Arithmetic via Resultants

Arithmetic on algebraic numbers uses **resultant-based composed
polynomials** [Strzebonski 1997].

Given $\alpha$ with minimal polynomial $p(x)$ and $\beta$ with minimal
polynomial $q(x)$:

**Composed sum.** An annihilating polynomial for $\alpha + \beta$ is:

$$
\mathrm{ComposedSum}(p, q)(x) = \mathrm{Res}_y\bigl(p(x - y),\, q(y)\bigr).
$$

This polynomial has degree $\deg(p) \cdot \deg(q)$ and has $\alpha + \beta$
as a root (along with all sums $\alpha_i + \beta_j$ of roots of $p$ and
$q$).

**Composed product.** An annihilating polynomial for $\alpha \cdot \beta$
is:

$$
\mathrm{ComposedProduct}(p, q)(x) = \mathrm{Res}_y\bigl(y^{\deg p} \cdot p(x/y),\, q(y)\bigr).
$$

**Negation.** If $p(x)$ is the minimal polynomial of $\alpha$, then
$(-1)^{\deg p}\, p(-x)$ (made monic) annihilates $-\alpha$.

**Inversion.** If $p(x)$ annihilates $\alpha$, then $x^{\deg p} \cdot p(1/x)$ (the *reciprocal polynomial*, made monic) annihilates $1/\alpha$.

After computing the annihilating polynomial, the library:

1. Factors it into irreducible pieces over $\mathbb{Q}$.
2. Evaluates each factor at the numerical approximation of the result.
3. Selects the factor that has the result as a root (the one evaluating
   closest to zero).
4. Isolates the correct root within that factor using Sturm's theorem.

This gives `AlgNum` its `Num` and `Fractional` instances.

### 7.4 Composed Polynomials: Explicit Construction

The composed sum polynomial can be computed via the **resultant**:

$$
\mathrm{Res}_y\bigl(p(x - y),\, q(y)\bigr) = \prod_{i,j} (x - \alpha_i - \beta_j),
$$

where $\alpha_i$ are roots of $p$ and $\beta_j$ are roots of $q$. The
resultant is computed as the determinant of the Sylvester matrix, which
avoids computing the roots explicitly.

For the composed product:

$$
\mathrm{Res}_y\left(y^n\, p\left(\frac{x}{y}\right),\, q(y)\right)
  = \prod_{i,j} (x - \alpha_i \beta_j).
$$

**Degree.** The composed polynomial has degree $\deg(p) \cdot \deg(q)$,
which can grow rapidly under repeated arithmetic. This is the main
computational bottleneck for AlgNum arithmetic: adding two algebraic
numbers of degrees $m$ and $n$ produces a candidate of degree $mn$, which
must then be factored to find the true minimal polynomial.

### 7.5 Root Isolation

The library implements real root isolation in `Surd.Algebraic.RootIsolation`
using Sturm sequences.

**Algorithm:**

1. Compute a root bound $B$ such that all real roots of $f$ lie in
   $(-B, B)$. The Cauchy bound $B = 1 + \max_i |a_i / a_n|$ suffices,
   where $a_n$ is the leading coefficient and $a_i$ are the others.

2. Count the number of real roots in $(-B, B)$ using Sturm's theorem.

3. Bisect recursively: split $(-B, B)$ into $(-B, 0)$ and $(0, B)$,
   count roots in each half, and recurse until each sub-interval
   contains exactly one root.

4. Return a list of `IsolatingInterval` values, each containing exactly
   one root.

**Refinement.** Given an isolating interval $[a, b]$ for a root of $f$,
it can be narrowed to width $< \epsilon$ by:

1. Let $m = (a + b)/2$.
2. Count roots of $f$ in $[a, m]$ and $[m, b]$ using Sturm.
3. Replace the interval with whichever half contains the root.
4. Repeat until the width is below $\epsilon$.

### 7.6 Conversion Between RadExpr and AlgNum

The **forward direction** (RadExpr $\to$ AlgNum) computes the minimal
polynomial of a radical expression. Three approaches are tried in order:

1. **Tower-based** (`minimalPolyTower`): build a tower of field extensions
   $\mathbb{Q} \subset \mathbb{Q}(\alpha_1) \subset \cdots \subset \mathbb{Q}(\alpha_1, \ldots, \alpha_r)$ corresponding to the radicals
   in the expression, then compute the minimal polynomial of the
   expression element within this tower. Fast for expressions with
   few radicals ($\leq 6$).

2. **Resultant-based** (`minimalPoly`): compute an annihilating polynomial
   using iterated resultants (one per radical), then factor to find the
   minimal polynomial. Always correct but can produce large intermediate
   polynomials.

3. **PSLQ fallback** (`annihilatingPoly`): for expressions with many
   radicals ($> 6$), use numerical evaluation and the PSLQ algorithm
   (Section 13) to find an integer relation, yielding a candidate
   annihilating polynomial.

The **backward direction** (AlgNum $\to$ RadExpr) applies closed-form
solving based on the degree of the minimal polynomial:

- **Degree 1:** $\alpha = -a_0/a_1$ (rational).
- **Degree 2:** Quadratic formula.
- **Degree 3:** Cardano's formula, with casus irreducibilis handling.
- **Degree 4:** Ferrari's method, reducing to a resolvent cubic.
- **Degree 5:** Galois group identification and Lagrange resolvent tower
  (Section 10), if the Galois group is solvable.

### 7.7 The Simplification Round-Trip

The composition RadExpr $\xrightarrow{\text{radExprToAlgNum}}$ AlgNum
$\xrightarrow{\text{algNumToRadExpr}}$ RadExpr is the most powerful
simplification available. It collapses arbitrarily complex radical
expressions to the canonical form determined by the degree of the
minimal polynomial.

**Example.** The Gauss period expression for $\cos(2\pi/7)$ involves 5
nested radicals. The round-trip:

1. Computes the minimal polynomial $8x^3 + 4x^2 - 4x - 1 = 0$
   (degree 3).
2. Applies Cardano's formula to get a 3-radical expression.

The output has 3 radicals instead of 5 and is in a canonical Cardano form.

**Limitations.** The round-trip fails (returns `Nothing` from
`algNumToRadExpr`) when:
- The minimal polynomial has degree $> 5$.
- The degree is 5 but the Galois group is $A_5$ or $S_5$ (not solvable).
- The Cardano/Ferrari formula produces expressions requiring
  simplification of complex intermediates that the library cannot handle.

---

## 8. Field Extensions

### 8.1 Simple Algebraic Extensions

A **simple algebraic extension** $K(\alpha)$ is formed by adjoining a
root $\alpha$ of an irreducible polynomial $m(x) \in K[x]$ to the field
$K$. Algebraically:

$$
K(\alpha) \cong K[x] / (m(x)).
$$

Every element of $K(\alpha)$ can be uniquely represented as a polynomial
in $\alpha$ of degree $< \deg(m)$:

$$
a_0 + a_1 \alpha + a_2 \alpha^2 + \cdots + a_{d-1} \alpha^{d-1},
\qquad a_i \in K.
$$

**Arithmetic** is polynomial arithmetic modulo $m(x)$:
- Addition: coefficient-wise.
- Multiplication: polynomial multiplication followed by reduction modulo
  $m(x)$.
- Inversion: via the **extended Euclidean algorithm** (Section 8.2).

The library implements this in `Math.Field.Extension` with the `ExtElem`
type, which carries both the element (as a polynomial) and the minimal
polynomial of the extension.

### 8.2 Extended Euclidean Algorithm for Inverses

To compute $1/f(\alpha)$ in $K(\alpha) = K[x]/(m(x))$, we use the
extended GCD:

$$
\gcd(f(x), m(x)) = 1 \quad\Longrightarrow\quad
s(x)\, f(x) + t(x)\, m(x) = 1.
$$

Since $m(\alpha) = 0$, evaluating at $\alpha$ gives $s(\alpha)\, f(\alpha) = 1$, so $f(\alpha)^{-1} = s(\alpha)$.

The extended Euclidean algorithm computes $s(x)$ and $t(x)$ via the
standard recurrence. The GCD is guaranteed to be 1 because $m(x)$ is
irreducible and $f(x)$ has degree $< \deg(m)$, so they share no common
factor.

### 8.3 Tower Construction

For multi-step algebraic extensions, the library supports towers:

$$
\mathbb{Q} \subset \mathbb{Q}(\alpha) \subset \mathbb{Q}(\alpha)(\beta)
\subset \cdots
$$

Each level is a simple extension of the previous one. The `ExtElem` type
is nested: an element of $\mathbb{Q}(\alpha)(\beta)$ is an `ExtElem`
whose coefficients are themselves `ExtElem` values living in $\mathbb{Q}(\alpha)$.

The `Num` and `Fractional` instances on `ExtElem` enable seamless
arithmetic at any tower depth (up to depth 5 in practice), with
automatic reduction at each level.

**Sentinel fields.** The top level of the tower uses a *sentinel* minimal
polynomial (degree 0) to indicate that the coefficients are from the
base field $\mathbb{Q}$. The `reduce` function skips polynomial division
for sentinel fields.

### 8.4 Dynamic Towers

The `Surd.Field.DynTower` module provides a dynamic (runtime-configured)
alternative to the statically-nested `ExtElem` approach. A dynamic tower
stores:

- A list of minimal polynomials $[m_1, m_2, \ldots, m_d]$, one per
  level.
- Elements as nested coefficient lists.

Dynamic towers are useful when the number of extension levels is not known
at compile time, which is the typical case for Landau denesting (where the
number of radicals in the expression determines the tower depth).

### 8.5 Applications

Field extensions are used throughout the library:

1. **Trager factoring** (Section 4.5): factors $f(x) \in K(\alpha)[x]$ by
   computing norms via resultants and factoring over $K$.

2. **Minimal polynomial computation** (Section 7.6): the tower-based
   approach builds $\mathbb{Q}(\alpha_1)(\alpha_2)\cdots$ and represents
   the expression as an element of the tower, then computes its minimal
   polynomial over $\mathbb{Q}$.

3. **Landau denesting** (Section 4.4): factors $x^n - a$ over the field
   extension generated by the radicals in $a$.

4. **Gauss period descent** (Section 5.4): at each step, the period
   equation is solved over the field generated by the parent periods.

---

## 9. Polynomial Operations

### 9.1 Univariate Polynomials

The library uses a **sparse representation** for univariate polynomials:
a polynomial $a_0 + a_1 x + \cdots + a_n x^n$ is stored as a list of
$(i, a_i)$ pairs for nonzero coefficients, sorted by degree. This is
efficient for polynomials with many zero coefficients (common in
cyclotomic and resultant computations).

Standard operations include:
- Addition, subtraction: $O(\min(n,m))$ merges.
- Multiplication: $O(nm)$ for polynomials of degrees $n$ and $m$.
- Polynomial division with remainder: $O(nm)$.
- Composition $f(g(x))$: $O(n \cdot m^2)$ via Horner's method.
- Evaluation at a point: $O(n)$ via Horner's method.

### 9.2 GCD via Subresultant

The **polynomial GCD** is computed using the subresultant algorithm, which
avoids the coefficient explosion that plagues the naive Euclidean
algorithm over $\mathbb{Q}$.

**The problem.** When computing $\gcd(f, g)$ for polynomials in
$\mathbb{Q}[x]$ via the Euclidean algorithm, the intermediate remainders
can have coefficients with exponentially many digits (the *intermediate
expression swell*).

**Subresultant algorithm.** The subresultant PRS (polynomial remainder
sequence) controls coefficient growth by dividing each remainder by a
known factor:

1. Set $r_0 = f$, $r_1 = g$, $s_0 = 1$, $h_0 = 1$.
2. For $i \geq 1$: compute $r_{i+1} = \mathrm{prem}(r_{i-1}, r_i) / (s_i \cdot h_i^{d_i})$, where $\mathrm{prem}$ is the
   pseudo-remainder, $d_i = \deg(r_{i-1}) - \deg(r_i)$, and $s_i, h_i$
   are recursively defined scaling factors.
3. The last nonzero $r_i$ (made monic) is the GCD.

The subresultant algorithm ensures that all intermediate coefficients
remain integers (when starting with integer-coefficient polynomials),
avoiding rational arithmetic entirely.

### 9.3 Square-Free Factorisation

A polynomial $f$ is **square-free** if it has no repeated roots (over the
algebraic closure). The square-free part of $f$ is $f / \gcd(f, f')$.

**Yun's algorithm** computes the complete square-free factorisation
$f = \prod_i f_i^i$ where each $f_i$ is square-free and coprime to all
others:

1. Set $a = \gcd(f, f')$, $b = f / a$, $c = f' / a - b'$.
2. At each step: $d = \gcd(b, c)$, output $d$ as the next square-free
   factor, update $b = b/d$, $c = c/d - b'$.
3. Repeat until $b = 1$.

Square-free factorisation is a prerequisite for Trager factoring (which
requires square-free input) and for root isolation (which assumes simple
roots).

### 9.4 Factoring over $\mathbb{Q}$ (Kronecker's Method)

The library factors polynomials over $\mathbb{Q}$ using a method based on
Kronecker's approach [Kronecker 1882]: a degree-$d$ factor of a polynomial
$f$ of degree $n$ is determined by its values at $d + 1$ points.

**Algorithm:**

1. Choose $d + 1$ evaluation points $x_0, \ldots, x_d$.
2. Evaluate $f(x_i)$ at each point.
3. For each way to choose divisors $y_i \mid f(x_i)$, interpolate a
   candidate factor $g$ with $g(x_i) = y_i$.
4. Test whether $g \mid f$.

This is practical for small degrees but exponential in general. The
library also uses the rational root test (checking $\pm p/q$ for $p$
dividing $a_0$ and $q$ dividing $a_n$) as a fast preliminary filter.

### 9.5 Trager Factoring over Extensions

Trager's algorithm factors a square-free polynomial over an algebraic
extension $K(\alpha)$, as described in Section 4.5. The implementation
supports:

- `factorSFOverExtension`: factoring over $\mathbb{Q}(\alpha)$.
- `factorSFOverExtensionK`: factoring over $K(\alpha)$ where $K$ is
  itself an extension, enabling factoring over multi-level towers.

### 9.6 Cyclotomic Polynomials

The $n$-th **cyclotomic polynomial** is:

$$
\Phi_n(x) = \prod_{\substack{1 \leq k \leq n \\ \gcd(k, n) = 1}}
  (x - \zeta_n^k),
$$

where $\zeta_n = e^{2\pi i/n}$. It can be computed without knowing the
roots, using the Moebius inversion formula:

$$
\Phi_n(x) = \prod_{d \mid n} (x^d - 1)^{\mu(n/d)},
$$

where $\mu$ is the Moebius function. The library computes cyclotomic
polynomials using this formula, with the products and quotients performed
as exact polynomial divisions over $\mathbb{Z}$.

**Key properties used by the library:**
- $\Phi_n$ is the minimal polynomial of $\zeta_n$ over $\mathbb{Q}$.
- $\deg(\Phi_n) = \varphi(n)$.
- $\Phi_n$ is irreducible over $\mathbb{Q}$ (a non-trivial theorem).

### 9.7 Resultants

The **resultant** of two polynomials $f(x)$ and $g(x)$ is:

$$
\mathrm{Res}(f, g) = \prod_{f(\alpha) = 0} g(\alpha)
  = (-1)^{mn} \prod_{g(\beta) = 0} f(\beta),
$$

where $m = \deg f$ and $n = \deg g$. It equals zero if and only if $f$
and $g$ share a common root.

The resultant is computed as the determinant of the **Sylvester matrix**,
an $(m+n) \times (m+n)$ matrix whose rows are the coefficients of $f$ and
$g$ shifted by successive powers of $x$:

$$
\mathrm{Syl}(f, g) = \begin{pmatrix}
a_m & a_{m-1} & \cdots & a_0 & 0 & \cdots & 0 \\
0 & a_m & a_{m-1} & \cdots & a_0 & \cdots & 0 \\
\vdots & & \ddots & & & \ddots & \vdots \\
b_n & b_{n-1} & \cdots & b_0 & 0 & \cdots & 0 \\
0 & b_n & b_{n-1} & \cdots & b_0 & \cdots & 0 \\
\vdots & & \ddots & & & \ddots & \vdots
\end{pmatrix}.
$$

The library uses resultants in:
- Trager factoring (computing norms).
- Composed sum/product polynomials for AlgNum arithmetic.
- Minimal polynomial computation.

### 9.8 Multivariate Polynomial GCD

For multivariate polynomials (needed for transcendental extensions), the
library implements GCD via the **recursive subresultant** algorithm. A
multivariate polynomial in $\mathbb{Q}[x_1, \ldots, x_n]$ is viewed as a
univariate polynomial in $x_1$ with coefficients in
$\mathbb{Q}[x_2, \ldots, x_n]$, and the subresultant algorithm is applied
recursively.

The `reduceFrac` function uses multivariate GCD to reduce rational
functions $p(x_1, \ldots, x_n) / q(x_1, \ldots, x_n)$ to lowest terms.

---

## 10. Galois Theory and Solvable Quintics

### 10.1 The Galois Correspondence

For a polynomial $f(x) \in \mathbb{Q}[x]$, the **Galois group**
$\mathrm{Gal}(f) = \mathrm{Gal}(L/\mathbb{Q})$ is the group
of field automorphisms of the splitting field $L$ of $f$ that fix
$\mathbb{Q}$. The Galois group acts faithfully on the roots of $f$,
giving an embedding
$\mathrm{Gal}(f) \hookrightarrow S_n$ (the symmetric group on $n$
letters).

The **Fundamental Theorem of Galois Theory** establishes a bijection
between:

- Subgroups $H \leq \mathrm{Gal}(L/\mathbb{Q})$, and
- Intermediate fields $\mathbb{Q} \subseteq K \subseteq L$.

Under this bijection:
- $H$ corresponds to the fixed field $L^H = \{x \in L : \sigma(x) = x \text{ for all } \sigma \in H\}$.
- $K$ corresponds to the stabiliser
  $\mathrm{Gal}(L/K) = \{\sigma : \sigma|_K = \text{id}\}$.
- Normal subgroups $H \trianglelefteq G$ correspond to Galois
  (normal) extensions $K/\mathbb{Q}$.
- The degree $[L^H : \mathbb{Q}] = [G : H]$ (index of $H$ in $G$).

### 10.2 Solvability by Radicals

**Theorem** (Galois).
A polynomial $f(x) \in \mathbb{Q}[x]$ is solvable
by radicals---its roots can be expressed using $+, -, \times, \div, \sqrt[n]{\cdot}$ applied to rational numbers---if and only if its
Galois group $\mathrm{Gal}(f)$ is a **solvable group**.

A finite group $G$ is **solvable** if it admits a **composition series**:

$$
G = G_0 \trianglerighteq G_1 \trianglerighteq \cdots
  \trianglerighteq G_k = \{1\},
$$

where each quotient $G_i / G_{i+1}$ is cyclic of prime order.

**Connection to radical towers.** Each cyclic quotient of order $p$
corresponds to adjoining a $p$-th root. The composition series determines
the structure of the radical tower needed to express the roots:

$$
\mathbb{Q} \subset \mathbb{Q}(\omega, \sqrt[p_1]{\cdot})
  \subset \mathbb{Q}(\omega, \sqrt[p_1]{\cdot}, \sqrt[p_2]{\cdot})
  \subset \cdots
$$

where $\omega$ denotes appropriate roots of unity needed at each stage.
The requirement for roots of unity is why the casus irreducibilis
cannot be avoided: expressing real roots of an irreducible cubic with
three real roots requires complex cube roots of unity.

### 10.3 Transitive Subgroups of $S_5$

For degree 5, the transitive subgroups of $S_5$ (up to conjugacy) are
classified by the Butler--McKay numbering [Butler and McKay 1983]:

| Group   | Butler--McKay | Order | Solvable | Structure |
|:--------|:------------:|------:|:--------:|:----------|
| $C_5$   | $T_1$        | 5     | Yes      | Cyclic, $\mathbb{Z}/5\mathbb{Z}$ |
| $D_5$   | $T_2$        | 10    | Yes      | Dihedral, $\mathbb{Z}/5\mathbb{Z} \rtimes \mathbb{Z}/2\mathbb{Z}$ |
| $F_{20}$| $T_3$        | 20    | Yes      | Frobenius, $\mathbb{Z}/5\mathbb{Z} \rtimes \mathbb{Z}/4\mathbb{Z}$ |
| $A_5$   | $T_4$        | 60    | No       | Alternating group |
| $S_5$   | $T_5$        | 120   | No       | Symmetric group |

The subgroup lattice is:

$$
S_5 \supset A_5, \qquad S_5 \supset F_{20} \supset D_5 \supset C_5, \qquad A_5 \supset D_5.
$$

The **Abel--Ruffini theorem** (1824) states that the general quintic
cannot be solved by radicals. More precisely: $A_5$ is simple (has no
proper normal subgroups), so it is not solvable, and a generic quintic has
Galois group $S_5$, which contains $A_5$ as a normal subgroup with
quotient $C_2$. Since $A_5$ is not solvable, neither is $S_5$.

However, specific quintics with Galois groups $C_5$, $D_5$, or $F_{20}$
**can** be solved by radicals.

### 10.4 Galois Group Identification via Stauduhar Descent

The library identifies the Galois group of an irreducible degree-5
polynomial using the **Stauduhar descent** method [Stauduhar 1973].

**Resolvent 1: Discriminant.** Compute the discriminant

$$
\Delta(f) = \prod_{i < j} (\alpha_i - \alpha_j)^2.
$$

If $\Delta$ is a perfect square in $\mathbb{Q}$, then
$\mathrm{Gal}(f) \leq A_5$ (even permutations only).

**Resolvent 2: Sextic resolvent.** The $F_{20}$-invariant

$$
\theta(x_0, \ldots, x_4) = \sum_{i=0}^{4}
  x_i^2 (x_{i+1} x_{i+4} + x_{i+2} x_{i+3})
$$

(indices mod 5) has stabiliser $F_{20}$ in $S_5$. Its orbit has
$|S_5|/|F_{20}| = 120/20 = 6$ elements, giving a degree-6 resolvent
$R_\theta(x) \in \mathbb{Q}[x]$. If $R_\theta$ has a rational root,
then $\mathrm{Gal}(f) \leq F_{20}$.

**Decision table:**

| $\Delta$ perfect square? | $R_\theta$ has rational root? | Galois group |
|:------------------------:|:-----------------------------:|:------------:|
| No                       | No                            | $S_5$        |
| Yes                      | No                            | $A_5$        |
| No                       | Yes                           | $F_{20}$     |
| Yes                      | Yes                           | $D_5$ or $C_5$ |

**Frobenius/Chebotarev test.** When both tests place the group inside
$A_5 \cap F_{20} = D_5$, a Frobenius test distinguishes $C_5$ from $D_5$.
The Frobenius element at an unramified prime $p$ determines the
factorisation pattern of $f \pmod{p}$:

- $C_5$: only patterns $\{5\}$ (inert) or $\{1,1,1,1,1\}$ (split).
- $D_5$: additionally admits $\{1,2,2\}$ (one fixed point, two 2-cycles).

Testing 20 good primes (primes not dividing the discriminant) suffices
in practice.

### 10.5 Lagrange Resolvent Towers for Quintics

Once the Galois group is identified as solvable, the library constructs
radical expressions for the roots via Lagrange resolvents
[Dummit 1991].

**Algorithm** (for degree 5 with solvable Galois group $G$):

1. **Depress.** Shift the variable to eliminate the $x^4$ term:
   $f(x) \to f(x - a_4/(5a_5))$.

2. **Find cyclic ordering.** Compute the roots $\alpha_0, \ldots, \alpha_4$ numerically and find a permutation such that the Galois
   generator acts as the cyclic shift $\sigma(\alpha_k) = \alpha_{k+1 \bmod 5}$.

3. **Compute Lagrange resolvents.** With $\omega = e^{2\pi i/5}$:

   $$R_j = \sum_{k=0}^{4} \omega^{jk} \alpha_k, \qquad j = 0, 1, \ldots, 4.$$

   Then $R_j^5$ is fixed by $\sigma$ (since $\sigma(R_j) = \omega^{-j} R_j$),
   so $R_j^5$ lies in the base field.

4. **DFT coefficient recovery.** Express each $R_j^5$ as a known element
   of the base field. Write:

   $$R_j^5 = \sum_{s=0}^{4} d_s\, \omega^{js},$$

   and recover the $d_s$ from numerical evaluation.

5. **Branch selection.** For each $R_j$, determine the correct 5th root
   branch by comparing $\omega^{b_j} \sqrt[5]{R_j^5}$ against the
   numerical value of $R_j$ for each $b_j \in \{0, 1, 2, 3, 4\}$.

6. **Root recovery by inverse DFT:**

   $$\alpha_k = \frac{1}{5} \sum_{j=0}^{4} \omega^{-jk} R_j.$$

7. **Undo depression.** Shift back by $a_4/(5a_5)$.

### 10.6 Cardano's Formula (Degree 3)

For a depressed cubic $x^3 + px + q = 0$, Cardano's formula gives:

$$
x = \sqrt[3]{-\frac{q}{2} + \sqrt{\frac{q^2}{4} + \frac{p^3}{27}}} + \sqrt[3]{-\frac{q}{2} - \sqrt{\frac{q^2}{4} + \frac{p^3}{27}}}.
$$

**Critical implementation detail.** The two cube roots are NOT
independent: if $u_1 = \sqrt[3]{-q/2 + \sqrt{D}}$, then $u_2$ must be
$-p/(3u_1)$, NOT an independently computed cube root of $-q/2 - \sqrt{D}$.
Choosing an independent cube root gives a wrong answer in general: the
three possible cube roots of $u_2^3$ correspond to the three roots of the
cubic, but only one pairs correctly with the chosen branch of $u_1$.

**Casus irreducibilis.** When the discriminant $D = q^2/4 + p^3/27 < 0$
(equivalently, three distinct real roots), the formula involves
$\sqrt{D}$, which is imaginary. The cube roots of complex numbers are
required, and the final sum is real.

**Worked example: $x^3 - 3x - 1 = 0$.**

Here $p = -3$, $q = -1$. The discriminant is:

$$
D = \frac{1}{4} + \frac{-27}{27} = \frac{1}{4} - 1 = -\frac{3}{4}.
$$

Since $D < 0$, this is the casus irreducibilis. Cardano gives:

$$
x = \sqrt[3]{\frac{1}{2} + \frac{\sqrt{3}}{2}\,i} + \sqrt[3]{\frac{1}{2} - \frac{\sqrt{3}}{2}\,i} = \sqrt[3]{e^{i\pi/3}} + \sqrt[3]{e^{-i\pi/3}}
= e^{i\pi/9} + e^{-i\pi/9} = 2\cos(\pi/9).
$$

The other two roots are $2\cos(7\pi/9)$ and $2\cos(13\pi/9)$.

### 10.7 Ferrari's Method (Degree 4)

Ferrari's method reduces a depressed quartic $x^4 + bx^2 + cx + d = 0$
to a resolvent cubic plus quadratic factors.

1. Introduce an auxiliary variable $y$ and write:

   $$x^4 + bx^2 + cx + d = (x^2 + y)^2 - \bigl[(2y - b)x^2 - cx + (y^2 - d)\bigr].$$

2. The bracketed expression is a perfect square in $x$ when its
   discriminant (as a quadratic in $x$) vanishes:

   $$c^2 - 4(2y - b)(y^2 - d) = 0.$$

   This is the **resolvent cubic** $8y^3 - 4by^2 - 8dy + (4bd - c^2) = 0$.

3. Choose any root $y_0$ of the resolvent cubic. Then the quartic factors
   into two quadratics:

   $$x^4 + bx^2 + cx + d = \left(x^2 + y_0 - \sqrt{2y_0 - b}\cdot x\right) \cdot \left(x^2 + y_0 + \sqrt{2y_0 - b}\cdot x\right) + \text{linear corrections},$$

   and each quadratic is solved by the quadratic formula.

The library implements Ferrari's method in `Surd.Algebraic.Convert`,
using Cardano's formula for the resolvent cubic.

---

## 11. Symbolic Integration

### 11.1 Euler Substitution

**Euler substitution** reduces integrals of the form

$$
\int \frac{P(x)}{Q(x)} \cdot \bigl(\sqrt{ax^2 + bx + c}\bigr)^n \, dx
$$

to rational function integrals, which can then be evaluated by partial
fractions and Hermite reduction.

Three substitutions cover all cases where $ax^2 + bx + c$ has a real
square root:

**Euler Substitution I** ($a > 0$): Set $\sqrt{ax^2 + bx + c} = t - x\sqrt{a}$.

Squaring: $ax^2 + bx + c = t^2 - 2tx\sqrt{a} + ax^2$, so $bx + c = t^2 - 2tx\sqrt{a}$, giving:

$$
x = \frac{t^2 - c}{2t\sqrt{a} + b}, \qquad
dx = \frac{2t(2t\sqrt{a} + b) - 2\sqrt{a}(t^2 - c)}{(2t\sqrt{a} + b)^2}\,dt.
$$

The integrand becomes a rational function of $t$.

**Euler Substitution II** ($c > 0$): Set $\sqrt{ax^2 + bx + c} = xt + \sqrt{c}$.

Squaring and solving for $x$:

$$
x = \frac{2t\sqrt{c} - b}{a - t^2}.
$$

**Euler Substitution III** ($\Delta = b^2 - 4ac > 0$): When the quadratic
has real roots $r_1, r_2$, set $\sqrt{a(x - r_1)(x - r_2)} = t(x - r_1)$.

Squaring: $a(x - r_2) = t^2(x - r_1)$, giving:

$$
x = \frac{ar_2 - t^2 r_1}{a - t^2}.
$$

### 11.2 Rational Function Integration

After Euler substitution, the integrand is a rational function $R(t) = P(t)/Q(t)$. The library evaluates $\int R(t)\,dt$ via:

1. **Polynomial division.** Write $R(t) = S(t) + P'(t)/Q(t)$ where
   $\deg P' < \deg Q$. Integrate $S(t)$ directly.

2. **Square-free factorisation** of $Q(t)$.

3. **Hermite reduction.** For repeated factors $Q_i^k$ with $k > 1$,
   reduce the multiplicity by computing:

   $$\frac{P(t)}{Q_i(t)^k} = \frac{d}{dt}\left(\frac{A(t)}{Q_i(t)^{k-1}}\right) + \frac{B(t)}{Q_i(t)^{k-1}},$$

   where $A$ and $B$ are found by the extended Euclidean algorithm applied
to $Q_i$ and $Q_i'$.

4. **Partial fraction decomposition** of the remaining simple-fraction
   part.

5. **Integration of simple fractions:**
   - $\displaystyle\int \frac{dt}{t - a} = \ln|t - a|$
   - $\displaystyle\int \frac{At + B}{t^2 + pt + q}\,dt$ (irreducible
     quadratic denominator): split into
     $\frac{A}{2}\ln(t^2 + pt + q)$ plus an $\arctan$ term.

The result is a `SymExpr` containing rational constants, radical
constants, logarithms, and inverse trigonometric functions.

### 11.3 Elliptic Integral Reduction

For integrals involving $\sqrt{P(x)}$ where $P(x)$ has degree 3 or 4
(elliptic integrals), the library reduces to **Legendre normal forms**.

The three standard forms are:

$$
F(\varphi, k) = \int_0^{\sin\varphi} \frac{dt}{\sqrt{(1-t^2)(1-k^2 t^2)}},
$$

$$
E(\varphi, k) = \int_0^{\sin\varphi} \sqrt{\frac{1 - k^2 t^2}{1 - t^2}}\,dt,
$$

$$
\Pi(\varphi, n, k) = \int_0^{\sin\varphi} \frac{dt}{(1 - nt^2)\sqrt{(1-t^2)(1-k^2 t^2)}}.
$$

**Reduction algorithm:**

1. **Root computation.** Factor $P(x)$ over $\mathbb{Q}$ and compute its
   real roots as algebraic numbers. All roots must be real for the
   standard reduction to apply.

2. **Ordering.** Sort the roots $r_1 < r_2 < r_3 < r_4$ (degree 4) or
   $r_1 < r_2 < r_3$ (degree 3).

3. **Birational transformation.** A Moebius transformation maps the roots
   to Legendre form. The **elliptic modulus** $k$ is determined by the
   **cross-ratio** of the roots:

   $$k^2 = \frac{(r_2 - r_1)(r_4 - r_3)}{(r_3 - r_1)(r_4 - r_2)}.$$

4. **Exact modulus computation.** The cross-ratio involves algebraic
   numbers, and the library computes $k$ as an exact radical expression
   via `algNumToRadExpr`.

5. **Amplitude.** The Legendre amplitude is expressed symbolically as:

   $$\varphi = \arcsin\sqrt{\frac{x - r_1}{r_2 - r_1}}.$$

The output is a list of `LegendreForm` terms with exact radical
coefficients and moduli. Results can optionally be expressed using inverse
Jacobi elliptic functions ($\text{sn}^{-1}$, $\text{cn}^{-1}$,
$\text{dn}^{-1}$).

---

## 12. Arbitrary Precision Evaluation

### 12.1 Ball Arithmetic

The library uses **ball arithmetic** from the aern2-mp package for
arbitrary-precision evaluation. A **ball** $B = (c, r)$ represents the
set $\{x : |x - c| \leq r\}$, where $c$ (the centre) is an
arbitrary-precision floating-point number (`MPFloat`) and $r$ (the radius)
is a non-negative error bound.

**Key properties:**

- Arithmetic operations on balls produce balls that are guaranteed to
  contain the true result. For example, if $a \in B_1$ and $b \in B_2$,
  then $a + b \in B_1 + B_2$.
- The precision (number of bits in the mantissa of $c$) is configurable.
- Ball arithmetic propagates error bounds automatically, giving rigorous
  error control without manual interval tracking.

The library uses `MPBall` from aern2-mp, creating balls via
`mpBallP (prec n) r` where `n` is the number of precision bits and `r`
is a rational value.

### 12.2 Complex Ball Arithmetic

Complex numbers are represented as pairs of `MPBall`s:

$$
z = (x_{\text{re}},\, x_{\text{im}}), \qquad
x_{\text{re}}, x_{\text{im}} \in \text{MPBall}.
$$

This is the `ComplexMP` type in `Surd.Radical.EvalMP`. Arithmetic follows
the standard formulas:

$$
(a, b) + (c, d) = (a + c,\, b + d),
$$
$$
(a, b) \cdot (c, d) = (ac - bd,\, ad + bc).
$$

Division uses component-wise scaling to avoid overflow/underflow issues
that arise from Haskell's `Complex` division, which calls
`scaleFloat`/`exponent`---methods that crash on `MPBall` types.

### 12.3 Newton's Method for Complex Nth Roots

Computing the $n$-th root of a complex ball $z$ is not provided natively
by aern2-mp (which has `sqrt` but not general $n$-th roots). The library
uses **Newton's method**:

$$
w_{k+1} = \frac{(n-1)\, w_k + z / w_k^{n-1}}{n},
$$

starting from a `Complex Double` estimate computed from the MPBall
midpoint.

**Convergence.** Newton's method for $w^n = z$ converges quadratically
(the number of correct digits doubles with each iteration). Starting from
a Double estimate (roughly 15 significant digits), 20 iterations give
$15 \times 2^{20} \approx 10^7$ digits of accuracy---far more than any
practical precision requirement.

**Starting point subtlety.** The initial estimate must use the MPBall
midpoint (`complexMPToDouble z`), NOT a separately computed Double
evaluation chain. For near-zero $R_j^q$ values in the Gauss period
descent, Double's phase is garbage (the mantissa has only 15 digits, but
the value may be $\sim 10^{-30}$), causing Newton to converge to the
wrong root.

### 12.4 Why Atan Fails

The aern2-mp package does not implement `atan` (the `Floating` instance
stubs it with `error`). This means the polar form of complex $n$-th roots:

$$
\sqrt[n]{r\, e^{i\theta}} = r^{1/n}\, e^{i\theta/n}
$$

cannot be used directly, since computing $\theta = \mathrm{atan2}(y, x)$
from the Cartesian form is not available in MPBall. Newton's method
provides a purely algebraic alternative that avoids all trigonometric
functions.

### 12.5 Precision Requirements for Coefficient Matching

In the DFT coefficient matching step of the Gauss period descent
(Section 5.6), the library must distinguish between exact rational
values that differ by small amounts. The required precision depends on:

- **The prime $p$:** larger primes produce larger $d_s$ values (the DFT
  coefficients). For $p = 89$ with $q = 11$, the $d_s$ values reach
  approximately $10^8$.

- **The number of periods:** with $k$ periods, the $d_s$ may be
  degree-$(k-1)$ polynomials in the period values, requiring more
  precision to match coefficients.

The library uses 500-bit or 1000-bit precision MPBalls for coefficient
matching, converting to `Rational` via continued fractions when the
ball width is sufficiently small ($ < 10^{-30}$). At 500 bits, all tested
angles achieve adequate precision, with evaluation times of 10--60ms.

### 12.6 DAG Evaluation at Arbitrary Precision

The `dagEvalComplexMP` function evaluates a `RadDAG` at arbitrary
precision in a single bottom-up pass. Each node is evaluated once and the
result (a `ComplexMP` pair of `MPBall`s) is stored in an `IntMap`. This
gives $O(n)$ complexity where $n$ is the DAG size.

The `dagEvalRealMP` variant extracts the real part of the result, which
is used for evaluating expressions known to be real (such as
$\cos(p\pi/q)$).

The `dftCoeffsMP` function computes the DFT coefficients $d_s$ at high
precision, used in the Gauss period descent for large primes where
Double precision is insufficient.

---

## 13. PSLQ and Integer Relations

### 13.1 The Integer Relation Problem

Given real numbers $x_1, \ldots, x_n$, the **integer relation problem**
asks whether there exist integers $m_1, \ldots, m_n$, not all zero, such
that:

$$
m_1 x_1 + m_2 x_2 + \cdots + m_n x_n = 0.
$$

This problem arises in the library when computing minimal polynomials
numerically: if $\alpha$ is an algebraic number of degree $d$, then the
vector $(1, \alpha, \alpha^2, \ldots, \alpha^d)$ satisfies an integer
relation given by the coefficients of the minimal polynomial.

### 13.2 The PSLQ Algorithm

**PSLQ** (Partial Sum of Least Squares) [Ferguson and Bailey 1999] is a
numerically stable algorithm for finding integer relations. It is
considered the standard algorithm for this problem and was named one of
the "Top Ten Algorithms of the Century" by Computing in Science and
Engineering.

**Input.** A vector $\mathbf{x} = (x_1, \ldots, x_n) \in \mathbb{R}^n$
with $\|\mathbf{x}\| = 1$ (normalised).

**Output.** An integer vector $\mathbf{m} = (m_1, \ldots, m_n)$ such
that $\mathbf{m} \cdot \mathbf{x} = 0$, or a certificate that no
relation with $\|\mathbf{m}\| \leq M$ exists.

**Algorithm outline:**

1. **Initialise.** Compute the partial sums of squares
   $s_k = \sqrt{\sum_{j=k}^{n} x_j^2}$. Construct the $n \times (n-1)$
   lower trapezoidal matrix $H$ with entries:

$$
H_{ij} = \begin{cases}
  s_{j+1}/s_j & \text{if } i = j, \\
  -x_i x_j / (s_j s_{j+1}) & \text{if } i > j, \\
  0 & \text{if } i < j.
\end{cases}
$$

Set $A = B = I_n$ (identity matrices).

2. **Iterate.** At each step:

   a. Select $j$ maximising $\gamma^j |H_{jj}|$ where
      $\gamma > 2/\sqrt{3}$ is a tuning parameter.

   b. Swap rows $j$ and $j+1$ of $H$, and corresponding rows/columns of
      $A$ and $B$.

   c. Apply a **Givens rotation** to restore upper Hessenberg form of $H$:
      a $2 \times 2$ rotation in the $(j, j+1)$ plane zeroes out the
      subdiagonal element.

   d. Apply **integer reduction** (LLL-style): for each $i$ from $j+1$ to
      $n$, and for each $k$ from $j$ down to 1, subtract
      $\mathrm{nint}(B_{ik}/B_{kk}) \cdot$ row $k$ from row $i$ of
      $B$ (and correspondingly update $A$ and $H$).

   e. **Termination check.** If the minimum of $|H_{jj}|$ is below a
      threshold (related to the input precision), the corresponding row
      of $B$ is a candidate relation. Verify it.

3. **Bound.** If $\min_j |H_{jj}| > \epsilon$, then no integer relation
   with $\|\mathbf{m}\| < 1/(\max_j |H_{jj}|)$ exists.

### 13.3 Numerical Precision Requirements

PSLQ requires high-precision arithmetic. For finding a relation of degree
$d$ with coefficients bounded by $C$, the input must be computed to at
least $d \log_{10} C + 30$ decimal digits of precision.

In the surd library, PSLQ is used as a fallback for minimal polynomial
computation when the tower-based approach cannot handle the expression
(more than 6 radicals). The expression is evaluated to high precision
using `dagEvalComplexMP` at 500+ bits, and powers
$1, \alpha, \alpha^2, \ldots, \alpha^d$ are computed for candidate
degrees $d$.

### 13.4 Givens Rotations

The PSLQ implementation uses **Givens rotations** for the orthogonal
update step. A Givens rotation $G(i, j, \theta)$ is the identity matrix
with four modified entries:

$$
G_{ii} = G_{jj} = \cos\theta, \quad G_{ij} = -\sin\theta, \quad G_{ji} = \sin\theta.
$$

It zeroes out a single off-diagonal element while preserving
orthogonality. In PSLQ, Givens rotations restore the upper Hessenberg
form of $H$ after row swaps.

The library uses the Givens rotation implementation from the linear-massiv
package, which provides type-safe matrices and vectors with efficient
array-backed storage via the massiv library.

### 13.5 Application to Minimal Polynomial Recovery

To find the minimal polynomial of a radical expression $E$:

1. Evaluate $\alpha = E$ to high precision (500+ bits).
2. For candidate degree $d = 1, 2, 3, \ldots$:
   a. Form the vector $\mathbf{x} = (1, \alpha, \alpha^2, \ldots, \alpha^d)$.
   b. Normalise $\mathbf{x}$ and run PSLQ.
   c. If a relation $\mathbf{m}$ is found, the coefficients give a
      polynomial $p(x) = m_0 + m_1 x + \cdots + m_d x^d$ with
      $p(\alpha) = 0$.
   d. Verify the relation at higher precision. If verified, factor $p$
      over $\mathbb{Q}$ and extract the irreducible factor containing
      $\alpha$.
3. Stop at the first successful degree.

This approach always terminates (every radical expression is algebraic)
and produces the correct minimal polynomial, subject to sufficient
numerical precision.

---

## 14. Bibliography

[Besicovitch 1940] A. S. Besicovitch, "On the linear independence of
fractional powers of integers," *Journal of the London Mathematical
Society*, vol. s1-15, no. 1, pp. 3--6, 1940.
doi:[10.1112/jlms/s1-15.1.3](https://doi.org/10.1112/jlms/s1-15.1.3)

[Bloemer 1991] J. Bloemer, "How to denest Ramanujan's nested radicals,"
in *Proceedings of the 33rd Annual Symposium on Foundations of Computer
Science (FOCS)*, pp. 447--456, IEEE, 1992.
doi:[10.1109/SFCS.1992.267807](https://doi.org/10.1109/SFCS.1992.267807)

[Borodin et al. 1985] A. Borodin, R. Fagin, J. E. Hopcroft, and
M. Tompa, "Decreasing the nesting depth of expressions involving square
roots," *Journal of Symbolic Computation*, vol. 1, no. 2, pp. 169--188,
1985.
doi:[10.1016/S0747-7171(85)80013-4](https://doi.org/10.1016/S0747-7171(85)80013-4)

[Caviness and Fateman 1976] B. F. Caviness and R. J. Fateman,
"Simplification of radical expressions," in *Proceedings of the 1976
ACM Symposium on Symbolic and Algebraic Computation (SYMSAC '76)*,
pp. 329--338, ACM, 1976.
doi:[10.1145/800205.806353](https://doi.org/10.1145/800205.806353)

[Cox 2012] D. A. Cox, *Galois Theory*, 2nd ed., Pure and Applied
Mathematics (Hoboken). Wiley, 2012.
doi:[10.1002/9781118218457](https://doi.org/10.1002/9781118218457)

[Dummit 1991] D. S. Dummit, "Solving solvable quintics," *Mathematics
of Computation*, vol. 57, no. 195, pp. 387--401, 1991.
doi:[10.1090/S0025-5718-1991-1079014-X](https://doi.org/10.1090/S0025-5718-1991-1079014-X)

[Ferguson and Bailey 1999] H. R. P. Ferguson and D. H. Bailey, "A
polynomial time, numerically stable integer relation algorithm,"
*Mathematics of Computation*, vol. 68, no. 228, pp. 351--369, 1999.
doi:[10.1090/S0025-5718-99-00995-3](https://doi.org/10.1090/S0025-5718-99-00995-3)

[Gauss 1801] C. F. Gauss, *Disquisitiones Arithmeticae*, Leipzig:
Gerhard Fleischer, 1801. (English translation by A. A. Clarke, Yale
University Press, 1966.)

[Kronecker 1882] L. Kronecker, "Grundzuege einer arithmetischen Theorie
der algebraischen Groessen," *Journal fuer die reine und angewandte
Mathematik*, vol. 92, pp. 1--122, 1882.
doi:[10.1515/crll.1882.92.1](https://doi.org/10.1515/crll.1882.92.1)

[Landau 1992] S. Landau, "Simplification of nested radicals,"
*SIAM Journal on Computing*, vol. 21, no. 1, pp. 85--110, 1992.
doi:[10.1137/0221009](https://doi.org/10.1137/0221009)

[Nosan et al. 2022] A. Nosan, A. Pouly, J. Schmid, and
M. Shirmohammadi, "On the identity testing of radical expressions,"
*arXiv preprint*, arXiv:2202.07961, 2022.

[Stauduhar 1973] R. P. Stauduhar, "The determination of Galois groups,"
*Mathematics of Computation*, vol. 27, no. 124, pp. 981--996, 1973.
doi:[10.1090/S0025-5718-1973-0327712-4](https://doi.org/10.1090/S0025-5718-1973-0327712-4)

[Strzebonski 1997] A. Strzebonski, "Computing in the field of complex
algebraic numbers," *Journal of Symbolic Computation*, vol. 24, no. 6,
pp. 647--656, 1997.
doi:[10.1006/jsco.1997.0156](https://doi.org/10.1006/jsco.1997.0156)

[Trager 1984] B. M. Trager, "Integration of algebraic functions,"
Ph.D. thesis, Department of EECS, Massachusetts Institute of Technology,
1984. Also published as MIT/LCS/TR-316.

[Weber and Keckeisen 2003] A. Weber and M. Keckeisen, "Simplification
of expressions involving radicals," *Journal of Symbolic Computation*,
vol. 36, no. 6, pp. 843--866, 2003.

[Zippel 1985] R. Zippel, "Simplification of expressions involving
radicals," *Journal of Symbolic Computation*, vol. 1, no. 2,
pp. 189--210, 1985.
doi:[10.1016/S0747-7171(85)80014-6](https://doi.org/10.1016/S0747-7171(85)80014-6)
