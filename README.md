# surd -- Exact Symbolic Computation with Radical Expressions

A comprehensive library for simplifying, denesting, and evaluating expressions
involving nested radicals, with exact trigonometric values, algebraic number
arithmetic, and Galois-theoretic polynomial solving.

## Features

- **Radical expression construction, normalization, and simplification** --
  A tree-based AST (`RadExpr k`) parameterised by coefficient type, with
  explicit normalization passes (constant folding, perfect power extraction,
  like-term collection, distribution) iterated to a fixed point.

- **Radical denesting** -- Borodin's algorithm for square roots, nth-root
  denesting for cube roots and beyond, and Landau's algorithm (via Trager
  factoring over algebraic extensions) as a general fallback. Handles
  expressions like sqrt(3 + 2*sqrt(2)) = 1 + sqrt(2).

- **Exact trigonometric values at all rational multiples of pi** --
  Gauss period descent with Lagrange resolvents computes cos(p*pi/q) and
  sin(p*pi/q) as radical expressions for every rational p/q. Supports
  primes, prime powers, and composites via CRT decomposition.

- **Algebraic number arithmetic** -- Minimal polynomial computation
  (resultant-based and tower-based), Sturm-based root isolation, and full
  field arithmetic (add, multiply, divide, nth root) on algebraic numbers.

- **Galois group identification and radical solving** -- Stauduhar descent
  with resolvent polynomials identifies Galois groups of irreducible
  quintics. Polynomials with solvable Galois groups (C5, D5, F20) are
  solved via Lagrange resolvent towers.

- **Symbolic integration with radical coefficients** -- Euler substitution
  for integrals involving sqrt(ax^2+bx+c), and reduction of elliptic
  integrals to Legendre normal forms with exact radical moduli.

- **Multiple output formats** -- Human-readable plain text (with CSE),
  LaTeX math-mode rendering, and DAG-based LaTeX for large expressions.

- **DAG-based evaluation** -- Explicit IntMap-based DAG via StableName
  detection eliminates exponential tree traversal. Constant folding,
  power simplification, and perfect power extraction all run in O(n)
  on unique nodes. cos(2*pi/37): 143s -> 35ms (4000x speedup).

- **Arbitrary-precision ball arithmetic** -- MPBall evaluation (via
  aern2-mp) provides rigorous numerical results at any desired precision.
  Newton's method for complex nth roots avoids atan pitfalls.

- **Normal form canonicalization** -- Q-linear combinations of radical
  monomials with automatic like-term collection, exponent reduction
  (i^2 = -1, (nth-root r)^n = r), and integral radicand canonicalization
  (Besicovitch/Zippel form).

## Quick Example (Haskell)

```haskell
import Surd.Types
import Surd.Radical.Expr
import Surd.Radical.Normalize
import Surd.Radical.Denest
import Surd.Radical.Pretty
import Surd.Trig

-- Exact cos(pi/3) = 1/2
> cosExact 1 3
Radical (Lit (1 % 2))

-- Exact cos(pi/5) as a radical expression
> case cosExact 1 5 of Radical e -> pretty e
"(1/4)(-1 + sqrt(5))"

-- Denesting: sqrt(3 + 2*sqrt(2)) = 1 + sqrt(2)
> pretty (denest (sqrt' (add (lit 3) (mul (lit 2) (sqrt' (lit 2))))))
"1 + sqrt(2)"

-- Normalization: sqrt(2) * sqrt(2) = 2
> pretty (normalize (mul (sqrt' (lit 2)) (sqrt' (lit 2))))
"2"

-- Perfect power extraction: sqrt(12) = 2*sqrt(3)
> pretty (normalize (sqrt' (lit 12)))
"2sqrt(3)"

-- LaTeX output
> latex (normalize (sqrt' (lit 12)))
"2\\sqrt{3}"
```

## Project Structure

```
surd/
├── haskell/                     # Reference implementation
│   ├── algebraic-polynomials/   # Standalone polynomial package (13 modules)
│   │   └── src/Math/            #   Polynomial/, Field/, Internal/
│   ├── src/Surd/                # Main surd library (22 modules)
│   │   ├── Types.hs             #   Core RadExpr AST
│   │   ├── Radical/             #   Normalize, Denest, Eval, DAG, Pretty, LaTeX, ...
│   │   ├── Trig/                #   Galois periods, RootOfUnity, TowerDescent
│   │   ├── Algebraic/           #   AlgNum, Convert, RootIsolation
│   │   ├── Galois/              #   Permutation, Identify, Resolvent, Solve
│   │   ├── Polynomial/          #   MinimalPoly, MinimalPolyTower
│   │   ├── Field/               #   Tower, DynTower
│   │   └── Integration/         #   Euler, Elliptic
│   ├── test/                    # 219+ tests (tasty + HUnit + QuickCheck)
│   └── demo/                    # Executables (TrigTable, SolvableQuintic, ...)
├── mercury/                     # Mercury port (di/uo, DCG style)
├── lean4/                       # Lean 4 port (termination proofs, notation)
├── ocaml/                       # OCaml port (functors, first-class modules)
├── scala3/                      # Scala 3 port (opaque types, effects)
├── idris2/                      # Idris 2 port (dependent types, Fin)
├── koka/                        # Koka port (algebraic effects)
├── fstar/                       # F* port (refinement types, Z3 verification)
├── clean/                       # Clean port (uniqueness types, generics)
├── curry/                       # Curry port (nondeterministic search)
└── docs/                        # Documentation
```

## Language Highlights

Each port exploits its language's defining features:

| Language  | Defining Feature Used                                         | Example                                               |
|-----------|---------------------------------------------------------------|-------------------------------------------------------|
| Haskell   | Parametric polymorphism, lazy evaluation, StableName DAG      | `RadExpr k` parameterised over coefficient field      |
| Mercury   | Determinism modes (di/uo), DCG grammars                       | `normalize(Expr::di, NormExpr::uo) is det`            |
| Lean 4    | Termination proofs, custom notation, tactics                  | Verified termination of normalization fixed point     |
| OCaml     | Functors, first-class modules                                 | `module Radical(K : Field) : RADICAL`                 |
| Scala 3   | Opaque types, context functions, ZIO effects                  | `opaque type RadExpr = ...` with given instances      |
| Idris 2   | Dependent types, `Fin n` for bounded indices                  | Root index `n : Fin` statically prevents zero         |
| Koka      | Algebraic effect handlers for evaluation strategies           | `effect eval { fun nth-root(n, x) : result }`         |
| F*        | Refinement types, Z3-backed SMT verification                  | `val root : n:pos{n >= 2} -> expr -> expr`            |
| Clean     | Uniqueness types, generic deriving                            | Unique arrays for mutable DAG construction            |
| Curry     | Nondeterministic narrowing search                             | Denesting as constraint-guided narrowing              |

## Building

### Haskell (reference implementation)

```bash
cd haskell
cabal build all
cabal test
cabal haddock   # Generate API documentation
```

Demo executables:

```bash
cabal run surd-trig-table        # Trig value tables (text + LaTeX)
cabal run surd-solvable-quintic  # Solve a quintic via Galois theory
cabal run surd-euler-integral    # Euler substitution demo
cabal run surd-elliptic-integral # Elliptic integral reduction demo
```

### Lean 4

```bash
cd lean4
lake build
```

### Mercury

```bash
cd mercury
mmc --make demo_trig_table
```

### OCaml

```bash
cd ocaml
eval $(opam env --switch=surd)
dune build
dune test
```

### Scala 3

```bash
cd scala3
sbt compile test
```

### Idris 2

```bash
cd idris2
idris2 --build surd.ipkg
```

### Koka

```bash
cd koka
koka --compile demo/trig_table.kk
```

### F*

```bash
cd fstar
make verify
```

### Clean

```bash
cd clean
clm DemoTrigTable -o demo_trig_table
```

### Curry

```bash
cd curry
cypm install
curry :load DemoTrigTable :eval main :quit
```

## Documentation

- **[docs/API.md](docs/API.md)** -- API reference organized by feature area
- **`cabal haddock`** -- Full Haddock documentation (Haskell)

## Mathematical Background

The library implements algorithms from:

- **Radical denesting**: Borodin, Fagin, Hopcroft & Tompa (1985); Landau (1992);
  Zippel (1985); Blomer (1991)
- **Cyclotomic radicals**: Weber & Keckeisen (2003); Gauss period theory
- **Galois theory**: Stauduhar (1973); Dummit (1991); Cox (2012)
- **Algebraic numbers**: Strzebonski (1997); Caviness & Fateman (1976, RADCAN)
- **Polynomial factoring**: Trager (1984); Kronecker; Berlekamp
- **Linear independence**: Besicovitch (1940); Nosan et al. (2022)
- **PSLQ**: Ferguson & Bailey (1999)

## License

BSD-3-Clause
