# surd API Reference

Concise reference for the surd symbolic mathematics library. All type
signatures are Haskell. Module paths are given for the Haskell
implementation; other language ports follow the same logical structure.

---

## 1. Expression Construction

### Surd.Types

The core AST. Constructors are "dumb" -- no simplification on construction.

```haskell
data RadExpr k
  = Lit !k                         -- Coefficient literal from field k
  | Neg !(RadExpr k)               -- Additive negation
  | Add !(RadExpr k) !(RadExpr k)  -- Addition
  | Mul !(RadExpr k) !(RadExpr k)  -- Multiplication
  | Inv !(RadExpr k)               -- Multiplicative inverse (1/e)
  | Root !Int !(RadExpr k)         -- Principal nth root (n >= 2)
  | Pow !(RadExpr k) !Int          -- Integer power
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Pattern synonyms
pattern Sub a b = Add a (Neg b)    -- Subtraction
pattern Div a b = Mul a (Inv b)    -- Division
pattern Sqrt x  = Root 2 x        -- Square root
```

| Function   | Type                                | Description                          |
|------------|-------------------------------------|--------------------------------------|
| `ratE`     | `Rational -> RadExpr Rational`      | Lift a rational into an expression   |
| `intE`     | `Integral a => a -> RadExpr Rational` | Lift an integer                    |

### Surd.Radical.Expr

Smart constructors and structural queries.

| Function           | Type                                              | Description                                       |
|--------------------|---------------------------------------------------|---------------------------------------------------|
| `lit`              | `k -> RadExpr k`                                  | Coefficient literal                               |
| `neg`              | `RadExpr k -> RadExpr k`                          | Negate                                            |
| `add`              | `RadExpr k -> RadExpr k -> RadExpr k`             | Add                                               |
| `sub`              | `RadExpr k -> RadExpr k -> RadExpr k`             | Subtract                                          |
| `mul`              | `RadExpr k -> RadExpr k -> RadExpr k`             | Multiply                                          |
| `div'`             | `RadExpr k -> RadExpr k -> RadExpr k`             | Divide (named `div'` to avoid Prelude clash)      |
| `inv`              | `RadExpr k -> RadExpr k`                          | Reciprocal                                        |
| `root`             | `Int -> RadExpr k -> RadExpr k`                   | nth root                                          |
| `sqrt'`            | `RadExpr k -> RadExpr k`                          | Square root (named `sqrt'` to avoid Prelude clash)|
| `pow`              | `RadExpr k -> Int -> RadExpr k`                   | Integer power                                     |
| `fromInteger'`     | `Integer -> RadExpr Rational`                     | Integer literal                                   |
| `fromRational'`    | `Rational -> RadExpr Rational`                    | Rational literal                                  |
| `depth`            | `RadExpr k -> Int`                                | Expression tree depth                             |
| `size`             | `RadExpr k -> Int`                                | Expression tree size (node count)                 |
| `freeOf`           | `Eq k => RadExpr k -> RadExpr k -> Bool`          | Test if expr is free of a subexpression           |
| `mapCoeffs`        | `(a -> b) -> RadExpr a -> RadExpr b`              | Map over coefficients (same as `fmap`)            |
| `collectRadicals`  | `RadExpr Rational -> [(Int, RadExpr Rational)]`   | List all (degree, radicand) pairs in the expr     |
| `topoSortRadicals` | `RadExpr Rational -> [(Int, RadExpr Rational)]`   | Topologically sorted radicals (deps first)        |
| `allRootsResolved` | `RadExpr Rational -> Bool`                        | True if all Root nodes have literal radicands     |

#### Quick Start

```haskell
import Surd.Radical.Expr
import Surd.Types

-- Build 3 + 2*sqrt(5)
expr :: RadExpr Rational
expr = add (lit 3) (mul (lit 2) (sqrt' (lit 5)))

-- Or using the raw constructors:
expr' = Add (Lit 3) (Mul (Lit 2) (Root 2 (Lit 5)))
```

---

## 2. Normalization

### Surd.Radical.Normalize

Explicit simplification passes. Does NOT denest -- see Denesting below.

| Function              | Type                                           | Description                                        |
|-----------------------|------------------------------------------------|-----------------------------------------------------|
| `normalize`           | `RadExpr Rational -> RadExpr Rational`         | All passes iterated to fixed point (up to 10x)     |
| `normalizeOnce`       | `RadExpr Rational -> RadExpr Rational`         | Single pass of all sub-passes composed once         |
| `flattenArith`        | `RadExpr k -> RadExpr k`                       | Cancel double negation/inversion                    |
| `foldConstants`       | `RadExpr Rational -> RadExpr Rational`         | Evaluate pure-literal subtrees                      |
| `simplifyPowers`      | `RadExpr Rational -> RadExpr Rational`         | (nth-root x)^n = x, nested roots combine           |
| `extractPerfectPowers` | `RadExpr Rational -> RadExpr Rational`        | sqrt(12) = 2*sqrt(3)                                |
| `collectCoefficients` | `RadExpr Rational -> RadExpr Rational`         | Merge Lit factors in products                       |
| `collectTerms`        | `RadExpr Rational -> RadExpr Rational`         | Group like terms in sums, add coefficients          |
| `distribute`          | `RadExpr Rational -> RadExpr Rational`         | Lit*(a+b) -> Lit*a + Lit*b                          |

The normalization pipeline is:
```
normalize = fixN 10 (collectTerms . collectCoefficients . distribute
  . sortCommutative . extractPerfectPowers . simplifyPowers
  . foldConstants . flattenArith)
```

### Surd.Radical.NormalForm

Canonical normal form: Q-linear combinations of radical monomials.

```haskell
data Atom = RatRoot Int Rational | ImagUnit | NestedRoot Int (RadExpr Rational)
newtype Monomial = Monomial (Map Atom Int)      -- Product of atoms with exponents
newtype NormExpr = NormExpr (Map Monomial Rational)  -- Sum of monomials with coefficients
```

| Function      | Type                                          | Description                                  |
|---------------|-----------------------------------------------|----------------------------------------------|
| `toNormExpr`  | `RadExpr Rational -> NormExpr`                | Convert tree to normal form                  |
| `fromNormExpr`| `NormExpr -> RadExpr Rational`                | Convert normal form back to tree             |
| `normLit`     | `Rational -> NormExpr`                        | Rational literal in NF                       |
| `normAtom`    | `Atom -> NormExpr`                            | Single atom in NF                            |
| `normAdd`     | `NormExpr -> NormExpr -> NormExpr`            | Add in NF (automatic like-term collection)   |
| `normSub`     | `NormExpr -> NormExpr -> NormExpr`            | Subtract in NF                               |
| `normNeg`     | `NormExpr -> NormExpr`                        | Negate in NF                                 |
| `normMul`     | `NormExpr -> NormExpr -> NormExpr`            | Multiply in NF (exponent reduction)          |
| `normScale`   | `Rational -> NormExpr -> NormExpr`            | Scalar multiply                              |
| `normPow`     | `NormExpr -> Int -> NormExpr`                 | Integer power in NF                          |
| `normInv`     | `NormExpr -> NormExpr`                        | Inversion in NF (rationalization)            |
| `normIsZero`  | `NormExpr -> Bool`                            | Test if expression is zero                   |
| `normCoeff`   | `NormExpr -> Monomial -> Rational`            | Coefficient of a monomial                    |

#### Quick Start

```haskell
import Surd.Radical.NormalForm

-- Round-trip canonicalizes: sqrt(2)*sqrt(2) -> 2
let e = Mul (Root 2 (Lit 2)) (Root 2 (Lit 2))
fromNormExpr (toNormExpr e)  -- Lit 2
```

---

## 3. Evaluation

### Surd.Radical.Eval

Numerical evaluation at various precisions.

| Function               | Type                                                    | Description                                    |
|------------------------|---------------------------------------------------------|------------------------------------------------|
| `eval`                 | `RadExpr Rational -> Double`                            | Fast floating-point evaluation                 |
| `evalComplex`          | `RadExpr Rational -> Complex Double`                    | Complex Double evaluation                      |
| `evalExact`            | `RadExpr Rational -> ExactReal`                         | CReal evaluation (~60 decimal digits)          |
| `evalComplexExact`     | `RadExpr Rational -> ExactComplex`                      | Complex CReal evaluation                       |
| `evalInterval`         | `RadExpr Rational -> Interval`                          | Rigorous rational interval evaluation          |
| `evalComplexInterval`  | `RadExpr Rational -> ComplexInterval`                    | Complex rational interval evaluation           |

Type aliases: `ExactReal = CReal 200`, `ExactComplex = Complex ExactReal`.

### Surd.Radical.EvalMP

Arbitrary-precision ball arithmetic via aern2-mp.

| Function                      | Type                                              | Description                                         |
|-------------------------------|---------------------------------------------------|-----------------------------------------------------|
| `dagEvalComplexMP`            | `Int -> RadDAG -> ComplexInterval`                 | Evaluate DAG at given bit precision                 |
| `dagEvalRealMP`               | `Int -> RadDAG -> Interval`                        | Real part only, at given bit precision              |
| `mpBallToInterval`            | `MPBall -> Interval`                               | Convert MPBall to rational interval                 |
| `complexMPToComplexInterval`  | `ComplexMP -> ComplexInterval`                      | Convert complex MPBall to complex interval          |
| `dftCoeffsMP`                 | `Int -> Int -> RadDAG -> [Rational]`               | DFT coefficients at high precision                  |

### Surd.Radical.DAG

Explicit sharing-preserving DAG representation.

```haskell
data RadDAG = RadDAG
  { dagNodes :: IntMap (RadNodeOp, [NodeId])
  , dagRoot  :: NodeId
  }
```

| Function                  | Type                                               | Description                                          |
|---------------------------|------------------------------------------------------|------------------------------------------------------|
| `toDAG`                   | `RadExpr Rational -> RadDAG`                         | Convert tree to DAG (StableName sharing detection)   |
| `fromDAG`                 | `RadDAG -> RadExpr Rational`                         | Convert DAG back to tree (sharing preserved)         |
| `dagSize`                 | `RadDAG -> Int`                                      | Number of unique nodes                               |
| `dagDepth`                | `RadDAG -> Int`                                      | Maximum path length from root                        |
| `dagFoldConstants`        | `RadDAG -> RadDAG`                                   | Constant folding + power simplification on DAG       |
| `dagNormalize`            | `RadDAG -> RadDAG`                                   | Like-term/coefficient collection on DAG              |
| `dagEvalComplex`          | `RadDAG -> Complex Double`                           | O(n) complex Double evaluation on DAG                |
| `dagEvalComplexInterval`  | `RadDAG -> ComplexInterval`                           | O(n) complex interval evaluation on DAG              |

#### Quick Start

```haskell
import Surd.Radical.DAG

let dag = toDAG (cosOfUnity 37)  -- Large expression with sharing
dagSize dag     -- Number of unique nodes (not exponential tree size)
dagDepth dag    -- DAG depth
let simplified = fromDAG (dagFoldConstants dag)
```

---

## 4. Rendering

### Surd.Radical.Pretty

| Function    | Type                                  | Description                                     |
|-------------|---------------------------------------|-------------------------------------------------|
| `pretty`    | `RadExpr Rational -> String`          | Human-readable mathematical notation            |
| `prettyPrec`| `Int -> RadExpr Rational -> String`   | With precedence context (for parenthesization)  |
| `prettyCSE` | `RadExpr Rational -> String`          | With common subexpression elimination (let bindings) |

### Surd.Radical.LaTeX

| Function    | Type                                  | Description                                     |
|-------------|---------------------------------------|-------------------------------------------------|
| `latex`     | `RadExpr Rational -> String`          | LaTeX math-mode rendering                       |
| `latexPrec` | `Int -> RadExpr Rational -> String`   | With precedence context                         |
| `latexDAG`  | `RadExpr Rational -> String`          | DAG-aware LaTeX (names shared subexpressions)   |

#### Quick Start

```haskell
import Surd.Radical.Pretty
import Surd.Radical.LaTeX

let e = normalize (sqrt' (lit 12))
pretty e   -- "2sqrt(3)"
latex e    -- "2\\sqrt{3}"
```

---

## 5. Denesting

### Surd.Radical.Denest

Top-level dispatcher: tries specialized algorithms first, falls back to Landau.

| Function     | Type                                       | Description                                  |
|--------------|--------------------------------------------|----------------------------------------------|
| `denest`     | `RadExpr Rational -> RadExpr Rational`     | Denest with pre-normalization                |
| `denestFull` | `RadExpr Rational -> RadExpr Rational`     | Denest without pre-normalization             |

### Surd.Radical.Denest.Sqrt

Borodin's algorithm for square root denesting.

### Surd.Radical.Denest.NthRoot

Cube root and higher-degree root denesting.

### Surd.Radical.Denest.Landau

Landau's algorithm via Trager factoring over algebraic extensions.
Handles depth 1-4 radicands with degree 1-4 factor extraction
(Cardano for cubics, Ferrari for quartics).

#### Quick Start

```haskell
import Surd.Radical.Denest
import Surd.Radical.Pretty

-- sqrt(3 + 2*sqrt(2)) = 1 + sqrt(2)
pretty (denest (sqrt' (add (lit 3) (mul (lit 2) (sqrt' (lit 2))))))
-- "1 + sqrt(2)"

-- Cube root denesting
pretty (denest (root 3 (add (lit 2) (sqrt' (lit 5)))))
```

---

## 6. Trigonometry

### Surd.Trig

Primary entry points for exact trig evaluation.

| Function              | Type                                        | Description                                        |
|-----------------------|---------------------------------------------|----------------------------------------------------|
| `cosExact`            | `Integer -> Integer -> TrigResult`          | cos(p*pi/q) as a radical or minimal polynomial     |
| `sinExact`            | `Integer -> Integer -> TrigResult`          | sin(p*pi/q) as a radical or minimal polynomial     |
| `tanExact`            | `Integer -> Integer -> Maybe TrigResult`    | tan(p*pi/q), Nothing if sin or cos is MinPoly      |
| `cosMinPoly`          | `Int -> Poly Rational`                      | Cyclotomic polynomial for cos(2*pi/n)              |
| `simplifyTrigResult`  | `TrigResult -> TrigResult`                  | Simplify for display (Cardano + NF round-trip)     |
| `simplifiedSin`       | `Integer -> Integer -> TrigResult -> TrigResult` | Compute sin from simplified cos               |

```haskell
data TrigResult
  = Radical (RadExpr Rational)   -- Exact radical expression
  | MinPoly (Poly Rational)      -- Minimal polynomial fallback
```

### Surd.Trig.RootOfUnity

| Function           | Type                                               | Description                                      |
|--------------------|----------------------------------------------------|--------------------------------------------------|
| `cosOfUnity`       | `Int -> Maybe (RadExpr Rational)`                  | cos(2*pi/n) as a radical expression              |
| `sinOfUnity`       | `Int -> Maybe (RadExpr Rational)`                  | sin(2*pi/n)                                      |
| `allCosOfUnity`    | `Int -> Maybe (Map Int (RadExpr Rational))`        | All cos(2*pi*k/n) for k = 0..n-1                |
| `allSinOfUnity`    | `Int -> Maybe (Map Int (RadExpr Rational))`        | All sin(2*pi*k/n) for k = 0..n-1                |
| `isConstructible`  | `Int -> Bool`                                      | True if only square roots are needed             |
| `fermatPrimes`     | `[Integer]`                                        | Known Fermat primes [3, 5, 17, 257, 65537]       |

### Surd.Trig.Galois

Gauss period descent engine.

| Function              | Type                                                        | Description                                    |
|-----------------------|-------------------------------------------------------------|------------------------------------------------|
| `cosOfUnityViaGauss`  | `Int -> Maybe (RadExpr Rational)`                           | cos(2*pi/n) via Gauss period descent           |
| `allPeriodsViaGauss`  | `Int -> Maybe (Map Int (RadExpr Rational))`                 | All zeta^k as radical expressions              |
| `gaussPeriods`        | `Int -> Int -> [RadExpr Rational] -> [RadExpr Rational]`    | Compute Gauss periods for one descent step     |
| `primitiveRoot`       | `Int -> Int`                                                | Smallest primitive root mod n                  |
| `subgroupChain`       | `Int -> [(Int, Int)]`                                       | Subgroup chain of (Z/nZ)* for descent          |

### Surd.Trig.TowerDescent

Alternative tower-based descent (field tower representation).

| Function              | Type                                          | Description                                    |
|-----------------------|-----------------------------------------------|------------------------------------------------|
| `cosViaTower`         | `Int -> Maybe TowerResult`                    | cos(2*pi/n) via dynamic field tower            |
| `allPeriodsViaTower`  | `Int -> Maybe (Map Int TowerResult)`          | All periods via tower                          |
| `evalTowerApprox`     | `TowerResult -> Complex Double`               | Approximate numerical evaluation               |

#### Quick Start

```haskell
import Surd.Trig
import Surd.Radical.Pretty

-- cos(pi/5) = (sqrt(5) - 1) / 4
case cosExact 1 5 of
  Radical e -> putStrLn (pretty e)

-- sin(pi/6) = 1/2
case sinExact 1 6 of
  Radical e -> putStrLn (pretty e)

-- cos(2*pi/7): involves cube roots (casus irreducibilis)
case simplifyTrigResult (cosExact 2 7) of
  Radical e -> putStrLn (pretty e)
```

---

## 7. Algebraic Numbers

### Surd.Algebraic.Number

Algebraic numbers as (minimal polynomial, isolating interval) pairs.

```haskell
data AlgNum = AlgNum
  { anMinPoly  :: Poly Rational   -- Minimal polynomial over Q
  , anInterval :: Interval        -- Isolating interval for the root
  }
```

| Function         | Type                                       | Description                                  |
|------------------|--------------------------------------------|----------------------------------------------|
| `algFromRational`| `Rational -> AlgNum`                       | Embed a rational number                      |
| `algFromPoly`    | `Poly Rational -> Double -> AlgNum`        | AlgNum from polynomial + approximate root    |
| `algMinPoly`     | `AlgNum -> Poly Rational`                  | Extract minimal polynomial                   |
| `algApprox`      | `AlgNum -> Double`                         | Numerical approximation                      |
| `algAdd`         | `AlgNum -> AlgNum -> AlgNum`               | Addition via composed sum                    |
| `algMul`         | `AlgNum -> AlgNum -> AlgNum`               | Multiplication via composed product          |
| `algNeg`         | `AlgNum -> AlgNum`                         | Negate                                       |
| `algInv`         | `AlgNum -> AlgNum`                         | Reciprocal                                   |
| `algSub`         | `AlgNum -> AlgNum -> AlgNum`               | Subtraction                                  |
| `algDiv`         | `AlgNum -> AlgNum -> AlgNum`               | Division                                     |
| `algPow`         | `AlgNum -> Int -> AlgNum`                  | Integer power                                |
| `algRoot`        | `AlgNum -> Int -> AlgNum`                  | nth root                                     |
| `algEq`          | `AlgNum -> AlgNum -> Bool`                 | Rigorous equality (Sturm-based)              |
| `algCompare`     | `AlgNum -> AlgNum -> Ordering`             | Rigorous ordering (Sturm-based)              |
| `algShow`        | `AlgNum -> String`                         | Display                                      |

### Surd.Algebraic.Convert

Conversion between radical expressions and algebraic numbers.

| Function              | Type                                            | Description                                       |
|-----------------------|-------------------------------------------------|---------------------------------------------------|
| `radExprToAlgNum`     | `RadExpr Rational -> AlgNum`                    | Compute minimal polynomial + isolating interval   |
| `algNumToRadExpr`     | `AlgNum -> Maybe (RadExpr Rational)`            | Express as radicals (deg <= 5 if solvable)        |
| `simplifyViaCanonical`| `RadExpr Rational -> RadExpr Rational`          | Round-trip simplification: expr -> AlgNum -> expr |
| `algNumInfo`          | `AlgNum -> String`                              | Debug info (minpoly, interval, approx)            |

### Surd.Algebraic.RootIsolation

Sturm-based root isolation over Q.

#### Quick Start

```haskell
import Surd.Algebraic.Number
import Surd.Algebraic.Convert
import Surd.Radical.Pretty

-- Convert a complex radical expression to its minimal polynomial
let e = add (sqrt' (lit 2)) (sqrt' (lit 3))
let a = radExprToAlgNum e
algMinPoly a   -- x^4 - 10x^2 + 1

-- Simplify via round-trip
pretty (simplifyViaCanonical e)
```

---

## 8. Minimal Polynomials

### Surd.Polynomial.MinimalPoly

Resultant-based computation.

| Function          | Type                                         | Description                                     |
|-------------------|----------------------------------------------|-------------------------------------------------|
| `minimalPoly`     | `RadExpr Rational -> Poly Rational`          | Minimal polynomial over Q                       |
| `annihilatingPoly`| `RadExpr Rational -> Poly Rational`          | Annihilating polynomial (may not be irreducible)|

### Surd.Polynomial.MinimalPolyTower

Tower-based computation (faster for expressions with many shared radicals).

| Function                | Type                                           | Description                                      |
|-------------------------|------------------------------------------------|--------------------------------------------------|
| `minimalPolyTower`      | `RadExpr Rational -> Poly Rational`            | Minimal polynomial via extension tower           |
| `annihilatingPolyTower` | `RadExpr Rational -> Poly Rational`            | Annihilating polynomial via tower                |
| `collectRadicals`       | `RadExpr Rational -> [(Int, RadExpr Rational)]`| Collect distinct radicals for tower construction |

---

## 9. Field Extensions

### Math.Field.Extension (algebraic-polynomials)

Arithmetic in K(alpha) = K[x]/(minpoly).

```haskell
data ExtElem k = ExtElem
  { eeCoeffs  :: [k]           -- Polynomial representation
  , eeField   :: ExtField k    -- The extension field
  }
-- Has Num, Fractional instances
```

### Surd.Field.Tower

Static (type-level) extension towers.

| Function      | Type                                                              | Description                             |
|---------------|-------------------------------------------------------------------|-----------------------------------------|
| `adjoinRoot`  | `(Eq k, Fractional k) => Int -> k -> (ExtField k, ExtElem k)`    | Adjoin nth root: K -> K(alpha)          |
| `adjoinSqrt`  | `(Eq k, Fractional k) => k -> (ExtField k, ExtElem k)`           | Adjoin square root                      |
| `evalInField` | `(Eq k, Fractional k) => ExtField k -> RadExpr k -> ExtElem k`   | Evaluate a radical expression in K(alpha)|

### Surd.Field.DynTower

Dynamic (runtime-depth) extension towers.

| Function          | Type                                              | Description                                  |
|-------------------|---------------------------------------------------|----------------------------------------------|
| `adjoinTowerRoot` | `Int -> TowerElem -> (TowerLevel, TowerElem)`     | Extend tower by one level                    |
| `towerToRadExpr`  | `TowerElem -> RadExpr Rational`                   | Convert tower element to radical expression  |
| `tIsZero`         | `TowerElem -> Bool`                               | Test for zero                                |
| `tLevel`          | `TowerElem -> Int`                                | Tower depth                                  |
| `promoteTo`       | `TowerLevel -> TowerElem -> TowerElem`            | Promote to a higher tower level              |

---

## 10. Polynomial Operations

All in the **algebraic-polynomials** package.

### Math.Polynomial.Univariate

Sparse univariate polynomials over any ring.

```haskell
newtype Poly k = Poly (Map Int k)  -- Map from degree to coefficient
```

Standard arithmetic: `polyAdd`, `polyMul`, `polyDiv`, `polyMod`, `polyGCD`,
`degree`, `leadCoeff`, `monicPoly`, `polyEval`, `polyDeriv`.

### Math.Polynomial.Multivariate

Sparse multivariate polynomials.

```haskell
newtype MPoly k = MPoly (Map (Map Int Int) k)  -- Map from monomial to coefficient
```

Operations: `mpolyAdd`, `mpolyMul`, `mpolyEval`, `gcdMPoly` (subresultant GCD),
`reduceFrac` (rational function simplification).

### Math.Polynomial.Factoring

| Function           | Type                                              | Description                               |
|--------------------|---------------------------------------------------|-------------------------------------------|
| `factorSquareFree` | `Poly Rational -> [Poly Rational]`                | Square-free factorization over Q          |
| `factor`           | `Poly Rational -> [(Poly Rational, Int)]`         | Full factorization over Q (Kronecker)     |
| `rationalRoots`    | `Poly Rational -> [Rational]`                     | Find all rational roots                   |

### Math.Polynomial.TragerFactoring

| Function                   | Type                                                        | Description                                 |
|----------------------------|-------------------------------------------------------------|---------------------------------------------|
| `factorSFOverExtension`    | `Poly (ExtElem Rational) -> [Poly (ExtElem Rational)]`      | Factor over Q(alpha)                        |
| `factorSFOverExtensionK`   | `Poly (ExtElem k) -> [Poly (ExtElem k)]`                    | Factor over any K(alpha)                    |

### Math.Polynomial.Cyclotomic

| Function     | Type                        | Description                    |
|--------------|-----------------------------|--------------------------------|
| `cyclotomic` | `Int -> Poly Rational`      | nth cyclotomic polynomial      |

### Math.Polynomial.Resultant

Resultant, composed polynomials, and related operations.

| Function           | Type                                                    | Description                              |
|--------------------|---------------------------------------------------------|------------------------------------------|
| `polyResultant`    | `Poly k -> Poly k -> k`                                | Resultant of two polynomials             |
| `composedSum`      | `Poly Rational -> Poly Rational -> Poly Rational`      | Polynomial whose roots are a_i + b_j     |
| `composedProduct`  | `Poly Rational -> Poly Rational -> Poly Rational`      | Polynomial whose roots are a_i * b_j     |

### Math.Polynomial.Groebner (Surd.Radical.Groebner)

Buchberger's algorithm and ideal reduction.

### Math.Polynomial.RootBound

Root bound computation and approximate root finding.

### Math.Internal.PSLQ

PSLQ integer relation finding for numerical minimal polynomial recovery.

---

## 11. Galois Theory

### Surd.Galois.Permutation

Permutation groups with Schreier-Sims BSGS computation.

### Surd.Galois.TransitiveGroup

Database of transitive subgroups of S_n (n <= 5) with Butler-McKay numbering.

### Surd.Galois.Identify

Galois group identification for irreducible polynomials.

| Function               | Type                                       | Description                                     |
|------------------------|--------------------------------------------|-------------------------------------------------|
| `identifyGaloisGroup5` | `Poly Rational -> GaloisResult`            | Identify Galois group of degree-5 polynomial    |

```haskell
data GaloisResult
  = GaloisGroup TransitiveGroup   -- Identified group (C5, D5, F20, A5, S5)
  | GaloisUnknown String          -- Could not identify
```

### Surd.Galois.Resolvent

Resolvent polynomial computation (discriminant, sextic, etc.).

### Surd.Galois.RadicalTower

Radical tower construction for solvable polynomials via Lagrange resolvents.

### Surd.Galois.Solve

Top-level solving interface.

| Function           | Type                                                   | Description                                          |
|--------------------|--------------------------------------------------------|------------------------------------------------------|
| `solveAlgNum`      | `AlgNum -> Maybe (RadExpr Rational)`                   | Express algebraic number as radical (deg 5 solvable) |
| `solvePoly`        | `Poly Rational -> Maybe [RadExpr Rational]`            | All roots as radicals (if solvable)                  |
| `identifyAndSolve` | `Poly Rational -> (GaloisResult, Maybe [RadExpr Rational])` | Identify group and solve if possible            |

#### Quick Start

```haskell
import Surd.Galois.Solve
import Surd.Galois.Identify
import Math.Polynomial.Univariate
import Surd.Radical.Pretty

-- x^5 - 5x + 12 has Galois group D5 (solvable)
let f = polyFromList [(0, 12), (1, -5), (5, 1)]
case identifyAndSolve f of
  (GaloisGroup g, Just roots) -> do
    putStrLn ("Galois group: " ++ show g)
    mapM_ (putStrLn . pretty) roots
  (GaloisGroup g, Nothing) ->
    putStrLn ("Group " ++ show g ++ " is not solvable")
```

---

## 12. Integration

### Surd.Integration.Euler

Euler substitution for integrals with square roots of quadratics.

| Function            | Type                                              | Description                                     |
|---------------------|---------------------------------------------------|-------------------------------------------------|
| `eulerIntegrate`    | `EulerIntegrand -> IntegralResult`                | Integrate P(x)/Q(x) * sqrt(ax^2+bx+c)^n dx     |
| `integrateRational` | `Poly Rational -> Poly Rational -> SymExpr`       | Integrate a rational function (Hermite + partial fractions) |
| `prettySymExpr`     | `SymExpr -> String`                               | Pretty-print symbolic result                    |
| `latexSymExpr`      | `SymExpr -> String`                               | LaTeX render symbolic result                    |

### Surd.Integration.Elliptic

Elliptic integral reduction to Legendre normal forms.

| Function                | Type                                          | Description                                       |
|-------------------------|-----------------------------------------------|---------------------------------------------------|
| `reduceElliptic`        | `EllipticIntegrand -> EllipticResult`         | Reduce to F, E, Pi with exact radical modulus k   |
| `prettyEllipticResult`  | `EllipticResult -> String`                    | Pretty-print result                               |
| `latexEllipticResult`   | `EllipticResult -> String`                    | LaTeX render result                               |

---

## 13. Equality and Ordering

### Surd.Radical.Equality

Rigorous equality via algebraic number comparison.

| Function    | Type                                                    | Description                                |
|-------------|---------------------------------------------------------|--------------------------------------------|
| `radicalEq` | `RadExpr Rational -> RadExpr Rational -> Bool`          | True iff same algebraic number             |
| `radicalNeq`| `RadExpr Rational -> RadExpr Rational -> Bool`          | True iff different algebraic numbers       |

### Surd.Radical.Order

Rigorous ordering via Sturm-based interval refinement.

| Function        | Type                                                  | Description                      |
|-----------------|-------------------------------------------------------|----------------------------------|
| `radicalCompare`| `RadExpr Rational -> RadExpr Rational -> Ordering`    | Rigorous comparison              |
| `radicalLt`     | `RadExpr Rational -> RadExpr Rational -> Bool`        | Strictly less than               |
| `radicalGt`     | `RadExpr Rational -> RadExpr Rational -> Bool`        | Strictly greater than            |
| `radicalLeq`    | `RadExpr Rational -> RadExpr Rational -> Bool`        | Less than or equal               |
| `radicalGeq`    | `RadExpr Rational -> RadExpr Rational -> Bool`        | Greater than or equal            |

#### Quick Start

```haskell
import Surd.Radical.Equality
import Surd.Radical.Order

-- These are the same algebraic number:
radicalEq (sqrt' (lit 2)) (div' (lit 2) (sqrt' (lit 2)))  -- True

-- Rigorous ordering:
radicalCompare (sqrt' (lit 2)) (lit (3/2))  -- LT  (sqrt(2) < 1.5)
```

---

## Cross-Language Equivalents

| Operation           | Haskell                          | OCaml                             | Lean 4                    | Scala 3                      |
|---------------------|----------------------------------|-----------------------------------|---------------------------|-------------------------------|
| Create rational     | `lit (3/4)`                      | `Expr.lit (Q.make 3 4)`          | `lit (3 / 4)`             | `Expr.lit(Rational(3, 4))`   |
| Square root         | `sqrt' (lit 2)`                  | `Expr.sqrt (Expr.lit (Q.of_int 2))` | `sqrt (lit 2)`         | `Expr.sqrt(Expr.lit(2))`    |
| Normalize           | `normalize expr`                 | `Normalize.normalize expr`        | `normalize expr`          | `expr.normalize`             |
| Denest              | `denest expr`                    | `Denest.denest expr`              | `denest expr`             | `expr.denest`                |
| Pretty print        | `pretty expr`                    | `Pretty.to_string expr`          | `pretty expr`             | `expr.pretty`                |
| LaTeX               | `latex expr`                     | `Latex.to_string expr`           | `latex expr`              | `expr.latex`                 |
| Cos exact           | `cosExact 1 5`                   | `Trig.cos_exact 1 5`             | `cosExact 1 5`            | `Trig.cosExact(1, 5)`       |
| Sin exact           | `sinExact 1 6`                   | `Trig.sin_exact 1 6`             | `sinExact 1 6`            | `Trig.sinExact(1, 6)`       |
| Evaluate (Double)   | `eval expr`                      | `Eval.to_float expr`             | `eval expr`               | `expr.evalDouble`            |
| Radical equality    | `radicalEq a b`                  | `Equality.equal a b`             | `radicalEq a b`           | `a.radicalEq(b)`            |
| To algebraic number | `radExprToAlgNum expr`           | `Convert.to_algnum expr`         | `toAlgNum expr`           | `expr.toAlgNum`              |
| Minimal polynomial  | `minimalPoly expr`               | `MinPoly.minimal expr`           | `minimalPoly expr`        | `expr.minimalPoly`           |

---

## Module Index

### algebraic-polynomials package

| Module                          | Description                                          |
|---------------------------------|------------------------------------------------------|
| `Math.Polynomial.Univariate`    | Sparse univariate polynomials over any ring          |
| `Math.Polynomial.Multivariate`  | Sparse multivariate polynomials                      |
| `Math.Polynomial.Groebner`      | Buchberger's algorithm, ideal reduction              |
| `Math.Polynomial.Factoring`     | Square-free and Kronecker factoring over Q           |
| `Math.Polynomial.Cyclotomic`    | Cyclotomic polynomials                               |
| `Math.Polynomial.Resultant`     | Resultant, composed sum/product, interpolation       |
| `Math.Polynomial.RootBound`     | Root bounds and approximate root finding             |
| `Math.Polynomial.TragerFactoring`| Factoring over algebraic extension fields           |
| `Math.Field.Extension`          | Arithmetic in K(alpha) = K[x]/(minpoly)             |
| `Math.Field.Transcendental`     | Rational function field Q(x1, ..., xn)              |
| `Math.Internal.Positive`        | Positive newtype for type-safe positive integers     |
| `Math.Internal.PrimeFactors`    | Prime factorization, primality testing               |
| `Math.Internal.Interval`        | Rational interval arithmetic                         |
| `Math.Internal.PSLQ`            | PSLQ integer relation algorithm                      |

### surd package

| Module                          | Description                                          |
|---------------------------------|------------------------------------------------------|
| `Surd.Types`                    | Core RadExpr AST                                     |
| `Surd.Radical.Expr`             | Smart constructors, structural queries               |
| `Surd.Radical.Normalize`        | Normalization passes (constant folding, distribution)|
| `Surd.Radical.NormalForm`       | Canonical normal form (radical monomials)            |
| `Surd.Radical.DAG`              | Explicit DAG representation with sharing             |
| `Surd.Radical.Eval`             | Numerical evaluation (Double, CReal, Interval)       |
| `Surd.Radical.EvalMP`           | Arbitrary-precision MPBall evaluation                |
| `Surd.Radical.Pretty`           | Human-readable text rendering                        |
| `Surd.Radical.LaTeX`            | LaTeX math-mode rendering                            |
| `Surd.Radical.Denest`           | Top-level denesting dispatcher                       |
| `Surd.Radical.Denest.Sqrt`      | Borodin square root denesting                        |
| `Surd.Radical.Denest.NthRoot`   | Cube root and nth root denesting                     |
| `Surd.Radical.Denest.Landau`    | Landau denesting via Trager factoring                |
| `Surd.Radical.Equality`         | Rigorous equality testing                            |
| `Surd.Radical.Order`            | Rigorous ordering                                    |
| `Surd.Radical.Groebner`         | Ideal reduction for radical expressions              |
| `Surd.Trig`                     | Exact cos/sin/tan at rational multiples of pi        |
| `Surd.Trig.RootOfUnity`         | Root of unity radical expressions                    |
| `Surd.Trig.Galois`              | Gauss period descent engine                          |
| `Surd.Trig.TowerDescent`        | Tower-based period descent (alternative)             |
| `Surd.Algebraic.Number`         | AlgNum type and arithmetic                           |
| `Surd.Algebraic.Convert`        | RadExpr <-> AlgNum conversion                        |
| `Surd.Algebraic.RootIsolation`  | Sturm-based root isolation                           |
| `Surd.Polynomial.MinimalPoly`   | Resultant-based minimal polynomials                  |
| `Surd.Polynomial.MinimalPolyTower` | Tower-based minimal polynomials (fast)            |
| `Surd.Field.Tower`              | Static algebraic extension towers                    |
| `Surd.Field.DynTower`           | Dynamic runtime-depth extension towers               |
| `Surd.Field.DynTower.Display`   | Pretty-printing for tower elements                   |
| `Surd.Galois.Permutation`       | Permutation groups, Schreier-Sims                    |
| `Surd.Galois.TransitiveGroup`   | Transitive subgroup database                         |
| `Surd.Galois.Resolvent`         | Resolvent polynomial computation                     |
| `Surd.Galois.Identify`          | Galois group identification                          |
| `Surd.Galois.RadicalTower`      | Radical tower construction for solvable groups       |
| `Surd.Galois.Solve`             | Top-level polynomial solving interface               |
| `Surd.Integration.Euler`        | Euler substitution for radical integrals             |
| `Surd.Integration.Elliptic`     | Elliptic integral reduction to Legendre forms        |
