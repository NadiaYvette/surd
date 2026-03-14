{- |
Module      : Surd.Galois.RadicalTower
Description : Radical tower construction for solvable polynomials via Lagrange resolvents
Stability   : experimental

Radical tower construction for solvable polynomials.

Given an irreducible polynomial \(f(x) \in \mathbb{Q}[x]\) of degree \(n\) with
solvable Galois group \(G\), construct radical expressions for its roots via
Lagrange resolvents descending through the composition series of \(G\).

The algorithm generalises the Gauss period descent from "Surd.Trig.Galois"
to arbitrary solvable polynomials. Currently supports degree 5 with Galois
groups \(C_5\), \(D_5\), or \(F_{20}\) (the Frobenius group of order 20).

=== Lagrange resolvent theory

For a cyclic extension of degree \(n\) with roots
\(\alpha_0, \alpha_1, \ldots, \alpha_{n-1}\) ordered so that the Galois generator
acts as the cyclic shift \(\sigma(\alpha_k) = \alpha_{k+1 \bmod n}\), the
__Lagrange resolvents__ are:

\[
  R_j = \sum_{k=0}^{n-1} \omega^{jk} \alpha_k
\]

where \(\omega = e^{2\pi i/n}\) is a primitive \(n\)-th root of unity.
The key property is that \(R_j^n\) is fixed by \(\sigma\) (since
\(\sigma(R_j) = \omega^{\-j} R_j\)), so \(R_j^n\) lies in the coefficient field.

=== DFT relationship

The values \(R_j^n\) are recovered via their DFT coefficients:

\[
  R_j^n = \sum_{s=0}^{n-1} d_s \, \omega^{js}
\]

where the \(d_s\) are given by the inverse DFT:

\[
  d_s = \frac{1}{n} \sum_{j=0}^{n-1} \omega^{\-js} R_j^n
\]

=== Inverse DFT for root recovery

Once the \(R_j\) are known (as radical expressions), the roots are recovered by
the inverse DFT:

\[
  \alpha_k = \frac{1}{n} \sum_{j=0}^{n-1} \omega^{\-jk} R_j
\]

=== Branch selection

Computing \(R_j\) from \(R_j^n\) requires choosing the correct \(n\)-th root branch.
The principal \(n\)-th root \(\sqrt[n]{R_j^n}\) may differ from the true \(R_j\) by
a power of \(\omega\):

\[
  R_j = \omega^{b_j} \cdot \sqrt[n]{R_j^n}
\]

The branch index \(b_j\) is determined by numerical comparison against the
known numerical value of \(R_j\).

=== Algorithm pipeline

@
 f(x) in Q[x], degree 5, solvable Galois group G
    |
    v
 [1] Depress: shift to eliminate x^4 term
    |
    v
 [2] findCyclicOrdering: permute numerical roots so
     sigma acts as (0 1 2 3 4)
    |
    v
 [3] Compute R_j^5 numerically via Lagrange resolvents
    |
    v
 [4] DFT: compute d_s from R_j^5
    |
    v
 [5] matchDs: recognise d_s in the coefficient field
     C5:  d_s in Q
     D5:  d_s in Q(sqrt(disc))
     F20: d_s in Q(omega_5)
    |
    v
 [6] Reconstruct R_j^5 as RadExpr, take 5th root,
     select branch via selectBranch5
    |
    v
 [7] Inverse DFT: alpha_k = (1\/5) sum omega^{\-jk} R_j
    |
    v
 [8] Un-depress and match to original root ordering
@

=== Galois orbit structure

The Galois group acts on the DFT coefficients \(d_s\) via
\(\tau(d_s) = d_{\tau^*(s)}\) where \(\tau^*\) is the induced action on indices.
This determines which field the \(d_s\) live in:

* __\(C_5\)__ (cyclic group of order 5): The generator \(\sigma\) fixes all
  \(d_s\) individually, so every \(d_s \in \mathbb{Q}\).

* __\(D_5\)__ (dihedral group of order 10): The extra involution
  \(\tau: \alpha_k \mapsto \alpha_{\-k}\) sends \(d_s \mapsto d_{\-s \bmod 5}\).
  Orbits: \(\{d_0\}\), \(\{d_1, d_4\}\), \(\{d_2, d_3\}\). Each conjugate pair
  satisfies a quadratic over \(\mathbb{Q}\).

* __\(F_{20}\)__ (Frobenius group of order 20): The generator
  \(\tau: \alpha_k \mapsto \alpha_{2k \bmod 5}\) sends
  \(d_s \mapsto d_{2s \bmod 5}\). Orbits: \(\{d_0\}\),
  \(\{d_1, d_2, d_4, d_3\}\). The four non-trivial \(d_s\) lie in
  \(\mathbb{Q}(\omega_5)\) and form a single Galois orbit.

=== References

* Lagrange, J. L. (1770–1771). \"Réflexions sur la résolution algébrique
  des équations.\" /Nouveaux Mémoires de l'Académie Royale/, Berlin.
* Dummit, D. S. (1991). \"Solving solvable quintics.\"
  /Math. Comp./ 57(195), 387–401.
  DOI: 10.1090\/S0025-5718-1991-1079014-X
* Cox, D. A. (2012). /Galois Theory/, 2nd ed. Wiley.
  DOI: 10.1002\/9781118218457
-}
module Surd.Galois.RadicalTower (
    -- * Entry point
    solveViaTower,
    solveViaTowerN,

    -- * Cyclic ordering
    findCyclicOrdering,
)
where

import Data.Complex (Complex (..), imagPart, magnitude, mkPolar, phase, realPart)
import Data.List (minimumBy, permutations, sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Math.Polynomial.Univariate (Poly, degree, unPoly)
import Surd.Galois.Identify (GaloisResult (..))
import Surd.Galois.TransitiveGroup
import Surd.Radical.DAG (dagEvalComplex, dagFoldConstants, fromDAG, toDAG)
import Data.Map.Strict qualified as Map
import Surd.Trig.Galois (allPeriodsViaGauss)
import Surd.Types

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

{- | Solve a solvable polynomial via Lagrange resolvent descent.

Given a 'GaloisResult' (containing the identified transitive group and
numerical roots) and the original polynomial \(f \in \mathbb{Q}[x]\),
returns radical expressions for all \(n\) roots in the order matching the
numerical roots from 'GaloisResult'.

__Preconditions:__

* The Galois group must be solvable ('tgSolvable').
* The polynomial must have degree 5 (other degrees are not yet supported).

Returns 'Nothing' if either precondition fails.

The returned list contains exactly 5 'RadExpr' values, one per root,
including complex conjugate pairs where applicable.
-}
solveViaTower :: GaloisResult -> Poly Rational -> Maybe [RadExpr Rational]
solveViaTower gr f
    | not (tgSolvable (grGroup gr)) = Nothing
    | degree f /= 5 = Nothing
    | otherwise = solveSolvableQuintic (grGroup gr) f (grRoots gr)

------------------------------------------------------------------------
-- Solvable quintic solver (unified for C₅, D₅, F₂₀)
------------------------------------------------------------------------

{- | Core algorithm for solvable quintics. Implements the full pipeline:

1. __Depress__: make monic and shift \(x \to x - a_4/5\) to eliminate the
   \(x^4\) term, yielding depressed roots.
2. __Cyclic ordering__: call 'findCyclicOrdering' to find a permutation of
   the 5 depressed roots such that the Galois generator acts as
   \((0\;1\;2\;3\;4)\).
3. __Lagrange resolvents__: compute \(R_j = \sum_{k=0}^{4} \omega_5^{jk} \alpha_k\)
   numerically, then \(R_j^5\).
4. __DFT coefficients__: compute \(d_s = \frac{1}{5} \sum_j \omega_5^{\-js} R_j^5\).
5. __Coefficient matching__: dispatch to 'matchDsToQ' (\(C_5\)),
   'matchDsD5' (\(D_5\)), or 'matchDsF20' (\(F_{20}\)) to express \(d_s\) as
   radical expressions over the appropriate coefficient field.
6. __Reconstruct \(R_j^5\)__: build \(R_j^5 = \sum_s d_s \omega_5^{js}\) as
   'RadExpr'.
7. __Branch selection__: compute \(R_j = \omega_5^{b_j} \sqrt[5]{R_j^5}\) via
   'selectBranch5'.
8. __Inverse DFT__: recover \(\alpha_k = \frac{1}{5} \sum_j \omega_5^{\-jk} R_j\).
9. __Un-depress__: shift back by \(a_4/5\) and match to original root ordering.
-}
solveSolvableQuintic :: TransitiveGroup -> Poly Rational -> [Complex Double] -> Maybe [RadExpr Rational]
solveSolvableQuintic tg f numRoots = do
    -- Make monic and depress
    let cs = unPoly f
        lc = last cs
        monicCs = map (/ lc) cs
        a4 = monicCs !! 4
        shiftVal = -(a4 / 5)
        depRoots = [r - (fromRational shiftVal :+ 0) | r <- numRoots]

    let groupName = tgName tg

    -- Find cyclic ordering
    ordering <- findCyclicOrdering depRoots 5 groupName

    let orderedRoots = [depRoots !! i | i <- ordering]

    -- Compute all R_j and R_j⁵ numerically
    let rj j = sum [omega5C (j * k) * (orderedRoots !! k) | k <- [0 .. 4]]
        rjVals = [rj j | j <- [0 .. 4]]
        rjPows = [r ^ (5 :: Int) | r <- rjVals]

    -- DFT: d_s = (1/5) Σ_j ω₅^{-js} R_j⁵
    let dVals =
            [ (1 / 5) * sum [omega5C (5 - (j * s) `mod` 5) * (rjPows !! j) | j <- [0 .. 4]]
            | s <- [0 .. 4]
            ]

    -- Match d_s to the coefficient field
    dExprs <- matchDs groupName dVals

    -- R_j⁵ = Σ_s d_s · ω₅^{js}
    let rjPowExprs =
            [ foldl1
                Add
                [Mul (dExprs !! s) (omegaPow5 ((j * s) `mod` 5)) | s <- [0 .. 4]]
            | j <- [1 .. 4]
            ]

    -- Select correct branch: R_j = ω₅^{b_j} · ⁵√(R_j⁵)
    let rjExprs =
            [ selectBranch5 (rjPowExprs !! (j - 1)) (rjVals !! j)
            | j <- [1 .. 4]
            ]

    -- R₀ = sum of depressed roots = 0
    let allR = Lit 0 : rjExprs

    -- Roots: α_k = (1/5) Σ_{j=0}^4 ω₅^{-jk} R_j
    let rootExprs =
            [ dagFold $
                Mul
                    (Inv (Lit 5))
                    ( foldl1
                        Add
                        [ Mul (omegaPow5 ((5 - (j * k) `mod` 5) `mod` 5)) (allR !! j)
                        | j <- [0 .. 4]
                        ]
                    )
            | k <- [0 .. 4]
            ]

    -- Un-depress
    let finalExprs = [dagFold (Add e (Lit shiftVal)) | e <- rootExprs]

    -- Match to original root ordering
    Just (matchToOriginal finalExprs numRoots)

------------------------------------------------------------------------
-- Finding the cyclic ordering
------------------------------------------------------------------------

{- | Find a cyclic ordering of the 5 numerical roots such that the
Galois generator \(\sigma\) acts as the cyclic shift \((0\;1\;2\;3\;4)\).

The algorithm fixes root 0 in position 0 and tries all \(4! = 24\)
permutations of the remaining 4 roots. Each candidate ordering is scored
by 'scoreOrdering', which measures how close the resulting DFT
coefficients' orbit symmetric functions are to rationals. The ordering
with the lowest score wins, subject to quality thresholds (absolute
score \(< 5\) and separation from the second-best candidate).

Returns 'Nothing' if no ordering passes the quality checks, which
indicates the roots may not have been computed with sufficient precision
or the group identification is incorrect.
-}
findCyclicOrdering ::
    [Complex Double] -> Int -> String -> Maybe [Int]
findCyclicOrdering roots 5 groupName = do
    let rest = [1 .. 4]
        orderings = [0 : p | p <- permutations rest]
        scored = [(o, scoreOrdering roots o groupName) | o <- orderings]
    case sortBy (comparing snd) scored of
        ((bestO, bestScore) : (_, secondScore) : _)
            | bestScore < 2 * fromIntegral (5 :: Int) && bestScore < 0.5 * secondScore -> Just bestO
            | bestScore < 5 -> Just bestO
        _ -> Nothing
findCyclicOrdering _ _ _ = Nothing

------------------------------------------------------------------------
-- Scoring (orbit-based)
------------------------------------------------------------------------

{- | Score a candidate ordering by measuring how close the DFT coefficients'
Galois-orbit symmetric functions are to rationals. Lower score = better fit.

The scoring strategy depends on the Galois group:

* __\(C_5\)__: All \(d_s\) should be individually rational. Score is
  \(\sum_s \operatorname{scoreRational}(d_s)\).

* __\(D_5\)__: The involution \(\tau = (1\;4)(2\;3)\) gives orbits
  \(\{d_0\}\), \(\{d_1, d_4\}\), \(\{d_2, d_3\}\). Score checks that
  \(d_0 \in \mathbb{Q}\) and that the symmetric functions
  \(d_1 + d_4\), \(d_1 d_4\), \(d_2 + d_3\), \(d_2 d_3\) are rational.

* __\(F_{20}\)__: The generator \(\tau = (1\;2\;4\;3)\) gives orbits
  \(\{d_0\}\), \(\{d_1, d_2, d_3, d_4\}\). Score checks that \(d_0 \in \mathbb{Q}\)
  and that the elementary symmetric polynomials \(e_1, e_2, e_3, e_4\) of
  \(\{d_1, d_2, d_3, d_4\}\) are rational.
-}
scoreOrdering :: [Complex Double] -> [Int] -> String -> Double
scoreOrdering roots ordering groupName =
    let ordered = [roots !! i | i <- ordering]
        rj j = sum [omega5C (j * k) * (ordered !! k) | k <- [0 .. 4]]
        rjPows = [rj j ^ (5 :: Int) | j <- [0 .. 4]]
        dVals =
            [ (1 / 5) * sum [omega5C (5 - (j * s) `mod` 5) * (rjPows !! j) | j <- [0 .. 4]]
            | s <- [0 .. 4]
            ]
        (d0, d1, d2, d3, d4) = toQuint dVals
     in case groupName of
            "C5" -> sum [scoreRational d | d <- dVals]
            "D5" ->
                -- τ = (14)(23) gives τ(d_s) = d_{-s mod 5}
                -- Orbits: {d_0}, {d_1, d_4}, {d_2, d_3}
                scoreRational d0
                    + scoreRational (d1 + d4)
                    + scoreRational (d1 * d4)
                    + scoreRational (d2 + d3)
                    + scoreRational (d2 * d3)
            "F20" ->
                -- τ = (1243) gives τ(d_s) = d_{2s mod 5}
                -- Orbits: {d_0}, {d_1, d_2, d_3, d_4}
                let ds = [d1, d2, d3, d4]
                    e1 = sum ds
                    e2 = d1 * d2 + d1 * d3 + d1 * d4 + d2 * d3 + d2 * d4 + d3 * d4
                    e3 = d1 * d2 * d3 + d1 * d2 * d4 + d1 * d3 * d4 + d2 * d3 * d4
                    e4 = d1 * d2 * d3 * d4
                 in scoreRational d0
                        + scoreRational e1
                        + scoreRational e2
                        + scoreRational e3
                        + scoreRational e4
            _ -> 1e10 -- unsupported

-- | How close is a complex number to a rational?  Returns the sum of the
-- absolute imaginary part and the fractional part of the real part.
-- A score near zero indicates the value is close to a real integer.
scoreRational :: Complex Double -> Double
scoreRational (re :+ im) = abs im + fracPart re

-- | Fractional part of a 'Double': the distance from the nearest integer.
fracPart :: Double -> Double
fracPart x = abs (x - fromIntegral (round x :: Integer))

------------------------------------------------------------------------
-- DFT coefficient matching
------------------------------------------------------------------------

{- | Match DFT coefficients \(d_s\) to radical expressions, dispatching on the
Galois group. Routes to 'matchDsToQ' for \(C_5\), 'matchDsD5' for \(D_5\),
or 'matchDsF20' for \(F_{20}\). Returns 'Nothing' for unsupported groups.
-}
matchDs :: String -> [Complex Double] -> Maybe [RadExpr Rational]
matchDs "C5" dVals = matchDsToQ dVals
matchDs "D5" dVals = matchDsD5 dVals
matchDs "F20" dVals = matchDsF20 dVals
matchDs _ _ = Nothing

{- | Match DFT coefficients to \(\mathbb{Q}\) (for \(C_5\) Galois group).

Every \(d_s\) must be (approximately) real and close to a rational number.
Each is matched via 'approxRat'. Returns 'Nothing' if any \(d_s\) has
imaginary part exceeding 0.01.
-}
matchDsToQ :: [Complex Double] -> Maybe [RadExpr Rational]
matchDsToQ = mapM matchRat
  where
    matchRat (re :+ im)
        | abs im > 0.01 = Nothing
        | otherwise = Just (Lit (approxRat re))

{- | Match DFT coefficients for \(D_5\) (dihedral group of order 10).

The conjugate pair structure gives:

* \(d_0 \in \mathbb{Q}\)
* \(\{d_1, d_4\}\) are roots of \(t^2 - s_1 t + p_1 = 0\) with
  \(s_1 = d_1 + d_4 \in \mathbb{Q}\), \(p_1 = d_1 d_4 \in \mathbb{Q}\)
* \(\{d_2, d_3\}\) are roots of \(t^2 - s_2 t + p_2 = 0\) with
  \(s_2 = d_2 + d_3 \in \mathbb{Q}\), \(p_2 = d_2 d_3 \in \mathbb{Q}\)

Each pair is expressed via the quadratic formula:
\(d = \frac{s \pm \sqrt{s^2 - 4p}}{2}\), with the sign chosen by numerical
comparison against the known \(d_s\) values.
-}
matchDsD5 :: [Complex Double] -> Maybe [RadExpr Rational]
matchDsD5 dVals = do
    let (d0, d1, d2, d3, d4) = toQuint dVals

    -- d_0 is rational
    d0Expr <- matchRatC d0

    -- {d_1, d_4}: t² - s₁t + p₁ = 0
    let s1C = d1 + d4
        p1C = d1 * d4
    s1 <- matchRatC s1C
    p1 <- matchRatC p1C

    -- {d_2, d_3}: t² - s₂t + p₂ = 0
    let s2C = d2 + d3
        p2C = d2 * d3
    s2 <- matchRatC s2C
    p2 <- matchRatC p2C

    -- Build radical expressions for d_1, d_4
    let s1R = approxRat (realPart s1C)
        p1R = approxRat (realPart p1C)
        disc1R = s1R * s1R - 4 * p1R
        disc1Expr = Sub (Mul s1 s1) (Mul (Lit 4) p1)
        -- d_1 = (s₁ ± √disc₁)/2, d_4 = (s₁ ∓ √disc₁)/2
        sqrtDisc1 = Root 2 disc1Expr
        d1Plus = Mul (Inv (Lit 2)) (Add s1 sqrtDisc1)
        d1Minus = Mul (Inv (Lit 2)) (Sub s1 sqrtDisc1)
    -- Determine which branch matches d_1 numerically
    let sqrtDisc1Val =
            if disc1R >= 0
                then sqrt (fromRational disc1R) :+ 0
                else 0 :+ sqrt (fromRational (abs disc1R))
        d1PlusVal = (s1C + sqrtDisc1Val) / 2
    (d1Expr, d4Expr) <-
        if magnitude (d1PlusVal - d1) < magnitude (d1PlusVal - d4)
            then Just (d1Plus, d1Minus)
            else Just (d1Minus, d1Plus)

    -- Build radical expressions for d_2, d_3
    let s2R = approxRat (realPart s2C)
        p2R = approxRat (realPart p2C)
        disc2R = s2R * s2R - 4 * p2R
        disc2Expr = Sub (Mul s2 s2) (Mul (Lit 4) p2)
        sqrtDisc2 = Root 2 disc2Expr
        d2Plus = Mul (Inv (Lit 2)) (Add s2 sqrtDisc2)
        d2Minus = Mul (Inv (Lit 2)) (Sub s2 sqrtDisc2)
    let sqrtDisc2Val =
            if disc2R >= 0
                then sqrt (fromRational disc2R) :+ 0
                else 0 :+ sqrt (fromRational (abs disc2R))
        d2PlusVal = (s2C + sqrtDisc2Val) / 2
    (d2Expr, d3Expr) <-
        if magnitude (d2PlusVal - d2) < magnitude (d2PlusVal - d3)
            then Just (d2Plus, d2Minus)
            else Just (d2Minus, d2Plus)

    Just [d0Expr, d1Expr, d2Expr, d3Expr, d4Expr]

{- | Match DFT coefficients for \(F_{20}\) (Frobenius group of order 20).

Here \(d_0 \in \mathbb{Q}\) and \(\{d_1, d_2, d_3, d_4\}\) form a single
Galois orbit in \(\mathbb{Q}(\omega_5)\). Each non-trivial \(d_s\) is
expressed as a \(\mathbb{Q}\)-linear combination of powers of \(\omega_5\)
via 'matchQOmega5'.
-}
matchDsF20 :: [Complex Double] -> Maybe [RadExpr Rational]
matchDsF20 dVals = do
    let (v0, v1, v2, v3, v4) = toQuint dVals
    d0Expr <- matchRatC v0
    d1Expr <- matchQOmega5 v1
    d2Expr <- matchQOmega5 v2
    d3Expr <- matchQOmega5 v3
    d4Expr <- matchQOmega5 v4
    Just [d0Expr, d1Expr, d2Expr, d3Expr, d4Expr]

{- | Match a complex number to a rational, returning 'Nothing' if the
imaginary part exceeds 0.1 in magnitude.
-}
matchRatC :: Complex Double -> Maybe (RadExpr Rational)
matchRatC (re :+ im)
    | abs im > 0.1 = Nothing
    | otherwise = Just (Lit (approxRat re))

{- | Match a complex number to an element of \(\mathbb{Q}(\omega_5)\).

Uses the integral basis \(\{1, \omega_5, \omega_5^2, \omega_5^3\}\)
(note: \(\omega_5^4 = -1 - \omega_5 - \omega_5^2 - \omega_5^3\) by the
minimal polynomial \(\Phi_5\)).

Tries progressively more general decompositions:

1. __Single term__: \(d = r \cdot \omega_5^k\) for rational \(r\) and
   \(0 \le k \le 4\) (via 'matchSingleOmega5').
2. __Two terms__: \(d = a + b \cdot \omega_5^k\) for rational \(a, b\)
   (via 'matchTwoTermOmega5').
3. __General__: \(d = a_0 + a_1 \omega_5 + a_2 \omega_5^2 + a_3 \omega_5^3\)
   via conjugate pair decomposition of real and imaginary parts
   (via 'matchGeneralQOmega5').
-}
matchQOmega5 :: Complex Double -> Maybe (RadExpr Rational)
matchQOmega5 d =
    -- Try single-term: d ≈ r · ω₅^k
    case matchSingleOmega5 d of
        Just e -> Just e
        Nothing ->
            -- Try two-term: d ≈ a + b · ω₅^k
            case matchTwoTermOmega5 d of
                Just e -> Just e
                Nothing ->
                    -- General Q(ω₅) decomposition via conjugate pairs
                    matchGeneralQOmega5 d

{- | Try \(d = r \cdot \omega_5^k\) for rational \(r\) and \(0 \le k \le 4\).
Divides \(d\) by each \(\omega_5^k\) and checks if the result is close to
a real rational (via 'scoreRational').
-}
matchSingleOmega5 :: Complex Double -> Maybe (RadExpr Rational)
matchSingleOmega5 d =
    let candidates =
            [ (k, d * omega5C (negate k)) -- d / ω₅^k = d · ω₅^{-k}
            | k <- [0 .. 4]
            ]
        scored = [(k, v, scoreRational v) | (k, v) <- candidates]
        (bestK, bestV, bestScore) = minimumBy (comparing (\(_, _, s) -> s)) scored
     in if bestScore < 0.01
            then
                let r = approxRat (realPart bestV)
                 in Just $
                        if bestK == 0
                            then Lit r
                            else Mul (Lit r) (omegaPow5 bestK)
            else Nothing

{- | Try \(d = a + b \cdot \omega_5^k\) for rational \(a, b\) and each
\(k \in \{1,2,3,4\}\). For each \(k\), estimates \(b \approx \operatorname{Re}(d \cdot \omega_5^{\-k})\),
rounds to a nearby rational, subtracts, and checks if the remainder is rational.
-}
matchTwoTermOmega5 :: Complex Double -> Maybe (RadExpr Rational)
matchTwoTermOmega5 d =
    let
        -- For each k, subtract a·ω₅^k and check if remainder is rational
        -- d = a + b·ω₅^k  →  (d - a)·ω₅^{-k} = b (rational)
        -- But we don't know a. Instead try:
        -- d - r·ω₅^k should be rational for some rational r and k
        tries =
            [ (k, r, d - (fromRational r :+ 0) * omega5C k)
            | k <- [1 .. 4]
            , r <- candidateCoeffs d k
            ]
        scored = [(k, r, rv, scoreRational rv) | (k, r, rv) <- tries]
        best = minimumBy (comparing (\(_, _, _, s) -> s)) scored
        (bestK, bestR, _, bestScore) = best
     in
        if bestScore < 0.01
            then
                let a = approxRat (realPart (d - (fromRational bestR :+ 0) * omega5C bestK))
                 in Just (Add (Lit a) (Mul (Lit bestR) (omegaPow5 bestK)))
            else Nothing
  where
    -- Generate candidate rational coefficients for b in d = a + b·ω₅^k
    candidateCoeffs dv k =
        let
            -- If d = a + bω, then im(d) = b·im(ω) → b = im(d)/im(ω)
            bApprox = realPart (dv * omega5C (negate k))
         in
            -- Try a few nearby rationals
            [approxRat bApprox]

{- | General \(\mathbb{Q}(\omega_5)\) decomposition:
\(d = a_0 + a_1 \omega_5 + a_2 \omega_5^2 + a_3 \omega_5^3\).

Uses the conjugate pair structure of \(\omega_5\) and \(\omega_5^4 = \bar{\omega}_5\):

\[
  \operatorname{Re}(d) = a_0 + a_1 \cos(2\pi/5) + (a_2 + a_3) \cos(4\pi/5)
\]
\[
  \operatorname{Im}(d) = a_1 \sin(2\pi/5) + (a_2 - a_3) \sin(4\pi/5)
\]

Searches over small-denominator rational candidates for \(a_1\) and
\(v = a_2 - a_3\), then solves for \(u = a_2 + a_3\) and \(a_0\) from the
real part equation. Selects the candidate with smallest reconstruction error.
-}
matchGeneralQOmega5 :: Complex Double -> Maybe (RadExpr Rational)
matchGeneralQOmega5 (re :+ im) =
    -- im = a₁·sin(2π/5) + v·sin(4π/5) where v = a₂ - a₃
    -- re = a₀ + a₁·cos(2π/5) + u·cos(4π/5) where u = a₂ + a₃
    -- We need 2 more constraints. Use: try a₁ from im/sin(2π/5) (if v=0),
    -- then check if remainder fits.
    let s1 = sin (2 * pi / 5) -- sin(2π/5)
        s2 = sin (4 * pi / 5) -- sin(4π/5)
        c1 = cos (2 * pi / 5) -- cos(2π/5)
        c2 = cos (4 * pi / 5) -- cos(4π/5)
        -- Try: for each candidate a₁ (from rounding im/s1 etc),
        -- compute v = (im - a₁·s1)/s2, then u and a₀
        -- This is underdetermined: for a given a₁, v is determined, but u is free.
        -- We need: u = a₂+a₃ where a₂,a₃ ∈ Q and v = a₂-a₃.
        -- So a₂ = (u+v)/2, a₃ = (u-v)/2, both must be rational.
        -- From the real equation: a₀ = re - a₁·c1 - u·c2
        -- a₀ must also be rational.
        -- With a₀ rational: u = (re - a₀ - a₁·c1)/c2. But a₀ is unknown.
        -- Strategy: try all small-denominator a₁, then v is determined, then
        -- for each small-denominator u (or equivalently a₂), check rationality of a₀, a₃.
        -- For efficiency, just try a few candidates.
        candidates = do
            -- Try a₁ = n/d for small denominators
            a1R <- candidateRats (im / s1)
            let a1 = fromRational a1R :: Double
                v = (im - a1 * s1) / s2
            vR <- candidateRats v
            -- u is free; determine from: a₀ = re - a₁·c1 - u·c2 must be rational
            uR <- candidateRats ((re - a1 * c1) / c2) ++ [0]
            let a0D = re - a1 * c1 - (fromRational uR :: Double) * c2
            a0R <- candidateRats a0D
            let a2R = (uR + vR) / 2
                a3R = (uR - vR) / 2
                -- Reconstruct and check
                recon = fromRational a0R + fromRational a1R * c1 + (fromRational a2R + fromRational a3R) * c2
                reconIm = fromRational a1R * s1 + (fromRational a2R - fromRational a3R) * s2
                err = abs (recon - re) + abs (reconIm - im)
            [(err, a0R, a1R, a2R, a3R)]
     in case candidates of
            [] -> Nothing
            _ ->
                let (err, a0R, a1R, a2R, a3R) = minimumBy (comparing (\(e, _, _, _, _) -> e)) candidates
                 in if err < 0.01
                        then Just (buildQOmega5Expr a0R a1R a2R a3R)
                        else Nothing
  where
    candidateRats x =
        let r = approxRat x
         in [r | abs (fromRational r - x) < 0.01]

{- | Build the radical expression \(a_0 + a_1 \omega_5 + a_2 \omega_5^2 + a_3 \omega_5^3\),
omitting terms with zero coefficient.
-}
buildQOmega5Expr :: Rational -> Rational -> Rational -> Rational -> RadExpr Rational
buildQOmega5Expr a0 a1 a2 a3 =
    let terms = [(a0, 0), (a1, 1), (a2, 2), (a3, 3)]
        nonzero = filter (\(c, _) -> c /= 0) terms
        mkTerm (c, 0) = Lit c
        mkTerm (c, k) = Mul (Lit c) (omegaPow5 k)
     in case map mkTerm nonzero of
            [] -> Lit 0
            [t] -> t
            (t : ts) -> foldl Add t ts

------------------------------------------------------------------------
-- ω₅ expressions
------------------------------------------------------------------------

{- | \(\omega_5 = e^{2\pi i/5}\) as a radical expression over \(\mathbb{Q}\).

Uses the explicit real and imaginary parts:

\[
  \cos(2\pi/5) = \frac{\sqrt{5} - 1}{4}, \qquad
  \sin(2\pi/5) = \frac{\sqrt{10 + 2\sqrt{5}}}{4}
\]

so \(\omega_5 = \cos(2\pi/5) + i \sin(2\pi/5)\) where \(i = \sqrt{\-1}\).
-}
omega5Expr :: RadExpr Rational
omega5Expr =
    let cos5 = Mul (Inv (Lit 4)) (Sub (Root 2 (Lit 5)) (Lit 1))
        sin5 = Mul (Inv (Lit 4)) (Root 2 (Add (Lit 10) (Mul (Lit 2) (Root 2 (Lit 5)))))
        i = Root 2 (Lit (-1))
     in Add cos5 (Mul i sin5)

-- | \(\omega_5^k\) as a radical expression, reduced modulo 5.
omegaPow5 :: Int -> RadExpr Rational
omegaPow5 0 = Lit 1
omegaPow5 1 = omega5Expr
omegaPow5 k = Pow omega5Expr (k `mod` 5)

-- | \(\omega_5^k = e^{2\pi i k/5}\) as 'Complex' 'Double', for numerical evaluation.
omega5C :: Int -> Complex Double
omega5C k = mkPolar 1 (2 * pi * fromIntegral k / 5)

------------------------------------------------------------------------
-- Branch selection for ⁵√
------------------------------------------------------------------------

{- | Select the correct branch of the 5th root of \(R_j^5\).

The principal 5th root \(\sqrt[5]{R_j^5}\) may differ from the true resolvent
\(R_j\) by a power of \(\omega_5\). This function evaluates all 5 candidates
\(\omega_5^k \cdot \sqrt[5]{R_j^5}\) for \(k \in \{0,\ldots,4\}\) numerically
and selects the one closest to the known numerical value @targetVal@.

Returns @Root 5 rj5Expr@ if \(k=0\), or @Mul (omegaPow5 k) (Root 5 rj5Expr)@
otherwise.
-}
selectBranch5 :: RadExpr Rational -> Complex Double -> RadExpr Rational
selectBranch5 rj5Expr targetVal =
    let
        -- Evaluate R_j⁵ numerically
        rj5Val = dagEvalC rj5Expr
        -- Principal 5th root
        principalVal =
            mkPolar
                (magnitude rj5Val ** 0.2)
                (phase rj5Val / 5)
        principalRoot = Root 5 rj5Expr
        -- Try all branches
        scored =
            [ (k, magnitude (omega5C k * principalVal - targetVal))
            | k <- [0 .. 4]
            ]
        bestK = fst $ minimumBy (comparing snd) scored
     in
        if bestK == 0
            then principalRoot
            else Mul (omegaPow5 bestK) principalRoot

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Evaluate a 'RadExpr' to 'Complex' 'Double' via the DAG evaluator.
dagEvalC :: RadExpr Rational -> Complex Double
dagEvalC = dagEvalComplex . toDAG

-- | Fold constants in a 'RadExpr' via the DAG constant-folding pass.
dagFold :: RadExpr Rational -> RadExpr Rational
dagFold = fromDAG . dagFoldConstants . toDAG

-- | Match radical expressions to original numerical roots by proximity.
matchToOriginal :: [RadExpr Rational] -> [Complex Double] -> [RadExpr Rational]
matchToOriginal exprs numRoots =
    let exprVals = [(e, dagEvalC e) | e <- exprs]
     in [ fst $ minimumBy (comparing (\(_, v) -> magnitude (v - t))) exprVals
        | t <- numRoots
        ]

{- | Approximate a 'Double' as a small-denominator rational (denominators up to 10000).
Searches for the fraction \(n/d\) minimising \(|n/d - x|\) subject to
error \(< 10^{\-6}\). Falls back to @round x@ if no fraction is found.
-}
approxRat :: Double -> Rational
approxRat x =
    let candidates =
            [ (abs err, n % d)
            | d <- [1 :: Integer .. 10000]
            , let n = round (x * fromIntegral d) :: Integer
            , let err = fromIntegral n / fromIntegral d - x
            , abs err < 1e-6
            ]
     in case candidates of
            [] -> toRational (round x :: Integer)
            _ -> snd (minimum candidates)

{- | Extract exactly 5 elements from a list into a 5-tuple.
Calls 'error' if the list has fewer than 5 elements.
-}
toQuint :: [a] -> (a, a, a, a, a)
toQuint (a : b : c : d : e : _) = (a, b, c, d, e)
toQuint _ = error "toQuint: expected at least 5 elements"

------------------------------------------------------------------------
-- Generalized degree-n solver
------------------------------------------------------------------------

{- | Solve a solvable polynomial of any prime degree via Lagrange
resolvent descent. Generalises 'solveViaTower' beyond degree 5.

For degree 5, delegates to the specialised quintic solver. For other
prime degrees, uses degree-\(n\) DFT with \(\omega_n = e^{2\pi i/n}\).
-}
solveViaTowerN :: GaloisResult -> Poly Rational -> Maybe [RadExpr Rational]
solveViaTowerN gr f
    | not (tgSolvable (grGroup gr)) = Nothing
    | degree f == 5 = solveViaTower gr f
    | otherwise = solveSolvablePrime (grGroup gr) f (grRoots gr)

{- | Core algorithm for solvable polynomials of prime degree \(n\).

Same 9-step pipeline as the quintic solver, generalised to degree \(n\):

1. Depress (eliminate the second-highest term).
2. Find cyclic ordering so Galois generator acts as the n-cycle.
3. Lagrange resolvents R_j.
4. DFT: compute d_s from R_j^n.
5. Coefficient matching: for cyclic groups all d_s are rational;
   for larger groups they satisfy polynomial relations.
6. Reconstruct R_j^n as 'RadExpr'.
7. Branch selection via numerical comparison.
8. Inverse DFT to recover the roots.
9. Un-depress and match to original root ordering.
-}
solveSolvablePrime :: TransitiveGroup -> Poly Rational -> [Complex Double] -> Maybe [RadExpr Rational]
solveSolvablePrime tg f numRoots = do
    let n = degree f
        nI = fromIntegral n :: Integer

    -- Step 1: Depress
    let cs = unPoly f
        lc = last cs
        monicCs = map (/ lc) cs
        an1 = monicCs !! (n - 1)
        shiftVal = -(an1 / fromIntegral n)
        depRoots = [r - (fromRational shiftVal :+ 0) | r <- numRoots]

    -- Step 2: Find cyclic ordering
    ordering <- findCyclicOrderingN depRoots n tg

    let orderedRoots = [depRoots !! i | i <- ordering]

    -- Step 3: Lagrange resolvents R_j = Σ_k ω_n^{jk} α_k
    let omC k = mkPolar 1 (2 * pi * fromIntegral k / fromIntegral n)
        rj j = sum [omC (j * k) * (orderedRoots !! k) | k <- [0 .. n - 1]]
        rjVals = [rj j | j <- [0 .. n - 1]]
        rjPows = [r ^ n | r <- rjVals]

    -- Step 4: DFT: d_s = (1/n) Σ_j ω_n^{-js} R_j^n
    let dVals =
            [ (1 / fromIntegral n) * sum [omC (n - (j * s) `mod` n) * (rjPows !! j) | j <- [0 .. n - 1]]
            | s <- [0 .. n - 1]
            ]

    -- Step 5: Match d_s to radical expressions
    dExprs <- matchDsGeneral tg dVals n

    -- Step 6: R_j^n = Σ_s d_s · ω_n^{js}
    let omExpr = omegaNExpr n
        omPow k = omegaPowNExpr n omExpr k
        rjPowExprs =
            [ foldl1 Add [Mul (dExprs !! s) (omPow ((j * s) `mod` n)) | s <- [0 .. n - 1]]
            | j <- [1 .. n - 1]
            ]

    -- Step 7: Branch selection: R_j = ω_n^{b_j} · ⁿ√(R_j^n)
    let rjExprs =
            [ selectBranchN n omExpr (rjPowExprs !! (j - 1)) (rjVals !! j)
            | j <- [1 .. n - 1]
            ]

    -- R₀ = sum of depressed roots = 0 (for monic depressed polynomial)
    let allR = Lit 0 : rjExprs

    -- Step 8: Inverse DFT: α_k = (1/n) Σ_j ω_n^{-jk} R_j
    let rootExprs =
            [ dagFold $
                Mul (Inv (Lit (fromIntegral nI)))
                    (foldl1 Add
                        [ Mul (omPow ((n - (j * k) `mod` n) `mod` n)) (allR !! j)
                        | j <- [0 .. n - 1]
                        ])
            | k <- [0 .. n - 1]
            ]

    -- Step 9: Un-depress
    let finalExprs = [dagFold (Add e (Lit shiftVal)) | e <- rootExprs]

    Just (matchToOriginal finalExprs numRoots)

------------------------------------------------------------------------
-- Generalized omega_n
------------------------------------------------------------------------

{- | \(\omega_n = e^{2\pi i/n}\) as a radical expression.
Uses @allPeriodsViaGauss@ to get \(\zeta_n = e^{2\pi i/n}\) directly
as a radical expression, avoiding the circular dependency through
@Surd.Trig@.
-}
omegaNExpr :: Int -> RadExpr Rational
omegaNExpr 5 = omega5Expr  -- fast path
omegaNExpr n =
    case allPeriodsViaGauss n of
        Just periods ->
            case Map.lookup 1 periods of
                Just zeta -> zeta  -- ζ_n = e^{2πi/n}
                Nothing -> fallbackOmega n
        Nothing -> fallbackOmega n

-- | Fallback: build ω_n = cos(2π/n) + i·sin(2π/n) from the half-angle
-- identity cos(2π/n) = (ζ + ζ⁻¹)/2, using allPeriodsViaGauss for
-- cos and sin separately.
fallbackOmega :: Int -> RadExpr Rational
fallbackOmega n =
    let theta = 2 * pi / fromIntegral n
        cosVal = cos theta
        sinVal = sin theta
        -- Build numerically approximate omega as a literal (last resort)
     in Add (Lit (approxRat cosVal)) (Mul (Root 2 (Lit (-1))) (Lit (approxRat sinVal)))

-- | \(\omega_n^k\) as a radical expression.
omegaPowNExpr :: Int -> RadExpr Rational -> Int -> RadExpr Rational
omegaPowNExpr n _ 0 = Lit 1
omegaPowNExpr n omExpr 1 = omExpr
omegaPowNExpr n omExpr k
    | k' == 0 = Lit 1
    | k' == 1 = omExpr
    | otherwise = Pow omExpr k'
  where
    k' = k `mod` n

------------------------------------------------------------------------
-- Generalized cyclic ordering
------------------------------------------------------------------------

{- | Find a cyclic ordering of \(n\) numerical roots for a general
prime-degree polynomial.

For prime \(n\), the Galois group contains a unique \(p\)-cycle (the
translation in \(\mathrm{AGL}(1,p)\)). We fix root 0 and try the
\(n-1\) possible rotations of the remaining roots (determined by
choosing which root is \(\sigma(\alpha_0)\)).

For \(n \le 8\), falls back to trying all \((n-1)!\) orderings.
-}
findCyclicOrderingN :: [Complex Double] -> Int -> TransitiveGroup -> Maybe [Int]
findCyclicOrderingN roots n tg
    | n == 5 = findCyclicOrdering roots 5 (tgName tg)
    | n <= 8 =
        -- For small n, brute-force all (n-1)! orderings
        let rest = [1 .. n - 1]
            orderings = [0 : p | p <- permutations rest]
            scored = [(o, scoreOrderingN roots o n tg) | o <- orderings]
         in case sortBy (comparing snd) scored of
                ((bestO, bestScore) : (_, secondScore) : _)
                    | bestScore < 5 * fromIntegral n && bestScore < 0.5 * secondScore -> Just bestO
                    | bestScore < fromIntegral n -> Just bestO
                _ -> Nothing
    | otherwise =
        -- For large n, try (n-1) rotations of a candidate p-cycle
        findCyclicOrderingByRotation roots n tg

{- | For large primes, find the cyclic ordering by testing which
assignment of root 1 (given root 0 is fixed) produces the best
DFT coefficient rationality score.

Since the Galois generator is a \(p\)-cycle, once we know
\(\alpha_1 = \sigma(\alpha_0)\), the entire ordering is determined:
\(\alpha_k = \sigma^k(\alpha_0)\).

We score each choice of \(\alpha_1\) among the \(n-1\) candidates by
checking whether the resulting DFT coefficients are approximately
rational.
-}
findCyclicOrderingByRotation :: [Complex Double] -> Int -> TransitiveGroup -> Maybe [Int]
findCyclicOrderingByRotation roots n tg = do
    -- For each candidate "next root" after root 0, build a full ordering
    -- by repeatedly applying: α_{k+1} = argmin|roots[j] - (σ applied to α_k)|
    -- where σ is determined by the pair (α_0, α_1).
    let candidates =
            [ (ordering, scoreOrderingN roots ordering n tg)
            | next <- [1 .. n - 1]
            , let ordering = buildOrdering roots n 0 next
            , length ordering == n  -- sanity check
            ]
    case sortBy (comparing snd) candidates of
        ((bestO, bestScore) : _)
            | bestScore < 5 * fromIntegral n -> Just bestO
        _ -> Nothing

{- | Build a cyclic ordering by choosing root 0 at position 0 and root
@next@ at position 1, then greedily assigning each subsequent position
to the nearest unassigned root that maintains the cyclic structure.

For a polynomial with Galois group containing a p-cycle σ, once we
fix α_0 and α_1 = σ(α_0), the "step" in the complex plane is
approximately constant (up to Galois orbit structure), so greedy
nearest-neighbour works well.
-}
buildOrdering :: [Complex Double] -> Int -> Int -> Int -> [Int]
buildOrdering roots n start next =
    let -- The "step vector" from root 0 to root 1 in the DFT sense
        -- isn't simply geometric, so use a different approach:
        -- Given α_0 and α_1, compute all R_j and R_j^n numerically.
        -- If the ordering is correct, R_j^n should have ~rational DFT coeffs.
        -- Instead of greedy, try all (n-1) cyclic permutations generated
        -- by the choice of "next".
        omC k = mkPolar 1 (2 * pi * fromIntegral k / fromIntegral n)

        -- Strategy: Fix root `start` at position 0, root `next` at position 1.
        -- Compute the implied "Galois action" as a permutation of roots by
        -- finding for each root the closest root to where σ would send it.
        -- σ(α_0) = α_1, so the "rotation" in root-index space is:
        -- we need to find the permutation σ of root indices such that
        -- σ(start) = next.

        -- Simple approach: build the ordering as a cycle starting from start
        -- going to next, then find the closest unused root to where the
        -- "same step" would take us.
        step = roots !! next - roots !! start
        go used pos acc
            | length acc >= n = reverse acc
            | otherwise =
                let target = roots !! pos + step
                    unused = [i | i <- [0 .. n - 1], i `notElem` used]
                 in case unused of
                    [] -> reverse acc
                    _ ->
                        let closest = minimumBy (comparing (\i -> magnitude (roots !! i - target))) unused
                         in go (closest : used) closest (closest : acc)
     in go [start, next] next [next, start]

{- | Score a candidate ordering for general degree \(n\).

For the cyclic group case (the most common for primes), all \(d_s\)
should be approximately rational. The score is the sum of rationality
scores over all \(d_s\).
-}
scoreOrderingN :: [Complex Double] -> [Int] -> Int -> TransitiveGroup -> Double
scoreOrderingN roots ordering n tg =
    let ordered = [roots !! i | i <- ordering]
        omC k = mkPolar 1 (2 * pi * fromIntegral k / fromIntegral n)
        rj j = sum [omC (j * k) * (ordered !! k) | k <- [0 .. n - 1]]
        rjPows = [rj j ^ n | j <- [0 .. n - 1]]
        dVals =
            [ (1 / fromIntegral n) * sum [omC (n - (j * s) `mod` n) * (rjPows !! j) | j <- [0 .. n - 1]]
            | s <- [0 .. n - 1]
            ]
        groupOrder = tgOrder tg
        p = fromIntegral n :: Integer
        d = groupOrder `div` p
     in if d == 1
            -- Cyclic: all d_s should be rational
            then sum [scoreRational dv | dv <- dVals]
            -- Dihedral: d_0 rational, conjugate pairs
            else if d == 2
            then scoreRational (head dVals) +
                 sum [scoreRational (dVals !! s + dVals !! (n - s))
                      + scoreRational (dVals !! s * dVals !! (n - s))
                     | s <- [1 .. n `div` 2]]
            -- General: check elementary symmetric functions of d_s orbits
            else sum [scoreRational dv | dv <- dVals]  -- simplified

------------------------------------------------------------------------
-- Generalized coefficient matching
------------------------------------------------------------------------

{- | Match DFT coefficients for a general solvable group at prime degree.

Dispatches based on the Galois group structure:

* For cyclic \(\mathbb{Z}/n\): all \(d_s \in \mathbb{Q}\).
* For dihedral \(D_n\) (\(d = 2\)): \(d_0 \in \mathbb{Q}\), conjugate pairs.
* For larger stabilisers: match via orbit structure of \((\mathbb{Z}/p\mathbb{Z})^{\ast}\).
-}
matchDsGeneral :: TransitiveGroup -> [Complex Double] -> Int -> Maybe [RadExpr Rational]
matchDsGeneral tg dVals n =
    let p = fromIntegral n :: Integer
        d = tgOrder tg `div` p
     in if d == 1
            then matchDsAllRational dVals
            else if d == 2
            then matchDsDihedral dVals n
            else matchDsViaOrbits tg dVals n

-- | All d_s must be rational (cyclic Galois group).
matchDsAllRational :: [Complex Double] -> Maybe [RadExpr Rational]
matchDsAllRational = mapM matchRatC'
  where
    matchRatC' (re :+ im)
        | abs im > 0.05 = Nothing
        | otherwise = Just (Lit (approxRat re))

{- | Dihedral group: \(d_0 \in \mathbb{Q}\), and for \(1 \le s \le (n-1)/2\),
\(\{d_s, d_{n-s}\}\) form conjugate pairs satisfying a quadratic over
\(\mathbb{Q}\).
-}
matchDsDihedral :: [Complex Double] -> Int -> Maybe [RadExpr Rational]
matchDsDihedral dVals n = do
    -- d_0 is rational
    d0Expr <- matchRatStrict (head dVals)

    -- Conjugate pairs: {d_s, d_{n-s}} for s = 1, ..., (n-1)/2
    let halfN = (n - 1) `div` 2
    pairExprs <- mapM (\s -> matchConjPair (dVals !! s) (dVals !! (n - s))) [1 .. halfN]

    -- Assemble: d_0, d_1, ..., d_{n-1}
    let result = replicate n (Lit 0)  -- placeholder
    -- Fill in: d_0
    let r0 = [d0Expr]
    -- Fill in pairs
    let rPairs = concatMap (\(s, (eS, eNS)) -> [(s, eS), (n - s, eNS)]) (zip [1..] pairExprs)
    let fillIn arr [] = arr
        fillIn arr ((idx, expr) : rest) =
            fillIn (take idx arr ++ [expr] ++ drop (idx + 1) arr) rest
     in Just (fillIn (d0Expr : tail result) rPairs)

matchRatStrict :: Complex Double -> Maybe (RadExpr Rational)
matchRatStrict (re :+ im)
    | abs im > 0.05 = Nothing
    | otherwise = Just (Lit (approxRat re))

-- | Match a conjugate pair {d_s, d_{n-s}} to quadratic expressions.
matchConjPair :: Complex Double -> Complex Double -> Maybe (RadExpr Rational, RadExpr Rational)
matchConjPair d1 d2 = do
    -- s = d1 + d2, p = d1 * d2 should be rational
    let s = d1 + d2
        p = d1 * d2
    sExpr <- matchRatStrict s
    pExpr <- matchRatStrict p
    let sR = approxRat (realPart s)
        pR = approxRat (realPart p)
        discR = sR * sR - 4 * pR
        discExpr = Sub (Mul sExpr sExpr) (Mul (Lit 4) pExpr)
        sqrtDisc = Root 2 discExpr
        ePlus = Mul (Inv (Lit 2)) (Add sExpr sqrtDisc)
        eMinus = Mul (Inv (Lit 2)) (Sub sExpr sqrtDisc)
    -- Determine which branch matches d1
    let sqrtDiscVal =
            if discR >= 0
                then sqrt (fromRational discR) :+ 0
                else 0 :+ sqrt (fromRational (abs discR))
        d1PlusVal = (s + sqrtDiscVal) / 2
    if magnitude (d1PlusVal - d1) < magnitude (d1PlusVal - d2)
        then Just (ePlus, eMinus)
        else Just (eMinus, ePlus)

{- | Match DFT coefficients via Galois orbit structure for larger
stabiliser groups.

For a group \(\mathbb{Z}/p \rtimes H\) with \(|H| = d\), the action of
\(H\) on \(\{d_1, \ldots, d_{n-1}\}\) partitions them into orbits.
The elementary symmetric polynomials of each orbit are rational.

For each orbit, the \(d_s\) values are roots of a polynomial over
\(\mathbb{Q}\) whose degree equals the orbit size. For orbit sizes
\(\le 4\), we use the quadratic/Cardano/Ferrari formulas.
-}
matchDsViaOrbits :: TransitiveGroup -> [Complex Double] -> Int -> Maybe [RadExpr Rational]
matchDsViaOrbits _tg dVals n = do
    -- Fallback: try to match all d_s as rational first
    -- (works when the group is large enough that d_s happen to be rational)
    case matchDsAllRational dVals of
        Just exprs -> Just exprs
        Nothing -> do
            -- Try: d_0 rational, rest matched via Q(omega_n)
            d0Expr <- matchRatStrict (head dVals)
            restExprs <- mapM (matchQOmegaN n) (tail dVals)
            Just (d0Expr : restExprs)

{- | Match a complex number to an element of \(\mathbb{Q}(\omega_n)\).

Uses the basis \(\{1, \omega_n, \omega_n^2, \ldots, \omega_n^{n-2}\}\)
(since \(\Phi_n(\omega_n) = 0\) gives a relation for \(\omega_n^{n-1}\)).

Tries progressively:
1. Single term: \(d = r \cdot \omega_n^k\).
2. General decomposition by solving the linear system numerically.
-}
matchQOmegaN :: Int -> Complex Double -> Maybe (RadExpr Rational)
matchQOmegaN n d = do
    -- Try single-term: d ≈ r · ω_n^k
    let omC k = mkPolar 1 (2 * pi * fromIntegral k / fromIntegral n)
        candidates =
            [ (k, d * omC (negate k), scoreRational (d * omC (negate k)))
            | k <- [0 .. n - 1]
            ]
        (bestK, bestV, bestScore) = minimumBy (comparing (\(_, _, s) -> s)) candidates
     in if bestScore < 0.01
            then
                let r = approxRat (realPart bestV)
                    omExpr = omegaNExpr n
                 in Just $ if bestK == 0
                        then Lit r
                        else Mul (Lit r) (omegaPowNExpr n omExpr bestK)
            else
                -- Try general decomposition: d = Σ a_k ω_n^k
                matchGeneralQOmegaN n d

{- | General decomposition: solve for \(a_0, \ldots, a_{n-2}\) in
\(d = \sum_{k=0}^{n-2} a_k \omega_n^k\) by setting up the linear system
from real and imaginary parts and solving via least squares.
-}
matchGeneralQOmegaN :: Int -> Complex Double -> Maybe (RadExpr Rational)
matchGeneralQOmegaN n d =
    -- Use the overconstrained system: Re(d) and Im(d) give 2 equations
    -- in n-1 unknowns. For small n this is underdetermined.
    -- Strategy: try all possible 2-term decompositions first.
    let omC k = mkPolar 1 (2 * pi * fromIntegral k / fromIntegral n)
        omExpr = omegaNExpr n
        -- Try 2-term: d = a · ω_n^j + b · ω_n^k
        twoTermCandidates =
            [ (err, j, aR, k, bR)
            | j <- [0 .. n - 1]
            , k <- [j + 1 .. n - 1]
            , let wj = omC j
                  wk = omC k
                  -- Solve: a·wj + b·wk = d
                  -- [Re(wj) Re(wk)] [a]   [Re(d)]
                  -- [Im(wj) Im(wk)] [b] = [Im(d)]
                  det = realPart wj * imagPart wk - imagPart wj * realPart wk
            , abs det > 1e-10
            , let a = (realPart d * imagPart wk - imagPart d * realPart wk) / det
                  b = (realPart wj * imagPart d - imagPart wj * realPart d) / det
                  aR = approxRat a
                  bR = approxRat b
                  recon = (fromRational aR :+ 0) * wj + (fromRational bR :+ 0) * wk
                  err = magnitude (recon - d)
            , err < 0.01
            ]
     in case sortBy (comparing (\(e, _, _, _, _) -> e)) twoTermCandidates of
            ((_, j, aR, k, bR) : _) ->
                let termJ = if j == 0 then Lit aR else Mul (Lit aR) (omegaPowNExpr n omExpr j)
                    termK = if k == 0 then Lit bR else Mul (Lit bR) (omegaPowNExpr n omExpr k)
                 in Just (Add termJ termK)
            [] -> Nothing  -- give up

------------------------------------------------------------------------
-- Generalized branch selection
------------------------------------------------------------------------

{- | Select the correct branch of the \(n\)-th root of \(R_j^n\).

Tries all \(n\) candidates \(\omega_n^k \cdot \sqrt[n]{R_j^n}\) and
picks the one closest to the known numerical value.
-}
selectBranchN :: Int -> RadExpr Rational -> RadExpr Rational -> Complex Double -> RadExpr Rational
selectBranchN n omExpr rjnExpr targetVal =
    let rjnVal = dagEvalC rjnExpr
        principalVal = mkPolar (magnitude rjnVal ** (1 / fromIntegral n)) (phase rjnVal / fromIntegral n)
        principalRoot = Root n rjnExpr
        omC k = mkPolar 1 (2 * pi * fromIntegral k / fromIntegral n)
        scored =
            [ (k, magnitude (omC k * principalVal - targetVal))
            | k <- [0 .. n - 1]
            ]
        bestK = fst $ minimumBy (comparing snd) scored
     in if bestK == 0
            then principalRoot
            else Mul (omegaPowNExpr n omExpr bestK) principalRoot
