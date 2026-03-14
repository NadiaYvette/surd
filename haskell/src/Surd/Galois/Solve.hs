{- |
Module      : Surd.Galois.Solve
Description : Top-level polynomial solving interface via Galois group computation
Stability   : experimental

Top-level interface for solving polynomials via Galois group computation.

= Position in the library

This module is the user-facing entry point for radical solving of
polynomials of degree \(\ge 5\). It composes three internal modules:

  * "Surd.Galois.Identify" — identifies the Galois group of an
    irreducible polynomial (currently degree 5 only).
  * "Surd.Galois.RadicalTower" — constructs an explicit radical tower
    and solves via Lagrange resolvents once a solvable group is known.
  * "Surd.Algebraic.Convert" — handles degrees 1–4 via the quadratic
    formula, Cardano's method (degree 3), and Ferrari's method (degree 4).

For degree \(\le 4\), 'solveAlgNum' returns 'Nothing' so that the caller
(typically 'Surd.Algebraic.Convert.algNumToRadExpr') can use its own
direct formulas. For degree 5, the Galois pipeline is invoked.

= Degree routing

@
  degree \(\le 4\)  →  Nothing  (caller delegates to Cardano\/Ferrari)
  degree = 5     →  identify Galois group; if solvable, solve via radical tower
  degree > 5     →  Nothing  (not yet supported)
@

= Abel–Ruffini and solvability by radicals

The Abel–Ruffini theorem (1824) states that the general polynomial of
degree \(\ge 5\) cannot be solved by radicals — there is no formula
analogous to the quadratic formula that works for /every/ quintic.
However, /specific/ quintics whose Galois group is a solvable group
(a proper subgroup of the symmetric group \(S_5\)) /can/ be expressed
in radicals.

The Galois-theoretic criterion is: a polynomial \(f \in \mathbb{Q}[x]\)
is solvable by radicals if and only if its Galois group
\(\operatorname{Gal}(f)\) is a solvable group — that is, it admits a
subnormal series whose successive quotients are all cyclic. For degree 5,
the solvable transitive subgroups of \(S_5\) are \(\mathbb{Z}/5\),
\(D_5\) (dihedral), and \(F_{20}\) (Frobenius). The non-solvable
groups \(A_5\) and \(S_5\) yield polynomials with no radical solution.

= Pipeline

  1. Check if the minimal polynomial has degree > 4 (otherwise return
     'Nothing' to let the caller use Cardano\/Ferrari).
  2. Identify the Galois group via 'identifyGaloisGroup5'.
  3. If the group is solvable, construct radical expressions for all
     roots via 'solveViaTower' using Lagrange resolvents.
  4. For 'solveAlgNum': match the target algebraic number against the
     computed roots by numerical proximity (approximate to 15 decimal
     digits, pick the closest radical root).

= References

  * Dummit, D. S. (1991). \"Solving solvable quintics.\"
    /Math. Comp./ 57(195), 387–401.
    DOI: 10.1090\/S0025-5718-1991-1079014-X
  * Cox, D. A. (2012). /Galois Theory/, 2nd ed. Wiley.
    DOI: 10.1002\/9781118218457
  * Abel, N. H. (1824). \"Mémoire sur les équations algébriques où
    l'on démontre l'impossibilité de la résolution de l'équation
    générale du cinquième degré.\" (Historical reference)
-}
module Surd.Galois.Solve (
    -- * Algebraic number interface
    solveAlgNum,

    -- * Polynomial interface
    solvePoly,
    identifyAndSolve,
)
where

import Data.Complex (Complex (..), imagPart, magnitude, realPart)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Math.Polynomial.Univariate (Poly, degree)
import Surd.Algebraic.Number (AlgNum (..), algApprox)
import Surd.Galois.Identify (GaloisResult (..), identifyGaloisGroup, identifyGaloisGroup5)
import Surd.Galois.RadicalTower (solveViaTower, solveViaTowerN)
import Surd.Galois.TransitiveGroup (tgName, tgSolvable)
import Surd.Radical.DAG (dagEvalComplex, toDAG)
import Surd.Types

{- | Try to express an algebraic number as a radical expression.

This extends 'Surd.Algebraic.Convert.algNumToRadExpr' to handle
irreducible polynomials of degree > 4. The strategy:

  1. Compute all radical roots of the minimal polynomial via 'solvePoly'.
  2. Approximate the target algebraic number to 15 significant digits
     using 'algApprox'.
  3. Pick the radical root whose numerical evaluation is closest to
     the approximation (see 'pickClosestReal').

Returns 'Nothing' in the following cases:

  * __Degree \(\le 4\)__: returns 'Nothing' by design, so that the
    caller can delegate to the direct Cardano\/Ferrari path in
    "Surd.Algebraic.Convert", which produces simpler expressions.
  * __Degree > 5__: not yet supported.
  * __Non-solvable Galois group__ (\(A_5\) or \(S_5\)): the polynomial
    has no radical solution (Abel–Ruffini).
  * __Numerical matching failure__: no computed root is within tolerance
    of the target value.
-}
solveAlgNum :: AlgNum -> Maybe (RadExpr Rational)
solveAlgNum a = do
    let p = anMinPoly a
        d = degree p
    if d <= 4
        then Nothing -- delegate to existing algNumToRadExpr
        else do
            -- Get all radical root expressions
            allRoots <- solvePoly p
            -- Pick the one matching the isolating interval
            let approxVal = fromRational (algApprox (1 / (10 ^ (15 :: Int))) a) :: Double
            pickClosestReal allRoots approxVal

{- | Solve an irreducible polynomial, returning radical expressions for all
roots.

This is a thin wrapper around 'identifyAndSolve' that discards the
group name and returns only the list of radical root expressions.
Returns 'Nothing' if the polynomial is unsupported (degree \(\ne 5\))
or has a non-solvable Galois group.
-}
solvePoly :: Poly Rational -> Maybe [RadExpr Rational]
solvePoly f = do
    result <- identifyAndSolve f
    Just (snd result)

{- | Identify the Galois group and solve if solvable.

Returns a pair @(groupName, roots)@ where @groupName@ is the name of
the transitive group (e.g. @\"Z5\"@, @\"D5\"@, @\"F20\"@) and @roots@
is the list of all radical root expressions. Currently restricted to
degree 5 polynomials; returns 'Nothing' for other degrees.

The group identification is performed by 'identifyGaloisGroup5', and
solvability is checked via 'tgSolvable'. If the group is solvable,
'solveViaTower' constructs the radical tower and returns the roots.
-}
identifyAndSolve :: Poly Rational -> Maybe (String, [RadExpr Rational])
identifyAndSolve f
    | degree f <= 4 = Nothing
    | otherwise = do
        gr <- identifyGaloisGroup f
        let tg = grGroup gr
        if not (tgSolvable tg)
            then Nothing
            else do
                roots <- solveViaTowerN gr f
                Just (tgName tg, roots)

{- | Pick the radical expression whose numerical value is closest to
the target real value.

Scores each candidate expression using 'evalDist' and selects the
one with the smallest distance. A two-tier strategy is used:

  * __Tight match__: if any expression is within @1e-4@ of the target,
    the closest among those is returned.
  * __Loose fallback__: if no tight match exists (e.g. all roots are
    complex), the overall closest is returned provided its distance
    is less than @1.0@.
  * If even the loose fallback exceeds @1.0@, returns 'Nothing'.
-}
pickClosestReal :: [RadExpr Rational] -> Double -> Maybe (RadExpr Rational)
pickClosestReal exprs target =
    let scored = [(e, evalDist e target) | e <- exprs]
        valid = [(e, d) | (e, d) <- scored, d < 1e-4]
     in case valid of
            [] ->
                -- All expressions might be complex; try anyway
                let best = minimumBy (comparing snd) scored
                 in if snd best < 1.0 then Just (fst best) else Nothing
            _ -> Just (fst $ minimumBy (comparing snd) valid)

{- | Compute the distance between a radical expression's numerical value
and a target real value.

If the expression evaluates to an essentially real number (imaginary
part below @1e-6@), returns the absolute difference of real parts.
Otherwise, returns the complex magnitude of the difference, which
penalises complex roots appropriately when matching against a real
algebraic number.
-}
evalDist :: RadExpr Rational -> Double -> Double
evalDist e target =
    let v = dagEvalComplex (toDAG e)
     in if abs (imagPart v) < 1e-6
            then abs (realPart v - target)
            else magnitude (v - (target :+ 0))
