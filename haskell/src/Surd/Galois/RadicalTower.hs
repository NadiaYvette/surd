{- | Radical tower construction for solvable polynomials.

Given an irreducible polynomial f(x) ∈ Q[x] of degree n with solvable
Galois group G, construct radical expressions for its roots via
Lagrange resolvents descending through the composition series of G.

The algorithm generalises the Gauss period descent from Surd.Trig.Galois
to arbitrary solvable polynomials:

  1. Find a cyclic ordering of the roots compatible with G
  2. Compute Lagrange resolvents R_j = Σ ω^{jk} · α_k
  3. Compute R_j^n via DFT: d_s = (1/n) Σ_j ω^{\-js} R_j^n
  4. Match d_s to the current coefficient field (Q, Q(√Δ), Q(ω₅))
  5. Build radical expressions via inverse DFT + n-th roots

The Galois action on d_s determines the coefficient field:
  C₅:  all d_s ∈ Q
  D₅:  d_0 ∈ Q, {d_1,d_4} and {d_2,d_3} are conjugate pairs over Q
  F₂₀: d_0 ∈ Q, {d_1,d_2,d_3,d_4} form a single orbit over Q
-}
module Surd.Galois.RadicalTower (
    solveViaTower,
    findCyclicOrdering,
)
where

import Data.Complex (Complex (..), magnitude, mkPolar, phase, realPart)
import Data.List (minimumBy, permutations, sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Math.Polynomial.Univariate (Poly, degree, unPoly)
import Surd.Galois.Identify (GaloisResult (..))
import Surd.Galois.TransitiveGroup
import Surd.Radical.DAG (dagEvalComplex, dagFoldConstants, fromDAG, toDAG)
import Surd.Types

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

{- | Solve a solvable polynomial via Lagrange resolvent descent.

Returns radical expressions for all n roots (including complex ones,
in the order matching the numerical roots from 'GaloisResult').

Returns Nothing if the group is not solvable or degree unsupported.
-}
solveViaTower :: GaloisResult -> Poly Rational -> Maybe [RadExpr Rational]
solveViaTower gr f
    | not (tgSolvable (grGroup gr)) = Nothing
    | degree f /= 5 = Nothing
    | otherwise = solveSolvableQuintic (grGroup gr) f (grRoots gr)

------------------------------------------------------------------------
-- Solvable quintic solver (unified for C₅, D₅, F₂₀)
------------------------------------------------------------------------

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
Galois generator acts as the shift (0 1 2 3 4).

Fixes root 0 in position 0 and tries all 24 orderings of the remaining
4 roots. The correct ordering gives d_s values whose orbit symmetric
functions are closest to rationals.
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

{- | Score a candidate ordering. Lower = better.

For C₅:  check all d_s are individually rational.
For D₅:  check d_0 ∈ Q and symmetric functions of conjugate pairs
         {d_1,d_4}, {d_2,d_3} are rational.
For F₂₀: check d_0 ∈ Q and elementary symmetric functions of
         {d_1,d_2,d_3,d_4} are rational.
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

-- | How close is a complex number to a rational?
scoreRational :: Complex Double -> Double
scoreRational (re :+ im) = abs im + fracPart re

fracPart :: Double -> Double
fracPart x = abs (x - fromIntegral (round x :: Integer))

------------------------------------------------------------------------
-- DFT coefficient matching
------------------------------------------------------------------------

-- | Match d_s values to radical expressions, dispatching on group.
matchDs :: String -> [Complex Double] -> Maybe [RadExpr Rational]
matchDs "C5" dVals = matchDsToQ dVals
matchDs "D5" dVals = matchDsD5 dVals
matchDs "F20" dVals = matchDsF20 dVals
matchDs _ _ = Nothing

-- | Match d_s values to Q (for C₅).
matchDsToQ :: [Complex Double] -> Maybe [RadExpr Rational]
matchDsToQ = mapM matchRat
  where
    matchRat (re :+ im)
        | abs im > 0.01 = Nothing
        | otherwise = Just (Lit (approxRat re))

{- | Match d_s values for D₅.

d_0 ∈ Q, {d_1, d_4} are roots of t² - s₁t + p₁ = 0,
{d_2, d_3} are roots of t² - s₂t + p₂ = 0, with rational s, p.
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

{- | Match d_s values for F₂₀.

d_0 ∈ Q, {d_1,d_2,d_3,d_4} form a Galois orbit in Q(ω₅).
Express each d_s as a Q-linear combination of ω₅ powers.
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

-- | Match a complex number to a rational, checking both re and im.
matchRatC :: Complex Double -> Maybe (RadExpr Rational)
matchRatC (re :+ im)
    | abs im > 0.1 = Nothing
    | otherwise = Just (Lit (approxRat re))

{- | Match a complex number to an element of Q(ω₅).

Tries progressively general forms:
  1. r · ω₅^k  (single term)
  2. a + b · ω₅^k  (two terms)
  3. General: a₀ + a₁ω₅ + a₂ω₅² + a₃ω₅³  (via conjugate pair decomposition)
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

-- | Try d = r · ω₅^k for rational r, integer k.
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

-- | Try d = a + b · ω₅^k for rational a, b and each k.
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

{- | General Q(ω₅) decomposition: d = a₀ + a₁ω₅ + a₂ω₅² + a₃ω₅³.

Uses the conjugate pair structure:
  re(d) = a₀ + a₁·cos(2π/5) + (a₂+a₃)·cos(4π/5)
  im(d) = a₁·sin(2π/5) + (a₂-a₃)·sin(4π/5)
Solve for a₁ and (a₂-a₃) from the imaginary part, then recover a₀, a₂, a₃.
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

-- | Build expression a₀ + a₁ω₅ + a₂ω₅² + a₃ω₅³.
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

{- | ω₅ = e^{2πi/5} as a radical expression.
cos(2π/5) = (√5 - 1)/4, sin(2π/5) = √(10 + 2√5)/4
-}
omega5Expr :: RadExpr Rational
omega5Expr =
    let cos5 = Mul (Inv (Lit 4)) (Sub (Root 2 (Lit 5)) (Lit 1))
        sin5 = Mul (Inv (Lit 4)) (Root 2 (Add (Lit 10) (Mul (Lit 2) (Root 2 (Lit 5)))))
        i = Root 2 (Lit (-1))
     in Add cos5 (Mul i sin5)

-- | ω₅^k as a radical expression.
omegaPow5 :: Int -> RadExpr Rational
omegaPow5 0 = Lit 1
omegaPow5 1 = omega5Expr
omegaPow5 k = Pow omega5Expr (k `mod` 5)

-- | ω₅^k as Complex Double.
omega5C :: Int -> Complex Double
omega5C k = mkPolar 1 (2 * pi * fromIntegral k / 5)

------------------------------------------------------------------------
-- Branch selection for ⁵√
------------------------------------------------------------------------

-- | Select the correct branch of the 5th root.
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

dagEvalC :: RadExpr Rational -> Complex Double
dagEvalC = dagEvalComplex . toDAG

dagFold :: RadExpr Rational -> RadExpr Rational
dagFold = fromDAG . dagFoldConstants . toDAG

-- | Match radical expressions to original numerical roots by proximity.
matchToOriginal :: [RadExpr Rational] -> [Complex Double] -> [RadExpr Rational]
matchToOriginal exprs numRoots =
    let exprVals = [(e, dagEvalC e) | e <- exprs]
     in [ fst $ minimumBy (comparing (\(_, v) -> magnitude (v - t))) exprVals
        | t <- numRoots
        ]

-- | Approximate a Double as a small-denominator rational.
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

-- | Extract exactly 5 elements from a list into a tuple.
toQuint :: [a] -> (a, a, a, a, a)
toQuint (a : b : c : d : e : _) = (a, b, c, d, e)
toQuint _ = error "toQuint: expected at least 5 elements"
