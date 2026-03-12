{- | Galois group identification for irreducible polynomials over \(\mathbb{Q}\).

= Strategy

We use the /Stauduhar descent/ approach: start by assuming the Galois group
is the full symmetric group \(S_5\), then apply resolvent polynomials to
test membership in successively smaller subgroups, descending through the
transitive subgroup lattice of \(S_5\).

The degree-5 lattice has five transitive groups (Butler–McKay numbering):

\[
  S_5 \;\supset\; A_5, \quad
  S_5 \;\supset\; F_{20} \;\supset\; D_5 \;\supset\; C_5, \quad
  A_5 \;\supset\; D_5 \;\supset\; C_5
\]

= Resolvents

Two resolvent tests suffice to navigate the lattice:

1. __Discriminant resolvent.__  The discriminant \(\Delta(f) = \prod_{i<j}
   (\alpha_i - \alpha_j)^2\) is a perfect square in \(\mathbb{Q}\) if and
   only if \(\mathrm{Gal}(f) \le A_5\).

2. __Sextic resolvent.__  The \(F_{20}\)-invariant

   \[
     \theta(x_0, \ldots, x_4)
       = \sum_{i=0}^{4} x_i^2 \bigl(x_{i+1}\,x_{i+4} + x_{i+2}\,x_{i+3}\bigr)
   \]

   (indices mod 5) has stabiliser \(F_{20}\) in \(S_5\).  Since
   \(|S_5|/|F_{20}| = 120/20 = 6\), the orbit of \(\theta\) under \(S_5\)
   has exactly 6 elements, and the resolvent \(R_\theta(x)\) is a degree-6
   polynomial over \(\mathbb{Q}\).  The group satisfies
   \(\mathrm{Gal}(f) \le F_{20}\) if and only if \(R_\theta\) has a
   rational root.

When both tests place the group inside \(A_5 \cap F_{20} = D_5\), a
Frobenius\/Chebotarev test distinguishes \(C_5\) from \(D_5\).

= Decision tree

@
  disc square?  sextic root?   result
  ────────────  ────────────   ──────
  no            no             S₅
  yes           no             A₅
  no            yes            F₂₀
  yes           yes            D₅ or C₅  (Frobenius test)
@

= Frobenius\/Chebotarev test

For a cyclic group \(C_5\), the Frobenius element at an unramified prime
\(p\) is a power of the generator, so the factorisation pattern of \(f\)
mod \(p\) is either \(\{5\}\) (inert) or \(\{1,1,1,1,1\}\) (split).
The dihedral group \(D_5\) additionally admits the pattern \(\{1,2,2\}\)
(one fixed point, two 2-cycles).  Testing 20 good primes is sufficient
in practice to distinguish the two cases.

== References

* Stauduhar, R. P. (1973). \"The determination of Galois groups.\"
  /Math. Comp./ 27(124), 981–996.
  DOI: 10.1090\/S0025-5718-1973-0327712-4

* Dummit, D. S. (1991). \"Solving solvable quintics.\"
  /Math. Comp./ 57(195), 387–401.
  DOI: 10.1090\/S0025-5718-1991-1079014-X

* Soicher, L. H. & McKay, J. (1985). \"Computing Galois groups over
  the rationals.\"  /J. Number Theory/ 20(3), 273–281.
  DOI: 10.1016\/0022-314X(85)90022-8
-}
module Surd.Galois.Identify (
    identifyGaloisGroup5,
    GaloisResult (..),
)
where

import Data.Complex (Complex (..), magnitude)
import Data.List (minimumBy, sort)
import Data.Ord (comparing)
import Data.Ratio (denominator, numerator, (%))
import Math.Polynomial.Univariate
import Surd.Galois.Resolvent
import Surd.Galois.TransitiveGroup

-- | Result of Galois group identification.
data GaloisResult = GaloisResult
    { grGroup :: !TransitiveGroup
    -- ^ The identified transitive group (see "Surd.Galois.TransitiveGroup").
    , grRoots :: ![Complex Double]
    {- ^ Approximate complex roots of the polynomial, in the order used
    internally for resolvent evaluation.  These are computed via
    'complexRootsOf' and are not sorted in any canonical way.
    -}
    }
    deriving (Show)

{- | Identify the Galois group of a degree-5 polynomial over \(\mathbb{Q}\).

Returns 'Nothing' if the polynomial does not have degree 5 or if the
sextic resolvent cannot be constructed (e.g., numerical clustering fails).

__Precondition:__ the input polynomial should be irreducible over
\(\mathbb{Q}\).  Irreducibility is /not/ checked; passing a reducible
polynomial will produce meaningless results.

The decision procedure is:

1. Compute the discriminant \(\Delta(f)\) and test whether it is a
   perfect square in \(\mathbb{Q}\).
2. Compute the sextic resolvent \(R_\theta(x)\) from approximate roots
   and test for a rational root.
3. If both tests are positive (\(\mathrm{Gal}(f) \le D_5\)), apply
   'isCyclicByFrobenius' to distinguish \(C_5\) from \(D_5\).
-}
identifyGaloisGroup5 :: Poly Rational -> Maybe GaloisResult
identifyGaloisGroup5 f
    | degree f /= 5 = Nothing
    | otherwise = do
        let disc = discriminantOf f
            discSq = isSquareRational disc
            roots = complexRootsOf f
        sextic <- sexticResolvent5 roots
        let hasSexticRoot = hasRationalRoot sextic
            name
                | not hasSexticRoot && not discSq = "S5"
                | not hasSexticRoot && discSq = "A5"
                | hasSexticRoot && not discSq = "F20"
                | isCyclicByFrobenius f = "C5"
                | otherwise = "D5"
            group = case [g | g <- transGroupsOfDegree 5, tgName g == name] of
                (g : _) -> g
                [] -> error $ "identifyGaloisGroup5: unknown group " ++ name
        Just GaloisResult{grGroup = group, grRoots = roots}

------------------------------------------------------------------------
-- Sextic resolvent
------------------------------------------------------------------------

{- | Construct the sextic resolvent polynomial from approximate complex roots.

Evaluates the \(F_{20}\)-invariant

\[
  \theta(x_0, \ldots, x_4)
    = \sum_{i=0}^{4} x_i^2
      \bigl(x_{i+1}\,x_{i+4} + x_{i+2}\,x_{i+3}\bigr)
\]

at all \(120\) permutations of the five roots, clusters the resulting
values into 6 orbits (since \(|\mathrm{Stab}_{S_5}(\theta)| = |F_{20}| = 20\)),
and builds \(R_\theta(x) = \prod_{j=1}^{6}(x - v_j)\) where \(v_j\) are
the orbit centres.  The coefficients are rounded to rationals.

Returns 'Nothing' if clustering does not produce exactly 6 groups, which
indicates numerical instability (e.g., nearly-degenerate roots).
-}
sexticResolvent5 :: [Complex Double] -> Maybe (Poly Rational)
sexticResolvent5 roots = do
    let theta xs =
            sum
                [ xs !! i ^ (2 :: Int)
                    * ( xs !! ((i + 1) `mod` 5) * xs !! ((i + 4) `mod` 5)
                            + xs !! ((i + 2) `mod` 5) * xs !! ((i + 3) `mod` 5)
                      )
                | i <- [0 .. 4]
                ]
        allPerms = perms [0 .. 4]
        vals = [theta [roots !! j | j <- p] | p <- allPerms]
        clusters = cluster vals 1e-4
    if length clusters /= 6
        then Nothing
        else roundToRatPoly (map clusterCenter clusters)
  where
    -- Cluster by pairwise distance: greedy assignment to nearest center
    cluster = clusterByDistance

clusterCenter :: [Complex Double] -> Complex Double
clusterCenter cs = sum cs / fromIntegral (length cs)

{- | Cluster complex values by proximity.  Two values end up in the same
cluster if they are within @tol@ of at least one existing cluster member.
This is a greedy single-linkage clustering.
-}
clusterByDistance :: [Complex Double] -> Double -> [[Complex Double]]
clusterByDistance [] _ = []
clusterByDistance (v : vs) tol = go [[v]] vs
  where
    go clusters [] = clusters
    go clusters (x : xs) =
        case findCluster x clusters of
            Just i -> go (addToCluster i x clusters) xs
            Nothing -> go ([x] : clusters) xs

    findCluster x clusters =
        let dists = [(i, minimum [magnitude (x - c) | c <- cl]) | (i, cl) <- zip [0 :: Int ..] clusters]
            best = minimumBy (comparing snd) dists
         in if snd best < tol then Just (fst best) else Nothing

    addToCluster i x clusters =
        [if j == i then x : cl else cl | (j, cl) <- zip [0 :: Int ..] clusters]

-- | Build ∏(x - rᵢ) and round coefficients to rationals.
roundToRatPoly :: [Complex Double] -> Maybe (Poly Rational)
roundToRatPoly roots = do
    let poly = foldl mulLin [1 :+ 0] roots
    cs <- mapM roundC poly
    Just (mkPoly cs)
  where
    mulLin acc r =
        let shifted = (0 :+ 0) : acc
            scaled = map (negate r *) acc ++ [0 :+ 0]
         in zipWith (+) shifted scaled
    roundC (re :+ im)
        | abs im > 1e-4 * max 1 (abs re) = Nothing
        | otherwise = Just (approxRat re)

-- | Approximate a Double as a small-denominator rational.
approxRat :: Double -> Rational
approxRat x =
    let candidates =
            [ (abs (fromIntegral n / fromIntegral d - x), n % d)
            | d <- [1 :: Integer .. 10000]
            , let n = round (x * fromIntegral d) :: Integer
            , abs (fromIntegral n / fromIntegral d - x) < 1e-6
            ]
     in case candidates of
            [] -> toRational (round x :: Integer)
            _ -> snd (minimum candidates)

------------------------------------------------------------------------
-- Frobenius test for C₅ vs D₅
------------------------------------------------------------------------

{- | Frobenius\/Chebotarev test to distinguish \(C_5\) from \(D_5\).

For each unramified prime \(p\) (i.e., \(p\) does not divide the leading
coefficient or the discriminant), we compute the factorisation pattern of
\(f \bmod p\).  By the Chebotarev density theorem:

* \(C_5\): the Frobenius at \(p\) generates a cyclic subgroup, so the
  only possible patterns are \(\{5\}\) (order-5 element) and
  \(\{1,1,1,1,1\}\) (identity).

* \(D_5\): additionally admits the pattern \(\{1,2,2\}\), corresponding
  to a reflection (order-2 element with one fixed point).

If any of the first 20 good primes yields a pattern other than
\(\{5\}\) or \(\{1,1,1,1,1\}\), the group is \(D_5\); otherwise we
conclude \(C_5\).
-}
isCyclicByFrobenius :: Poly Rational -> Bool
isCyclicByFrobenius f =
    let cs = unPoly f
        lcmDen = foldl lcm 1 (map denominator cs)
        intCs = [numerator (c * fromIntegral lcmDen) | c <- cs]
        lc = last intCs
        disc = discriminantOf f
        discN = numerator disc
        discD = denominator disc
        goodPrime p =
            let p' = fromIntegral p
             in lc `mod` p' /= 0 && discN `mod` p' /= 0 && discD `mod` p' /= 0
        testPs = take 20 [p | p <- smallPrimes, goodPrime p]
     in not (any (hasNonCyclicPattern intCs) testPs)

hasNonCyclicPattern :: [Integer] -> Int -> Bool
hasNonCyclicPattern intCs p =
    let pI = fromIntegral p
        cs = map (\c -> ((c `mod` pI) + pI) `mod` pI) intCs
        pat = factorPattern cs pI
     in pat /= [5] && pat /= [1, 1, 1, 1, 1]

{- | Distinct-degree factorisation pattern of a polynomial over \(\mathbb{F}_p\).

For \(k = 1, 2, \ldots\), computes
\(\gcd\!\bigl(x^{p^k} - x,\; f(x)\bigr)\) in \(\mathbb{F}_p[x]\).
Each non-trivial GCD factor has degree divisible by \(k\) and contributes
\(\deg(g)/k\) irreducible factors of degree \(k\) to the pattern.
The result is the sorted list of factor degrees.
-}
factorPattern :: [Integer] -> Integer -> [Int]
factorPattern fcs p = go [] 1 (fpTrim fcs) [0, 1]
  where
    go degs _k f _h | fpDeg f <= 0 = sort degs
    go degs k f h =
        -- h ← h^p mod f (this computes x^{p^k} mod f)
        let h' = fpPowMod h p f p
            -- g = gcd(h' - x, f) mod p
            hx = fpSub h' [0, 1] p
            g = fpGcd hx f p
            gd = fpDeg g
         in if gd == 0
                then go degs (k + 1) f h'
                else
                    let nf = gd `div` k
                        f' = fpDiv f g p
                     in go (degs ++ replicate nf k) (k + 1) f' h'

------------------------------------------------------------------------
-- F_p polynomial arithmetic (ascending coefficient lists)
------------------------------------------------------------------------

{- $fp_arith
Polynomials over \(\mathbb{F}_p\) are represented as ascending
coefficient lists @[a_0, a_1, \ldots, a_d]@ with entries in
\(\{0, \ldots, p-1\}\).  Trailing zeros are trimmed by 'fpTrim'.
These are internal helpers used by 'factorPattern' for the
distinct-degree factorisation.
-}

fpTrim :: [Integer] -> [Integer]
fpTrim = reverse . dropWhile (== 0) . reverse

fpDeg :: [Integer] -> Int
fpDeg cs = length (fpTrim cs) - 1

fpAdd :: [Integer] -> [Integer] -> Integer -> [Integer]
fpAdd a b p =
    let n = max (length a) (length b)
        a' = a ++ replicate (n - length a) 0
        b' = b ++ replicate (n - length b) 0
     in fpTrim [(x + y) `mod` p | (x, y) <- zip a' b']

fpSub :: [Integer] -> [Integer] -> Integer -> [Integer]
fpSub a b p =
    let n = max (length a) (length b)
        a' = a ++ replicate (n - length a) 0
        b' = b ++ replicate (n - length b) 0
     in fpTrim [((x - y) `mod` p + p) `mod` p | (x, y) <- zip a' b']

fpMul :: [Integer] -> [Integer] -> Integer -> [Integer]
fpMul a b p
    | null a || null b = []
    | otherwise =
        let na = length a; nb = length b
         in fpTrim
                [ sum
                    [ if j >= 0 && j < na && (i - j) >= 0 && (i - j) < nb
                        then (a !! j * b !! (i - j)) `mod` p
                        else 0
                    | j <- [0 .. i]
                    ]
                    `mod` p
                | i <- [0 .. na + nb - 2]
                ]

fpMod :: [Integer] -> [Integer] -> Integer -> [Integer]
fpMod a b p
    | fpDeg a < fpDeg b = fpTrim (map (\x -> ((x `mod` p) + p) `mod` p) a)
    | null (fpTrim b) = error "fpMod: division by zero"
    | otherwise =
        let da = fpDeg a
            db = fpDeg b
            ta = fpTrim a
            tb = fpTrim b
            lcB = last tb
            lcBInv = fpInv lcB p
            shift = da - db
            lcA = last ta
            fac = (lcA * lcBInv) `mod` p
            sub =
                [ let bi =
                        if i >= shift && (i - shift) < length tb
                            then tb !! (i - shift)
                            else 0
                   in ((ta_i - fac * bi) `mod` p + p) `mod` p
                | (i, ta_i) <- zip [0 ..] (ta ++ replicate (shift + length tb - length ta) 0)
                ]
         in fpMod (fpTrim sub) tb p

fpDiv :: [Integer] -> [Integer] -> Integer -> [Integer]
fpDiv a b p = go [] (fpTrim a)
  where
    db = fpDeg b
    tb = fpTrim b
    lcBInv = fpInv (last tb) p
    go q r
        | fpDeg r < db = fpTrim q
        | otherwise =
            let dr = fpDeg r
                tr = fpTrim r
                shift = dr - db
                fac = (last tr * lcBInv) `mod` p
                q' = fpAdd q (replicate shift 0 ++ [fac]) p
                sub =
                    [ if i >= shift && (i - shift) < length tb
                        then (fac * tb !! (i - shift)) `mod` p
                        else 0
                    | i <- [0 .. length tr - 1]
                    ]
                r' = fpTrim (zipWith (\x y -> ((x - y) `mod` p + p) `mod` p) tr sub)
             in go q' r'

fpGcd :: [Integer] -> [Integer] -> Integer -> [Integer]
fpGcd a b p
    | null (fpTrim b) || fpDeg b < 0 = fpMakeMonic (fpTrim a) p
    | otherwise = fpGcd b (fpMod a b p) p

fpMakeMonic :: [Integer] -> Integer -> [Integer]
fpMakeMonic [] _ = []
fpMakeMonic cs p =
    let lc = last cs
        lcInv = fpInv lc p
     in map (\c -> (c * lcInv) `mod` p) cs

fpPowMod :: [Integer] -> Integer -> [Integer] -> Integer -> [Integer]
fpPowMod base expo modulus p = go [1] base expo
  where
    go res _ 0 = res
    go res b e =
        let res' = if odd e then fpMod (fpMul res b p) modulus p else res
            b' = fpMod (fpMul b b p) modulus p
         in go res' b' (e `div` 2)

fpInv :: Integer -> Integer -> Integer
fpInv a m = ((x `mod` m) + m) `mod` m
  where
    (_, x, _) = eGcd a m

eGcd :: Integer -> Integer -> (Integer, Integer, Integer)
eGcd 0 b = (b, 0, 1)
eGcd a b = let (g, x, y) = eGcd (b `mod` a) a in (g, y - (b `div` a) * x, x)

smallPrimes :: [Int]
smallPrimes =
    [ 3
    , 5
    , 7
    , 11
    , 13
    , 17
    , 19
    , 23
    , 29
    , 31
    , 37
    , 41
    , 43
    , 47
    , 53
    , 59
    , 61
    , 67
    , 71
    , 73
    , 79
    , 83
    , 89
    , 97
    , 101
    , 103
    , 107
    , 109
    , 113
    ]

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [x : rest | (x, ys) <- picks xs, rest <- perms ys]
  where
    picks [] = []
    picks (y : ys) = (y, ys) : [(z, y : zs) | (z, zs) <- picks ys]
