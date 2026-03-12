{- | Galois group identification for irreducible polynomials over Q.

For degree 5, the decision tree:

@
  disc square?  sextic root?   result
  ────────────  ────────────   ──────
  no            no             S₅
  yes           no             A₅
  no            yes            F₂₀
  yes           yes            D₅ or C₅ (Frobenius test)
@
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

data GaloisResult = GaloisResult
    { grGroup :: !TransitiveGroup
    , grRoots :: ![Complex Double]
    }
    deriving (Show)

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

{- | The F₂₀-invariant θ(x₀,...,x₄) = Σᵢ xᵢ²(x_{i+1}x_{i+4}+x_{i+2}x_{i+3}).
Its stabiliser in S₅ is F₂₀ (order 20), yielding 6 orbit values.
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

{- | Cluster complex values by proximity. Two values in the same cluster
are within tol of at least one other member.
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

{- | Factor f mod p for small primes. C₅ gives only patterns {5} and
{1,1,1,1,1}. D₅ also admits {1,2,2}. If any prime yields a non-C₅
pattern, the group is D₅.
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

{- | Factorization degree pattern of a polynomial mod p via distinct-degree
factorization: compute gcd(x^{p^k} - x, f) for k = 1, 2, ...
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
