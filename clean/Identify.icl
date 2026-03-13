implementation module Identify

import StdEnv
import Poly
import Rational
import Resolvent
import TransitiveGroup
import Data.Integer

identifyGaloisGroup5 :: !(Poly Rational) -> ?(GaloisResult)
identifyGaloisGroup5 f
    | degree f <> 5 = ?None
    # disc = discriminantOf f
    # discSq = isSquareRational disc
    # roots = complexRootsOf f
    # sexticResult = sexticResolvent5 roots
    = buildGaloisResult f sexticResult discSq roots

buildGaloisResult :: !(Poly Rational) !(?(Poly Rational)) !Bool ![(Real, Real)] -> ?(GaloisResult)
buildGaloisResult _ ?None _ _ = ?None
buildGaloisResult f (?Just sextic) discSq roots
    # hasSexticRoot = hasRationalRoot sextic
    # name = if (not hasSexticRoot && not discSq) "S5"
             (if (not hasSexticRoot && discSq) "A5"
             (if (hasSexticRoot && not discSq) "F20"
             (if (isCyclicByFrobenius f) "C5" "D5")))
    # group = hd [g \\ g <- transGroupsOfDegree 5 | g.tgName == name]
    = ?Just { grGroup = group, grRoots = roots }

// ─── Sextic resolvent ───
sexticResolvent5 :: ![(Real, Real)] -> ?(Poly Rational)
sexticResolvent5 roots
    # allPermsL = perms [0, 1, 2, 3, 4]
    # vals = [thetaFunc roots p \\ p <- allPermsL]
    # clusters = clusterByDist vals 0.0001
    | length clusters <> 6 = ?None
    # centers = [clusterCenter c \\ c <- clusters]
    = roundToRatPoly centers

thetaFunc :: ![(Real, Real)] ![Int] -> (Real, Real)
thetaFunc roots p
    # xs = [roots !! j \\ j <- p]
    = sumC [cmulR (cmulR (xs !! i) (xs !! i))
                  (cadd (cmulR (xs !! ((i+1) rem 5)) (xs !! ((i+4) rem 5)))
                        (cmulR (xs !! ((i+2) rem 5)) (xs !! ((i+3) rem 5))))
           \\ i <- [0..4]]

sumC :: ![(Real, Real)] -> (Real, Real)
sumC [] = (0.0, 0.0)
sumC [x:xs] = cadd x (sumC xs)

clusterByDist :: ![(Real, Real)] !Real -> [[(Real, Real)]]
clusterByDist [] _ = []
clusterByDist [v:vs] tol = goCluster [[v]] vs tol

goCluster :: ![[(Real, Real)]] ![(Real, Real)] !Real -> [[(Real, Real)]]
goCluster clusters [] _ = clusters
goCluster clusters [x:xs] tol
    = goCluster (insertIntoCluster x clusters 0 tol) xs tol

insertIntoCluster :: !(Real, Real) ![[(Real, Real)]] !Int !Real -> [[(Real, Real)]]
insertIntoCluster x clusters idx tol
    | idx >= length clusters = [[x] : clusters]
    # cl = clusters !! idx
    # minDist = minList [cmagR (csub x c) \\ c <- cl]
    | minDist < tol = [if (j == idx) [x:cl2] cl2 \\ cl2 <- clusters & j <- [0..]]
    = insertIntoCluster x clusters (idx + 1) tol

minList :: ![Real] -> Real
minList [x] = x
minList [x:xs] = min x (minList xs)
minList [] = 9999999999.0

clusterCenter :: ![(Real, Real)] -> (Real, Real)
clusterCenter cs
    # n = toReal (length cs)
    # (re, im) = foldl cadd (0.0, 0.0) cs
    = (re / n, im / n)

roundToRatPoly :: ![(Real, Real)] -> ?(Poly Rational)
roundToRatPoly roots
    # poly = foldl mulLin [(1.0, 0.0)] roots
    # cs = [roundC c \\ c <- poly]
    | any isNoneQ cs = ?None
    = ?Just (mkPoly [c \\ ?Just c <- cs])

mulLin :: ![(Real, Real)] !(Real, Real) -> [(Real, Real)]
mulLin acc r
    # shifted = [(0.0, 0.0) : acc]
    # scaled = [cmulR (~ (fst r), ~ (snd r)) a \\ a <- acc] ++ [(0.0, 0.0)]
    = [cadd a b \\ a <- shifted & b <- scaled]

roundC :: !(Real, Real) -> ?(Rational)
roundC (re, im)
    | abs im > 0.0001 * max 1.0 (abs re) = ?None
    = ?Just (approxRatR re)

isNoneQ :: !(?(Rational)) -> Bool
isNoneQ ?None = True
isNoneQ _ = False

approxRatR :: !Real -> Rational
approxRatR x
    # candidates = [(abs (toReal n / toReal d - x), mkRational (toInteger n) (toInteger d))
                    \\ d <- [1..500], let n = toInt (x * toReal d + if (x >= 0.0) 0.5 (~0.5))
                    | abs (toReal n / toReal d - x) < 0.000001]
    | isEmpty candidates = ratFromInt (toInt (if (x >= 0.0) (x + 0.5) (x - 0.5)))
    = snd (hd (sortBy (\(a,_) (b,_) -> a < b) candidates))

// ─── Frobenius test ───
isCyclicByFrobenius :: !(Poly Rational) -> Bool
isCyclicByFrobenius f
    # cs = getCoeffs f
    # lcmDen = foldl (\acc r -> lcmI acc (toInt (denom r))) 1 cs
    # intCs = [toInt (numer (r * ratFromInt lcmDen)) \\ r <- cs]
    # lc = last intCs
    # disc = discriminantOf f
    # discN = toInt (numer disc)
    # discD = toInt (denom disc)
    # testPs = take 20 [p \\ p <- smallPrimes | lc rem p <> 0 && discN rem p <> 0 && discD rem p <> 0]
    = not (any (hasNonCyclicPattern intCs) testPs)

getCoeffs :: !(Poly Rational) -> [Rational]
getCoeffs (Poly cs) = cs

lcmI :: !Int !Int -> Int
lcmI a b = abs (a * b / gcdI a b)

gcdI :: !Int !Int -> Int
gcdI a 0 = abs a
gcdI a b = gcdI b (a rem b)

hasNonCyclicPattern :: ![Int] !Int -> Bool
hasNonCyclicPattern intCs p
    # cs = [((c rem p) + p) rem p \\ c <- intCs]
    # pat = factorPattern cs p
    = pat <> [5] && pat <> [1, 1, 1, 1, 1]

factorPattern :: ![Int] !Int -> [Int]
factorPattern fcs p = goFP [] 1 (fpTrim fcs) [0, 1] p

goFP :: ![Int] !Int ![Int] ![Int] !Int -> [Int]
goFP degs _ f _ _ | fpDeg f <= 0 = sort degs
goFP degs k f h p
    # h` = fpPowMod h p f p
    # hx = fpSub h` [0, 1] p
    # g = fpGcd hx f p
    # gd = fpDeg g
    | gd == 0 = goFP degs (k + 1) f h` p
    # nf = gd / k
    # f` = fpDiv f g p
    = goFP (degs ++ repeatn nf k) (k + 1) f` h` p

// F_p polynomial arithmetic
fpTrim :: ![Int] -> [Int]
fpTrim cs = reverse (dropWhile (\x -> x == 0) (reverse cs))

fpDeg :: ![Int] -> Int
fpDeg cs = length (fpTrim cs) - 1

fpSub :: ![Int] ![Int] !Int -> [Int]
fpSub a b p
    # n = max (length a) (length b)
    # a` = a ++ repeatn (n - length a) 0
    # b` = b ++ repeatn (n - length b) 0
    = fpTrim [((x - y) rem p + p) rem p \\ x <- a` & y <- b`]

fpMul :: ![Int] ![Int] !Int -> [Int]
fpMul a b p
    | isEmpty a || isEmpty b = []
    # na = length a
    # nb = length b
    = fpTrim [sumMod [if (j >= 0 && j < na && (i-j) >= 0 && (i-j) < nb)
                      ((a !! j * b !! (i-j)) rem p) 0
                   \\ j <- [0..i]] p
              \\ i <- [0 .. na + nb - 2]]

sumMod :: ![Int] !Int -> Int
sumMod xs p = (foldl (+) 0 xs) rem p

fpMod :: ![Int] ![Int] !Int -> [Int]
fpMod a b p
    | fpDeg a < fpDeg b = fpTrim [((x rem p) + p) rem p \\ x <- a]
    | isEmpty (fpTrim b) = abort "fpMod: zero"
    # ta = fpTrim a
    # tb = fpTrim b
    # lcB = last tb
    # lcBInv = fpInv lcB p
    # lcA = last ta
    # fac = (lcA * lcBInv) rem p
    # da = fpDeg ta
    # db = fpDeg tb
    # shift = da - db
    # sub = [fpModHelper ta_i i shift tb fac p
             \\ (i, ta_i) <- zip2 [0..] (ta ++ repeatn (shift + length tb - length ta) 0)]
    = fpMod (fpTrim sub) tb p

fpModHelper :: !Int !Int !Int ![Int] !Int !Int -> Int
fpModHelper ta_i i shift tb fac p
    # bi = if (i >= shift && (i-shift) < length tb) (tb !! (i-shift)) 0
    = ((ta_i - fac * bi) rem p + p) rem p

fpDiv :: ![Int] ![Int] !Int -> [Int]
fpDiv a b p = goDiv [] (fpTrim a) (fpDeg b) (fpTrim b) (fpInv (last (fpTrim b)) p) p

goDiv :: ![Int] ![Int] !Int ![Int] !Int !Int -> [Int]
goDiv q r db tb lcBInv p
    | fpDeg r < db = fpTrim q
    # tr = fpTrim r
    # dr = fpDeg tr
    # shift = dr - db
    # fac = (last tr * lcBInv) rem p
    # q` = fpAdd q (repeatn shift 0 ++ [fac]) p
    # sub = [if (i >= shift && (i-shift) < length tb) ((fac * tb !! (i-shift)) rem p) 0
             \\ i <- [0 .. length tr - 1]]
    # r` = fpTrim [((x - y) rem p + p) rem p \\ x <- tr & y <- sub]
    = goDiv q` r` db tb lcBInv p

fpAdd :: ![Int] ![Int] !Int -> [Int]
fpAdd a b p
    # n = max (length a) (length b)
    # a` = a ++ repeatn (n - length a) 0
    # b` = b ++ repeatn (n - length b) 0
    = fpTrim [(x + y) rem p \\ x <- a` & y <- b`]

fpGcd :: ![Int] ![Int] !Int -> [Int]
fpGcd a b p
    | isEmpty (fpTrim b) || fpDeg b < 0 = fpMakeMonic (fpTrim a) p
    = fpGcd b (fpMod a b p) p

fpMakeMonic :: ![Int] !Int -> [Int]
fpMakeMonic [] _ = []
fpMakeMonic cs p
    # lc = last cs
    # lcInv = fpInv lc p
    = [(c * lcInv) rem p \\ c <- cs]

fpPowMod :: ![Int] !Int ![Int] !Int -> [Int]
fpPowMod base expo modulus p = goPM [1] base expo modulus p

goPM :: ![Int] ![Int] !Int ![Int] !Int -> [Int]
goPM res _ 0 _ _ = res
goPM res b e modulus p
    # res` = if (isOdd e) (fpMod (fpMul res b p) modulus p) res
    # b` = fpMod (fpMul b b p) modulus p
    = goPM res` b` (e / 2) modulus p

fpInv :: !Int !Int -> Int
fpInv a m
    # (_, x, _) = eGcd a m
    = ((x rem m) + m) rem m

eGcd :: !Int !Int -> (Int, Int, Int)
eGcd 0 b = (b, 0, 1)
eGcd a b
    # (g, x, y) = eGcd (b rem a) a
    = (g, y - (b / a) * x, x)

smallPrimes :: [Int]
smallPrimes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]
