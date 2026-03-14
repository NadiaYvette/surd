--- Galois group identification for irreducible polynomials over Q.
---
--- For degree 5, uses the classical decision tree via discriminant and
--- sextic resolvent. For general prime degree p, uses generalized
--- Frobenius/Chebotarev descent through the AGL(1,p) lattice.
---
--- Decision tree for degree 5:
---   disc square?  sextic root?   result
---   no            no             S5
---   yes           no             A5
---   no            yes            F20
---   yes           yes            D5 or C5 (Frobenius test)
module Identify
  ( GaloisResult(..)
  , identifyGaloisGroup5
  , identifyGaloisGroup
  , showGaloisResult
  ) where

import Rational
import Poly
import Resolvent (discriminant, sexticResolvent, hasRationalRoot,
                  isDiscSquare)
import TransitiveGroup (TransitiveGroup, tgC5, tgD5, tgF20, tgA5, tgS5,
                         tgName, tgOrder, tgDegree, tgSolvable,
                         allTransitiveGroups, showTransitiveGroup)
import PrimeFactors (isPrime)

--- Result of Galois group identification.
data GaloisResult
  = Identified TransitiveGroup
  | NotSupported String

--- Identify the Galois group of a degree-5 irreducible polynomial.
identifyGaloisGroup5 :: Poly -> GaloisResult
identifyGaloisGroup5 p
  | degree p /= 5
  = NotSupported "only degree 5 supported"
  | not (isDiscSquare p) && not (hasRationalRoot (sexticResolvent p))
  = Identified tgS5
  | isDiscSquare p && not (hasRationalRoot (sexticResolvent p))
  = Identified tgA5
  | not (isDiscSquare p) && hasRationalRoot (sexticResolvent p)
  = Identified tgF20
  | isDiscSquare p && hasRationalRoot (sexticResolvent p) && frobeniusTestC5 p
  = Identified tgC5
  | isDiscSquare p && hasRationalRoot (sexticResolvent p)
  = Identified tgD5

--- Identify the Galois group of an irreducible polynomial of any
--- supported degree.
---
--- For degree 5, delegates to identifyGaloisGroup5.
--- For other prime degrees, uses generalized Frobenius/Chebotarev
--- descent through AGL(1,p).
--- Returns NotSupported for unsupported composite degrees.
identifyGaloisGroup :: Poly -> GaloisResult
identifyGaloisGroup p
  | degree p == 5 = identifyGaloisGroup5 p
  | degree p >= 3 && isPrime (degree p) = identifyGaloisGroupPrime p
  | degree p <= 4 = NotSupported "degree <= 4 handled by direct formulas"
  | otherwise = NotSupported "composite degree not yet supported"

------------------------------------------------------------------------
-- General prime-degree identification
------------------------------------------------------------------------

--- Identify the Galois group for an irreducible prime-degree polynomial.
---
--- Strategy:
---   1. Check if inside AGL(1,p) by examining factorisation patterns
---      modulo small primes (Frobenius/Chebotarev density theorem).
---   2. If inside AGL(1,p), determine the minimal stabiliser order d
---      from the observed cycle lengths.
---   3. If not inside AGL(1,p), distinguish A_p from S_p via discriminant.
identifyGaloisGroupPrime :: Poly -> GaloisResult
identifyGaloisGroupPrime p =
  let n = degree p
      groups = allTransitiveGroups n
      disc = discriminant p
      discSq = isRatPerfectSquare disc
      -- Get integer coefficients for mod-p factorisation
      cs = polyCoeffs p
      lcmDen = foldl lcmInt 1 (map denominator cs)
      intCs = map (\c -> numerator (ratMul c (Rational.fromInt lcmDen))) cs
      lc = lastOf intCs
      discN = numerator disc
      discD = denominator disc
      goodPrime pr =
        lc `mod` pr /= 0 && discN `mod` pr /= 0 && discD `mod` pr /= 0
      testPrimes = take 50 (filter goodPrime (filter (\pr -> pr > n) primesList))
      -- Collect factorisation patterns
      patterns = map (factorizationPatternPrime intCs n) testPrimes
      -- Check if all patterns are consistent with AGL(1,p)
      isAGLPattern pat =
        pat == [n]                              -- translation: [p]
        || pat == replicate n 1                 -- identity: [1,...,1]
        || (lengthOf pat >= 2                   -- non-translation: [1, k, k, ...]
            && minimumOf pat == 1
            && lengthOf (filter (== 1) pat) == 1
            && let ks = filter (/= 1) pat
               in allSame ks)
      insideAGL = all isAGLPattern patterns
      -- Find the minimum d: lcm of all observed non-trivial cycle lengths
      nonTrivCycleLengths =
        [ k
        | pat <- patterns
        , pat /= [n]
        , pat /= replicate n 1
        , k <- pat
        , k /= 1
        ]
      minD = if null nonTrivCycleLengths
             then 1
             else foldl lcmInt 1 nonTrivCycleLengths
  in if not insideAGL
     then -- Not in AGL(1,p): either A_p or S_p
       if discSq
       then case [g | g <- groups, tgName g == "A" ++ show n] of
              (g:_) -> Identified g
              []    -> Identified (lastOf groups)
       else Identified (lastOf groups)  -- S_p
     else -- Inside AGL(1,p): find the smallest group with d >= minD
       let solvableDivs = sortedList [tgOrder g `div` n | g <- groups, tgSolvable g]
           groupD = case filter (>= minD) solvableDivs of
                      (d:_) -> d
                      []    -> n - 1  -- fallback to full AGL(1,p)
       in case [g | g <- groups, tgOrder g == n * groupD] of
            (g:_) -> Identified g
            []    -> case [g | g <- groups, tgName g == "AGL(1," ++ show n ++ ")"] of
                       (g:_) -> Identified g
                       []    -> Identified (lastOf groups)

------------------------------------------------------------------------
-- Frobenius test for C5 vs D5 (degree 5 fast path)
------------------------------------------------------------------------

--- Frobenius/Chebotarev test to distinguish C5 from D5.
--- For C5, factorization mod p is either {5} or {1,1,1,1,1}.
--- For D5, the pattern {1,2,2} also appears.
frobeniusTestC5 :: Poly -> Bool
frobeniusTestC5 p =
  let testPrimes = filter (\pr -> pr > 5) (take 30 primesList)
      patterns = map (factorizationPattern p) testPrimes
      hasDihedral = any (\pat -> pat == [1, 2, 2] || pat == [2, 2, 1]) patterns
  in not hasDihedral

------------------------------------------------------------------------
-- Factorisation patterns mod p
------------------------------------------------------------------------

--- Factorization pattern of f mod p for degree-5 (legacy).
factorizationPattern :: Poly -> Int -> [Int]
factorizationPattern (Poly cs) p =
  let reduced = map (\c -> numerator c `mod` p) cs
      roots = [x | x <- [0 .. p - 1],
               evalModP reduced x p == 0]
      nRoots = length roots
  in case nRoots of
       0 -> [5]
       1 -> [1, 4]
       2 -> [1, 2, 2]
       5 -> [1, 1, 1, 1, 1]
       _ -> [nRoots, 5 - nRoots]

--- Factorization pattern via distinct-degree factorisation for
--- prime degree n. Uses F_p polynomial arithmetic.
factorizationPatternPrime :: [Int] -> Int -> Int -> [Int]
factorizationPatternPrime intCs n p =
  let cs = map (\c -> ((c `mod` p) + p) `mod` p) intCs
  in factorPatternFp cs p

--- Distinct-degree factorisation pattern of a polynomial over F_p.
--- Computes gcd(x^{p^k} - x, f(x)) for k = 1, 2, ...
factorPatternFp :: [Int] -> Int -> [Int]
factorPatternFp fcs p = go [] 1 (fpTrim fcs) [0, 1]
  where
    go degs _k f _h | fpDeg f <= 0 = sortedList degs
    go degs k f h =
      let h' = fpPowMod h p f p
          hx = fpSub h' [0, 1] p
          g = fpGcd hx f p
          gd = fpDeg g
      in if gd == 0
         then go degs (k + 1) f h'
         else let nf = gd `div` k
                  f' = fpDiv f g p
              in go (degs ++ replicate nf k) (k + 1) f' h'

------------------------------------------------------------------------
-- F_p polynomial arithmetic (ascending coefficient lists)
------------------------------------------------------------------------

fpTrim :: [Int] -> [Int]
fpTrim = reverseL . dropWhile (== 0) . reverseL

fpDeg :: [Int] -> Int
fpDeg cs = length (fpTrim cs) - 1

fpAdd :: [Int] -> [Int] -> Int -> [Int]
fpAdd a b p =
  let la = length a; lb = length b; n = maxInt la lb
      a' = a ++ replicate (n - la) 0
      b' = b ++ replicate (n - lb) 0
  in fpTrim [(x + y) `mod` p | (x, y) <- zip a' b']

fpSub :: [Int] -> [Int] -> Int -> [Int]
fpSub a b p =
  let la = length a; lb = length b; n = maxInt la lb
      a' = a ++ replicate (n - la) 0
      b' = b ++ replicate (n - lb) 0
  in fpTrim [((x - y) `mod` p + p) `mod` p | (x, y) <- zip a' b']

fpMul :: [Int] -> [Int] -> Int -> [Int]
fpMul a b p
  | null a || null b = []
  | otherwise =
      let na = length a; nb = length b
      in fpTrim
           [ foldl (\acc j ->
               if j >= 0 && j < na && (i - j) >= 0 && (i - j) < nb
               then (acc + (a !! j) * (b !! (i - j))) `mod` p
               else acc) 0 [0 .. i]
               `mod` p
           | i <- [0 .. na + nb - 2]
           ]

fpMod :: [Int] -> [Int] -> Int -> [Int]
fpMod a b p
  | fpDeg a < fpDeg b = fpTrim (map (\x -> ((x `mod` p) + p) `mod` p) a)
  | null (fpTrim b) = error "fpMod: division by zero"
  | otherwise =
      let ta = fpTrim a
          tb = fpTrim b
          lcB = lastOf tb
          lcBInv = fpInv lcB p
          shift = fpDeg ta - fpDeg tb
          lcA = lastOf ta
          fac = (lcA * lcBInv) `mod` p
          sub = [ let bi = if i >= shift && (i - shift) < length tb
                           then tb !! (i - shift)
                           else 0
                  in ((ta_i - fac * bi) `mod` p + p) `mod` p
                | (i, ta_i) <- zip [0..] (ta ++ replicate (shift + length tb - length ta) 0)
                ]
      in fpMod (fpTrim sub) tb p

fpDiv :: [Int] -> [Int] -> Int -> [Int]
fpDiv a b p = go [] (fpTrim a)
  where
    db = fpDeg b
    tb = fpTrim b
    lcBInv = fpInv (lastOf tb) p
    go q r
      | fpDeg r < db = fpTrim q
      | otherwise =
          let tr = fpTrim r
              shift = fpDeg tr - db
              fac = (lastOf tr * lcBInv) `mod` p
              q' = fpAdd q (replicate shift 0 ++ [fac]) p
              sub = [ if i >= shift && (i - shift) < length tb
                      then (fac * tb !! (i - shift)) `mod` p
                      else 0
                    | i <- [0 .. length tr - 1]
                    ]
              r' = fpTrim (zipWith (\x y -> ((x - y) `mod` p + p) `mod` p) tr sub)
          in go q' r'

fpGcd :: [Int] -> [Int] -> Int -> [Int]
fpGcd a b p
  | null (fpTrim b) || fpDeg b < 0 = fpMakeMonic (fpTrim a) p
  | otherwise = fpGcd b (fpMod a b p) p

fpMakeMonic :: [Int] -> Int -> [Int]
fpMakeMonic [] _ = []
fpMakeMonic cs p =
  let lc = lastOf cs
      lcInv = fpInv lc p
  in map (\c -> (c * lcInv) `mod` p) cs

fpPowMod :: [Int] -> Int -> [Int] -> Int -> [Int]
fpPowMod base expo modulus p = go [1] base expo
  where
    go res _ 0 = res
    go res b e =
      let res' = if odd e then fpMod (fpMul res b p) modulus p else res
          b' = fpMod (fpMul b b p) modulus p
      in go res' b' (e `div` 2)

fpInv :: Int -> Int -> Int
fpInv a m = ((x `mod` m) + m) `mod` m
  where
    (_, x, _) = eGcdInt a m

eGcdInt :: Int -> Int -> (Int, Int, Int)
eGcdInt 0 b = (b, 0, 1)
eGcdInt a b = let (g, x, y) = eGcdInt (b `mod` a) a in (g, y - (b `div` a) * x, x)

--- Extract coefficients from a Poly.
polyCoeffs :: Poly -> [Rational]
polyCoeffs (Poly cs) = cs

--- Evaluate polynomial mod p.
evalModP :: [Int] -> Int -> Int -> Int
evalModP cs x p = case cs of
  [] -> 0
  _  -> foldl (\acc c -> (acc * x + c) `mod` p) 0 (reverseL cs)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

--- Simple primes list.
primesList :: [Int]
primesList = 2 : filter isPrime [3, 5 ..]

--- Check if a rational is a perfect square.
isRatPerfectSquare :: Rational -> Bool
isRatPerfectSquare r
  | r == Rational.fromInt 0 = True
  | ratLt r (Rational.fromInt 0) = False
  | otherwise =
      let n = numerator r
          d = denominator r
      in isIntPerfectSquare (absI n) && isIntPerfectSquare d

isIntPerfectSquare :: Int -> Bool
isIntPerfectSquare n =
  let s = isqrtI n in s * s == n

isqrtI :: Int -> Int
isqrtI n
  | n <= 0    = 0
  | otherwise = go n
  where go x = let x' = (x + n `div` x) `div` 2
               in if x' >= x then x else go x'

absI :: Int -> Int
absI x = if x < 0 then negate x else x

lcmInt :: Int -> Int -> Int
lcmInt a b
  | a == 0    = 0
  | b == 0    = 0
  | otherwise = absI (a * b) `div` gcdI (absI a) (absI b)

gcdI :: Int -> Int -> Int
gcdI a b
  | b == 0    = a
  | otherwise = gcdI b (a `mod` b)

reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) []

lastOf :: [a] -> a
lastOf [x]    = x
lastOf (_:xs) = lastOf xs
lastOf []     = error "lastOf: empty"

lengthOf :: [a] -> Int
lengthOf = length

minimumOf :: [Int] -> Int
minimumOf [x] = x
minimumOf (x:xs) = minInt x (minimumOf xs)
minimumOf [] = error "minimumOf: empty"

minInt :: Int -> Int -> Int
minInt a b = if a <= b then a else b

maxInt :: Int -> Int -> Int
maxInt a b = if a >= b then a else b

allSame :: [Int] -> Bool
allSame [] = True
allSame [_] = True
allSame (x:y:ys) = x == y && allSame (y:ys)

sortedList :: [Int] -> [Int]
sortedList [] = []
sortedList (x:xs) = sortedList [y | y <- xs, y <= x] ++ [x]
                     ++ sortedList [y | y <- xs, y > x]

--- Show.
showGaloisResult :: GaloisResult -> String
showGaloisResult (Identified g) = "Identified: " ++ showTransitiveGroup g
showGaloisResult (NotSupported s) = "Not supported: " ++ s

instance Show GaloisResult where
  show = showGaloisResult
