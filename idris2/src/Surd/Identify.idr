||| Galois group identification for irreducible polynomials over Q.
|||
||| For degree 5, uses the discriminant + sextic resolvent decision tree.
||| For general prime degree p, uses Frobenius/Chebotarev cycle-pattern
||| analysis to determine whether the group lies inside AGL(1,p) and
||| identify the exact solvable subgroup.
module Surd.Identify

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Permutation
import Surd.TransitiveGroup
import Surd.Resolvent
import Surd.PrimeFactors

import Data.List
import Data.Maybe

%default covering

------------------------------------------------------------------------
-- Integer GCD helper
------------------------------------------------------------------------

gcdI : Integer -> Integer -> Integer
gcdI a 0 = a
gcdI a b = gcdI b (mod a b)

lcmI : Integer -> Integer -> Integer
lcmI a b = if a == 0 || b == 0 then 0
            else div (abs (a * b)) (gcdI (abs a) (abs b))

------------------------------------------------------------------------
-- Galois group identification result
------------------------------------------------------------------------

||| Result of Galois group identification.
public export
data GaloisResult : Type where
  ||| Successfully identified the Galois group of a degree-5 polynomial.
  Identified5 : TransGroup 5 -> GaloisResult
  ||| Successfully identified a prime-degree group at runtime.
  IdentifiedRT : TransGroupRT -> GaloisResult
  ||| Successfully identified with ad-hoc info (small degrees).
  IdentifiedAdHoc : String -> Integer -> Bool -> GaloisResult
  ||| Could not identify (unsupported degree or other issue).
  Unidentified : String -> GaloisResult

export
Show GaloisResult where
  show (Identified5 tg) = "Galois group: " ++ show tg
  show (IdentifiedRT tg) = "Galois group: " ++ show tg
  show (IdentifiedAdHoc name order solv) =
    "Galois group: " ++ name ++ " (order " ++ show order ++ ", "
    ++ (if solv then "solvable" else "non-solvable") ++ ")"
  show (Unidentified msg) = "Unidentified: " ++ msg

||| Check if a Galois result is solvable.
export
isSolvableResult : GaloisResult -> Bool
isSolvableResult (Identified5 tg) = tgSolvable tg
isSolvableResult (IdentifiedRT tg) = tgSolvable tg
isSolvableResult (IdentifiedAdHoc _ _ solv) = solv
isSolvableResult (Unidentified _) = False

||| Get the group name from a result.
export
resultGroupName : GaloisResult -> String
resultGroupName (Identified5 tg) = tgName tg
resultGroupName (IdentifiedRT tg) = tgName tg
resultGroupName (IdentifiedAdHoc name _ _) = name
resultGroupName (Unidentified _) = "unknown"

||| Get the group order from a result.
export
resultGroupOrder : GaloisResult -> Integer
resultGroupOrder (Identified5 tg) = tgOrder tg
resultGroupOrder (IdentifiedRT tg) = tgOrder tg
resultGroupOrder (IdentifiedAdHoc _ ord _) = ord
resultGroupOrder (Unidentified _) = 0

||| Get composition factors from a runtime-identified group.
export
resultCompositionFactors : GaloisResult -> List Int
resultCompositionFactors (IdentifiedRT tg) = tgCompositionFactors tg
resultCompositionFactors _ = []

------------------------------------------------------------------------
-- F_p polynomial arithmetic (ascending coefficient lists)
------------------------------------------------------------------------

-- Remove trailing zeros.
fpTrim : List Integer -> List Integer
fpTrim = reverse . dropWhile (== 0) . reverse

-- Degree of a polynomial over F_p.
fpDeg : List Integer -> Int
fpDeg cs = cast (length (fpTrim cs)) - 1

-- Modular multiplicative inverse via extended Euclidean algorithm.
eGcd : Integer -> Integer -> (Integer, Integer, Integer)
eGcd 0 b = (b, 0, 1)
eGcd a b = let (g, x, y) = eGcd (mod b a) a in (g, y - div b a * x, x)

fpInv : Integer -> Integer -> Integer
fpInv a m = mod (mod x m + m) m
  where
    x : Integer
    x = let (_, xx, _) = eGcd a m in xx

-- Addition over F_p.
fpAdd : List Integer -> List Integer -> Integer -> List Integer
fpAdd a b p =
  let n = max (length a) (length b)
      a' = a ++ replicate (minus n (length a)) 0
      b' = b ++ replicate (minus n (length b)) 0
  in fpTrim (zipWith (\x, y => mod (x + y) p) a' b')

-- Subtraction over F_p.
fpSub : List Integer -> List Integer -> Integer -> List Integer
fpSub a b p =
  let n = max (length a) (length b)
      a' = a ++ replicate (minus n (length a)) 0
      b' = b ++ replicate (minus n (length b)) 0
  in fpTrim (zipWith (\x, y => mod (mod (x - y) p + p) p) a' b')

-- Safe list index.
listIdx : Nat -> List Integer -> Integer
listIdx Z (x :: _) = x
listIdx (S k) (_ :: xs) = listIdx k xs
listIdx _ [] = 0

-- Multiplication over F_p (schoolbook).
fpMul : List Integer -> List Integer -> Integer -> List Integer
fpMul a b p =
  if null a || null b then []
  else
    let na = length a
        nb = length b
        rlen = minus (na + nb) 1
        coeff : Nat -> Integer
        coeff i = mod (foldl (\acc, j =>
          if j < na && minus i j < nb
            then acc + listIdx j a * listIdx (minus i j) b
            else acc) 0 (iterateN (S i) id 0)) p
    in fpTrim (map coeff (iterateN rlen id 0))

-- Polynomial remainder over F_p with safety bound.
fpMod : List Integer -> List Integer -> Integer -> List Integer
fpMod a b p = fpModGo 1000 (fpTrim a) (fpTrim b)
  where
    fpModGo : Nat -> List Integer -> List Integer -> List Integer
    fpModGo Z r _ = fpTrim r  -- safety bound
    fpModGo (S fuel) r d =
      let dr = fpDeg r
          dd = fpDeg d
      in if dr < dd then fpTrim (map (\x => mod (mod x p + p) p) r)
         else if null d then r
         else
           let lcR = case last' (fpTrim r) of Just v => v; Nothing => 0
               lcD = case last' d of Just v => v; Nothing => 1
               lcDInv = fpInv lcD p
               fac = mod (lcR * lcDInv) p
               shift = cast {to = Nat} (dr - dd)
               -- Subtract fac * x^shift * d from r
               subAt : Nat -> Integer
               subAt i =
                 let ri = listIdx i r
                     di = if i >= shift && minus i shift < length d
                            then listIdx (minus i shift) d
                            else 0
                 in mod (mod (ri - fac * di) p + p) p
               newR = fpTrim (map subAt (iterateN (length r) id 0))
           in fpModGo fuel newR d

-- Make monic over F_p.
fpMakeMonic : List Integer -> Integer -> List Integer
fpMakeMonic [] _ = []
fpMakeMonic cs p =
  case last' cs of
    Nothing => cs
    Just lc =>
      let lcInv = fpInv lc p
      in map (\c => mod (c * lcInv) p) cs

-- GCD of two polynomials over F_p (with safety bound).
fpGcd : List Integer -> List Integer -> Integer -> List Integer
fpGcd a b p = fpGcdGo 1000 a b
  where
    fpGcdGo : Nat -> List Integer -> List Integer -> List Integer
    fpGcdGo Z a _ = fpMakeMonic (fpTrim a) p
    fpGcdGo (S fuel) a b =
      if null (fpTrim b) || fpDeg b < 0
        then fpMakeMonic (fpTrim a) p
        else fpGcdGo fuel b (fpMod a b p)

-- Polynomial exact division over F_p (with safety bound).
fpDiv : List Integer -> List Integer -> Integer -> List Integer
fpDiv a b p = fpDivGo 1000 [] (fpTrim a)
  where
    db : Int
    db = fpDeg b
    tb : List Integer
    tb = fpTrim b
    lcBInv : Integer
    lcBInv = case last' tb of Just lc => fpInv lc p; Nothing => 1

    fpDivGo : Nat -> List Integer -> List Integer -> List Integer
    fpDivGo Z q _ = fpTrim q
    fpDivGo (S fuel) q r =
      if fpDeg r < db then fpTrim q
      else
        let tr = fpTrim r
            dr = fpDeg r
            shift = cast {to = Nat} (dr - db)
            fac = case last' tr of Just lr => mod (lr * lcBInv) p; Nothing => 0
            q' = fpAdd q (replicate shift 0 ++ [fac]) p
            subAt : Nat -> Integer
            subAt i =
              let ri = listIdx i tr
                  di = if i >= shift && minus i shift < length tb
                         then listIdx (minus i shift) tb
                         else 0
              in mod (mod (ri - fac * di) p + p) p
            newR = fpTrim (map subAt (iterateN (length tr) id 0))
        in fpDivGo fuel q' newR

-- Modular exponentiation of a polynomial over F_p (binary exponentiation).
fpPowMod : List Integer -> Integer -> List Integer -> Integer -> List Integer
fpPowMod base expo modulus p = fpPowModGo 1000 [1] base expo
  where
    fpPowModGo : Nat -> List Integer -> List Integer -> Integer -> List Integer
    fpPowModGo Z res _ _ = res  -- safety
    fpPowModGo _ res _ 0 = res
    fpPowModGo (S fuel) res b e =
      let res' = if mod e 2 == 1 then fpMod (fpMul res b p) modulus p else res
          b'   = fpMod (fpMul b b p) modulus p
      in fpPowModGo fuel res' b' (div e 2)

------------------------------------------------------------------------
-- Factorisation pattern (distinct-degree factorisation)
------------------------------------------------------------------------

||| Compute the factorisation pattern of a polynomial over F_p.
||| Returns the sorted list of irreducible factor degrees.
factorPattern : List Integer -> Integer -> List Int
factorPattern fcs p = factPatGo 100 [] 1 (fpTrim fcs) [0, 1]
  where
    factPatGo : Nat -> List Int -> Int -> List Integer -> List Integer -> List Int
    factPatGo Z degs _ _ _ = sort degs  -- safety
    factPatGo (S fuel) degs k f h =
      if fpDeg f <= 0 then sort degs
      else
        let -- h <- h^p mod f (computes x^{p^k} mod f)
            h' = fpPowMod h p f p
            -- g = gcd(h' - x, f) mod p
            hx = fpSub h' [0, 1] p
            g  = fpGcd hx f p
            gd = fpDeg g
        in if gd == 0
              then factPatGo fuel degs (k + 1) f h'
              else
                let nf : Int
                    nf = div gd k
                    f' = fpDiv f g p
                in factPatGo fuel (degs ++ replicate (cast nf) k) (k + 1) f' h'

------------------------------------------------------------------------
-- Frobenius / Chebotarev test
------------------------------------------------------------------------

||| Small primes for Frobenius testing.
smallPrimes : List Int
smallPrimes =
  [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
   59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]

||| Check if a polynomial has a non-cyclic factorisation pattern mod p.
||| Patterns consistent with a cyclic group are {n} (inert) or {1,...,1} (split).
hasNonCyclicPattern : List Integer -> Int -> Int -> Bool
hasNonCyclicPattern intCs p n =
  let pI = cast {to = Integer} p
      cs = map (\c => mod (mod c pI + pI) pI) intCs
      pat = factorPattern cs pI
  in pat /= [n] && pat /= replicate (cast n) 1

||| Frobenius/Chebotarev test to distinguish cyclic from non-cyclic.
||| Returns True if the group appears to be cyclic (Z/p).
export
isCyclicByFrobenius : Poly Rational -> Bool
isCyclicByFrobenius f =
  let cs = coeffs f
      lcmDen = foldl (\acc, c => lcmInteger acc (denom c)) 1 cs
      intCs = map (\c => numer (c * Rational.fromInteger lcmDen)) cs
      lc = case last' intCs of Just v => v; Nothing => 1
      n = cast {to = Int} (degreeInt f)
      goodPrime : Int -> Bool
      goodPrime p =
        let pI = cast {to = Integer} p
        in mod lc pI /= 0
      testPs = take 20 (filter goodPrime smallPrimes)
  in not (any (\p => hasNonCyclicPattern intCs p n) testPs)
  where
    lcmInteger : Integer -> Integer -> Integer
    lcmInteger a b = lcmI a b

------------------------------------------------------------------------
-- Degree 5 identification (fast path)
------------------------------------------------------------------------

||| Identify the Galois group of an irreducible degree-5 polynomial.
export
identifyGaloisGroup5 : Poly Rational -> GaloisResult
identifyGaloisGroup5 p =
  case degreeInt p of
    5 =>
      let discSquare = isDiscriminantSquare p
          sexticRoot = hasSexticRationalRoot p
      in case (discSquare, sexticRoot) of
           (False, False) => Identified5 s5
           (True, False)  => Identified5 a5
           (False, True)  => Identified5 f20
           (True, True)   =>
             if isCyclicByFrobenius p then Identified5 c5
             else Identified5 d5
    d => Unidentified ("Unsupported degree: " ++ show d)

------------------------------------------------------------------------
-- General prime-degree identification
------------------------------------------------------------------------

||| Identify the Galois group of an irreducible prime-degree polynomial
||| via Frobenius/Chebotarev cycle-pattern analysis.
|||
||| Strategy:
|||   1. Compute factorisation patterns mod small primes.
|||   2. For AGL(1,p) subgroups, valid patterns are:
|||      [p] (translation), [1,...,1] (identity), or [1, k, k, ...k]
|||      where k | (p-1).
|||   3. If all patterns are AGL-compatible, find the minimal stabiliser
|||      order d = lcm of observed non-trivial cycle lengths.
|||   4. If any non-AGL pattern is seen, group is A_p or S_p.
export
identifyGaloisGroupPrime : Poly Rational -> GaloisResult
identifyGaloisGroupPrime f =
  let n  = cast {to = Int} (degreeInt f)
      p  = cast {to = Integer} n
      cs = coeffs f

      lcmDen = foldl (\acc, c => lcmI acc (denom c)) 1 cs
      intCs = map (\c => numer (c * Rational.fromInteger lcmDen)) cs
      lc = case last' intCs of Just v => v; Nothing => 1

      goodPrime : Int -> Bool
      goodPrime pr =
        let prI = cast {to = Integer} pr
        in mod lc prI /= 0

      testPrimes = take 50 (filter goodPrime smallPrimes)

      -- Collect factorisation patterns mod each test prime
      patterns : List (List Int)
      patterns = map (\pr =>
        let prI = cast {to = Integer} pr
            modCs = map (\c => mod (mod c prI + prI) prI) intCs
        in factorPattern modCs prI) testPrimes

      -- Check if a pattern is AGL-compatible
      isAGLPattern : List Int -> Bool
      isAGLPattern pat =
        pat == [n]                          -- translation: [p]
        || pat == replicate (cast n) 1      -- identity: [1,...,1]
        || (length pat >= 2                 -- non-translation: [1, k, k, ...]
           && case head' pat of
                Nothing => False
                Just _ =>
                  let minP = foldl min n pat
                      ones = filter (== 1) pat
                      ks   = filter (/= 1) pat
                  in minP == 1 && length ones == 1
                     && case head' ks of
                          Nothing => False
                          Just k  => all (== k) ks)

      insideAGL = all isAGLPattern patterns

      -- Find minimum d: lcm of all observed non-trivial cycle lengths
      nonTrivCycleLengths : List Integer
      nonTrivCycleLengths =
        concatMap (\pat =>
          if pat == [n] || pat == replicate (cast n) 1 then []
          else map cast (filter (/= 1) pat)) patterns

      minD : Integer
      minD = if null nonTrivCycleLengths then 1
             else foldl lcmI 1 nonTrivCycleLengths

      groups = transGroupsOfDegreeRT n

      -- Check discriminant for A_p vs S_p
      discSq = isDiscriminantSquare f

  in if not insideAGL
     then
       -- Not in AGL(1,p): either A_p or S_p
       let apName = "A" ++ show p
           finalGroup = if discSq
             then case find (\g => tgName g == apName) groups of
                    Just g  => g
                    Nothing => case last' groups of Just g => g; Nothing => mkFallback n
             else case last' groups of Just g => g; Nothing => mkFallback n
       in IdentifiedRT finalGroup
     else
       -- Inside AGL(1,p): find smallest d >= minD that divides p-1
       let solvableDivs = sort [div (tgOrder g) p | g <- groups, tgSolvable g]
           groupD = case find (>= minD) solvableDivs of
                      Just d  => d
                      Nothing => p - 1  -- fallback to full AGL(1,p)
           finalGroup = case find (\g => tgOrder g == p * groupD) groups of
                          Just g  => g
                          Nothing => case find (\g => tgName g == "AGL(1," ++ show p ++ ")") groups of
                                       Just g  => g
                                       Nothing => mkFallback n
       in IdentifiedRT finalGroup
  where
    mkFallback : Int -> TransGroupRT
    mkFallback deg = MkTransGroupRT "unknown" deg 1 False [] [] []

------------------------------------------------------------------------
-- General identification
------------------------------------------------------------------------

||| Identify the Galois group of an irreducible polynomial over Q.
export
identifyGaloisGroup : Poly Rational -> GaloisResult
identifyGaloisGroup p =
  case degreeInt p of
    1 => IdentifiedAdHoc "trivial" 1 True
    2 => IdentifiedAdHoc "C2" 2 True
    3 => if isPrime 3
           then identifyGaloisGroupPrime p
           else IdentifiedAdHoc "S3_or_C3" 6 True
    4 => Unidentified "Degree 4: not yet fully implemented"
    5 => identifyGaloisGroup5 p
    d => if d > 0 && isPrime d
           then identifyGaloisGroupPrime p
           else Unidentified ("Degree " ++ show d ++ " not supported (composite)")

||| Check if a polynomial is solvable by radicals.
export
isSolvableByRadicals : Poly Rational -> Bool
isSolvableByRadicals p = isSolvableResult (identifyGaloisGroup p)
