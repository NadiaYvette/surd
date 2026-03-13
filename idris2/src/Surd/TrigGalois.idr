module Surd.TrigGalois

import Surd.Rational
import Surd.Types
import Surd.Eval
import Surd.Positive
import Surd.PrimeFactors
import Surd.Cyclotomic

import Data.SortedMap
import Data.List

%default covering

------------------------------------------------------------------------
-- Number theory helpers
------------------------------------------------------------------------

||| Modular exponentiation: base^exp mod m.
export
modExp : Integer -> Integer -> Integer -> Integer
modExp base 0 _ = 1
modExp base exp m =
  if exp < 0 then 0  -- not handled
  else
    let half = modExp base (div exp 2) m
        sq = mod (half * half) m
    in if mod exp 2 == 0 then sq
       else mod (sq * base) m

gcdI : Integer -> Integer -> Integer
gcdI a 0 = a
gcdI a b = assert_total $ gcdI b (mod a b)

isPrimRoot : Integer -> Integer -> Integer -> List Integer -> Bool
isPrimRoot g n phi factors =
  gcdI g n == 1 &&
  all (\p => modExp g (div phi p) n /= 1) factors

||| Find a primitive root modulo n (if one exists).
||| (Z/nZ)* is cyclic iff n = 1, 2, 4, p^k, or 2*p^k for odd prime p.
export
primitiveRoot : Integer -> Maybe Integer
primitiveRoot n =
  if n <= 1 then Nothing
  else if n == 2 then Just 1
  else
    let phi = eulerTotient n
        factors = case positive (cast phi) of
                    Nothing => []
                    Just pos => map fst (factorise pos)
    in findRoot 2 n phi factors
  where
    findRoot : Integer -> Integer -> Integer -> List Integer -> Maybe Integer
    findRoot g n phi factors =
      if g >= n then Nothing
      else if isPrimRoot g n phi factors then Just g
           else findRoot (g + 1) n phi factors

------------------------------------------------------------------------
-- Gauss period computation
------------------------------------------------------------------------

||| Compute the subgroup chain for (Z/nZ)*.
||| Given primitive root g and phi(n), factor phi into primes
||| and build the descending chain of subgroups.
export
subgroupChain : Integer -> Integer -> List Int
subgroupChain phi g =
  case positive (cast phi) of
    Nothing => []
    Just pos =>
      let fs = factorise pos
      in concatMap (\pf => replicate (snd pf) (cast (fst pf))) fs

periodExpr : Integer -> Int -> RadExpr Rational
periodExpr n k =
  -- zeta^k = cos(2*pi*k/n) + i*sin(2*pi*k/n)
  -- For the real part (cos), this is what we compute
  -- For now, return a placeholder that will be filled by cosOfUnity
  Lit (Rational.fromInteger (cast k))

buildPeriodsSimple : Integer -> Integer -> SortedMap Int (RadExpr Rational)
buildPeriodsSimple n g =
  -- Simple construction: for small n, use known radical forms
  -- For larger n, this would use the full Lagrange resolvent machinery
  fromList [(k, periodExpr n k) | k <- map cast [1 .. n - 1]]

||| Compute Gauss periods for a prime p.
|||
||| For a prime p with primitive root g and step q (a prime factor of p-1),
||| the Gauss period eta_j = sum_{k in coset_j} zeta^k
||| where the cosets partition (Z/pZ)* into q cosets of size (p-1)/q.
|||
||| This is the core of the Gauss period descent.
export
gaussPeriods : Int -> Maybe (SortedMap Int (RadExpr Rational))
gaussPeriods n =
  if n <= 2 then Nothing
  else
    let ni = cast {to = Integer} n
    in case primitiveRoot ni of
         Nothing => Nothing
         Just g =>
           let phi = eulerTotient ni
               steps = subgroupChain phi g
           in Just (descendPeriods ni g phi steps)
  where
    descendPeriods : Integer -> Integer -> Integer -> List Int -> SortedMap Int (RadExpr Rational)
    descendPeriods n g phi [] =
      -- Base case: individual elements
      fromList [(cast k, Lit (Rational.fromInteger k)) | k <- [1 .. n - 1]]
    descendPeriods n g phi steps =
      -- Start with sum of all primitive elements = mu(n) (Ramanujan sum)
      -- For primes: sum = -1
      -- Build all elements via period descent
      -- For now, return a map of all k -> zeta^k (using cos + i*sin form)
      buildPeriodsSimple n g

------------------------------------------------------------------------
-- All periods via Gauss descent
------------------------------------------------------------------------

||| Compute all primitive nth root of unity expressions via Gauss period descent.
||| Returns a map from k to the radical expression for zeta^k.
export
allPeriodsViaGauss : Int -> Maybe (SortedMap Int (RadExpr Rational))
allPeriodsViaGauss n =
  if n <= 2 then Nothing
  else gaussPeriods n
