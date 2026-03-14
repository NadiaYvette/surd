||| Normalization passes for radical expressions over Q.
|||
||| These are explicit transformations the user applies as needed.
||| They do NOT denest -- see Denest for that.
|||
||| The pipeline is:
|||   normalize = fixN 10 (collectTerms . collectCoefficients . distribute .
|||     sortCommutative . extractPerfectPowers . simplifyPowers .
|||     foldConstants . flattenArith)
module Surd.Normalize

import Surd.Rational
import Surd.Positive
import Surd.PrimeFactors
import Surd.Types

import Data.Nat
import Data.SortedMap
import Data.List
import Data.Maybe

%default covering

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

ratZero : Rational
ratZero = Rational.zero

ratOne : Rational
ratOne = Rational.one

isRatZero : Rational -> Bool
isRatZero = Surd.Rational.isZero

isRatOne : Rational -> Bool
isRatOne r = r == ratOne

negOne : Rational
negOne = negate ratOne

------------------------------------------------------------------------
-- flattenArith
------------------------------------------------------------------------

||| Flatten nested Add/Mul and cancel double negations / double inverses.
export
flattenArith : RadExpr k -> RadExpr k
flattenArith (Neg (Neg a)) = flattenArith a
flattenArith (Neg a) = Neg (flattenArith a)
flattenArith (Add a b) = Add (flattenArith a) (flattenArith b)
flattenArith (Mul a b) = Mul (flattenArith a) (flattenArith b)
flattenArith (Inv (Inv a)) = flattenArith a
flattenArith (Inv a) = Inv (flattenArith a)
flattenArith (Root n a) = Root n (flattenArith a)
flattenArith (Pow a n) = Pow (flattenArith a) n
flattenArith e = e

------------------------------------------------------------------------
-- foldConstants
------------------------------------------------------------------------

||| Fold constant subexpressions: evaluate pure-literal subtrees.
export
foldConstants : RadExpr Rational -> RadExpr Rational
foldConstants expr = case expr of
  Lit r => Lit r
  Neg a => case foldConstants a of
    Lit r => Lit (negate r)
    Neg a' => a'
    a' => Neg a'
  Add a b => case (foldConstants a, foldConstants b) of
    (Lit r, Lit s) => Lit (r + s)
    (Lit r, b') => if isRatZero r then b' else Add (Lit r) b'
    (a', Lit s) => if isRatZero s then a' else Add a' (Lit s)
    (a', Neg b') => if a' == b' then Lit ratZero else Add a' (Neg b')
    (a', b') => Add a' b'
  Mul a b =>
    let a' = foldConstants a
        b' = foldConstants b
    in case (a', b') of
      (Lit r, Lit s) => Lit (r * s)
      (Lit r, _) =>
        if isRatZero r then Lit ratZero
        else if isRatOne r then b'
        else if r == negOne then Neg b'
        else Mul (Lit r) b'
      (_, Lit s) =>
        if isRatZero s then Lit ratZero
        else if isRatOne s then a'
        else if s == negOne then Neg a'
        else Mul a' (Lit s)
      _ => Mul a' b'
  Inv a => case foldConstants a of
    Lit r => if not (isRatZero r) then Lit (recip r) else Inv (Lit r)
    Inv a' => a'
    a' => Inv a'
  Root n a => case foldConstants a of
    Lit r => if isRatZero r then Lit ratZero
             else if isRatOne r then Lit ratOne
             else Root n (Lit r)
    a' => Root n a'
  Pow a n => case foldConstants a of
    Lit r => Lit (powRatInt r (cast n))
    a' => if n == 0 then Lit ratOne
          else if n == 1 then a'
          else Pow a' n

------------------------------------------------------------------------
-- simplifyPowers
------------------------------------------------------------------------

||| Simplify power expressions.
export
simplifyPowers : RadExpr Rational -> RadExpr Rational
simplifyPowers expr = case expr of
  Mul (Root 2 a) (Root 2 b) =>
    let lte22 : LTE 2 2 = LTESucc (LTESucc LTEZero)
    in if a == b then simplifyPowers a
       else Mul (simplifyPowers (Root 2 a @{lte22})) (simplifyPowers (Root 2 b @{lte22}))
  Pow (Pow a m) n => simplifyPowers (Pow a (m * n))
  Pow (Root n a) m =>
    if m == cast n then simplifyPowers a
    else Pow (Root n (simplifyPowers a)) m
  Root n (Pow a m) =>
    if m == cast n then simplifyPowers a
    else Root n (Pow (simplifyPowers a) m)
  Root m (Root n a) =>
    let 0 mulPrf : LTE 2 (m * n) = believe_me (the (LTE 0 0) LTEZero)
    in Root (m * n) @{mulPrf} (simplifyPowers a)
  Neg a => Neg (simplifyPowers a)
  Add a b => Add (simplifyPowers a) (simplifyPowers b)
  Mul a b => Mul (simplifyPowers a) (simplifyPowers b)
  Inv a => Inv (simplifyPowers a)
  Root n a => Root n (simplifyPowers a)
  Pow a n => Pow (simplifyPowers a) n
  e => e

------------------------------------------------------------------------
-- extractPerfectPowers
------------------------------------------------------------------------

||| Given n and a positive integer m, extract the largest kth power
||| that divides m: m = extracted^n * remainder.
extractNthPower : Nat -> Integer -> (Rational, Rational)
extractNthPower n m =
  case positive (cast (abs m)) of
    Nothing => (ratOne, ratOne)
    Just pos =>
      let fs = factorise pos
          ni : Integer = cast n
          extracted = foldl (\acc, pr => acc * powRatInt (Rational.fromInteger (fst pr)) (div (cast (snd pr)) ni)) ratOne fs
          remainder = foldl (\acc, pr => acc * powRatInt (Rational.fromInteger (fst pr)) (mod (cast (snd pr)) ni)) ratOne fs
      in (extracted, remainder)

isOddNat : Nat -> Bool
isOddNat n = mod n 2 /= 0

||| Extract perfect nth powers from under radicals.
export
extractPerfectPowers : RadExpr Rational -> RadExpr Rational
extractPerfectPowers expr = case expr of
  Root n (Lit r) =>
    if r > ratZero then
      let rnum = numer r
          rden = denom r
          (numOut, numIn) = extractNthPower n rnum
          (denOut, denIn) = extractNthPower n rden
          res : (Rational, Rational)
          res = if denIn == ratOne
                  then (numOut / denOut, numIn)
                  else
                    let newInner = numIn * powRatInt denIn (cast n - 1)
                        newOuter = numOut / (denOut * denIn)
                        (numOut2, numIn2) = extractNthPower n (numer newInner)
                    in (newOuter * numOut2, numIn2)
          outerCoeff = fst res
          innerRat = snd res
      in case (outerCoeff == ratOne, innerRat == ratOne) of
           (True, True) => Lit ratOne
           (True, False) => Root n (Lit innerRat)
           (_, True) => Lit outerCoeff
           _ => Mul (Lit outerCoeff) (Root n (Lit innerRat))
    else if r < ratZero && isOddNat n then
      Neg (extractPerfectPowers (Root n (Lit (negate r))))
    else Root n (Lit r)
  Root n a => Root n (extractPerfectPowers a)
  Neg a => Neg (extractPerfectPowers a)
  Add a b => Add (extractPerfectPowers a) (extractPerfectPowers b)
  Mul a b => Mul (extractPerfectPowers a) (extractPerfectPowers b)
  Inv a => Inv (extractPerfectPowers a)
  Pow a n => Pow (extractPerfectPowers a) n
  e => e

------------------------------------------------------------------------
-- collectCoefficients
------------------------------------------------------------------------

flattenMul : RadExpr Rational -> List (RadExpr Rational)
flattenMul (Mul a b) = flattenMul a ++ flattenMul b
flattenMul e = [e]

partitionLits : List (RadExpr Rational) -> (List Rational, List (RadExpr Rational))
partitionLits [] = ([], [])
partitionLits (Lit r :: xs) =
  let (ls, rs) = partitionLits xs in (r :: ls, rs)
partitionLits (Inv (Lit r) :: xs) =
  if not (isRatZero r)
    then let (ls, rs) = partitionLits xs in (recip r :: ls, rs)
    else let (ls, rs) = partitionLits xs in (ls, Inv (Lit r) :: rs)
partitionLits (x :: xs) =
  let (ls, rs) = partitionLits xs in (ls, x :: rs)

buildMul : List (RadExpr Rational) -> RadExpr Rational
buildMul [] = Lit ratOne
buildMul [x] = x
buildMul (x :: xs) = Mul x (buildMul xs)

applyCoeff : Rational -> RadExpr Rational -> RadExpr Rational
applyCoeff c body =
  if isRatZero c then Lit ratZero
  else if isRatOne c then body
  else if c == negOne then Neg body
  else case body of
    Lit r => Lit (c * r)
    _ => Mul (Lit c) body

||| Collect rational coefficients in products.
export
collectCoefficients : RadExpr Rational -> RadExpr Rational
collectCoefficients = go
  where
    go : RadExpr Rational -> RadExpr Rational
    go expr = case expr of
      Mul _ _ =>
        let factors = flattenMul expr
            (lits, rest) = partitionLits (map go factors)
            coeff = foldl (*) ratOne lits
            body = buildMul rest
        in applyCoeff coeff body
      Neg a => case go a of
        Lit r => Lit (negate r)
        a' => Neg a'
      Add a b => Add (go a) (go b)
      Inv a => Inv (go a)
      Root n a => Root n (go a)
      Pow a n => Pow (go a) n
      e => e

------------------------------------------------------------------------
-- collectTerms
------------------------------------------------------------------------

flattenAdd : RadExpr Rational -> List (RadExpr Rational)
flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
flattenAdd e = [e]

splitCoeff : RadExpr Rational -> (Rational, RadExpr Rational)
splitCoeff (Mul (Lit c) body) = (c, body)
splitCoeff (Neg e) = let (c, b) = splitCoeff e in (negate c, b)
splitCoeff (Lit r) = (r, Lit ratOne)
splitCoeff e = (ratOne, e)

groupTerms : List (RadExpr Rational) -> SortedMap (RadExpr Rational) Rational
groupTerms = foldl addTerm empty
  where
    addTerm : SortedMap (RadExpr Rational) Rational -> RadExpr Rational -> SortedMap (RadExpr Rational) Rational
    addTerm m term =
      let (c, base) = splitCoeff term
          old = fromMaybe ratZero (lookup base m)
      in insert base (old + c) m

applyCoeffTerm : Rational -> RadExpr Rational -> RadExpr Rational
applyCoeffTerm c body =
  if isRatOne c && isRatOne' body then Lit ratOne
  else if isRatOne c then body
  else if c == negOne then Neg body
  else if isRatOne' body then Lit c
  else Mul (Lit c) body
  where
    isRatOne' : RadExpr Rational -> Bool
    isRatOne' (Lit r) = isRatOne r
    isRatOne' _ = False

buildAdd : List (RadExpr Rational) -> RadExpr Rational
buildAdd [] = Lit ratZero
buildAdd [x] = x
buildAdd (x :: xs) = foldl Add x xs

||| Collect like terms in sums.
export
collectTerms : RadExpr Rational -> RadExpr Rational
collectTerms = go
  where
    go : RadExpr Rational -> RadExpr Rational
    go expr = case expr of
      Add _ _ =>
        let terms = flattenAdd expr
            processed = map go terms
            grouped = groupTerms processed
            sorted = sortBy (\a, b => compare (Builtin.fst a) (Builtin.fst b)) (Data.SortedMap.toList grouped)
            rebuilt = mapMaybe (\p => let base = Builtin.fst p
                                          c = Builtin.snd p
                                      in if isRatZero c then Nothing
                                         else Just (applyCoeffTerm c base)) sorted
        in buildAdd rebuilt
      Neg a => Neg (go a)
      Mul a b => Mul (go a) (go b)
      Inv a => Inv (go a)
      Root n a => Root n (go a)
      Pow a n => Pow (go a) n
      e => e

------------------------------------------------------------------------
-- sortCommutative
------------------------------------------------------------------------

flattenAdd' : RadExpr Rational -> List (RadExpr Rational)
flattenAdd' (Add a b) = flattenAdd' a ++ flattenAdd' b
flattenAdd' e = [e]

flattenMul' : RadExpr Rational -> List (RadExpr Rational)
flattenMul' (Mul a b) = flattenMul' a ++ flattenMul' b
flattenMul' e = [e]

buildAdd' : List (RadExpr Rational) -> RadExpr Rational
buildAdd' [] = Lit ratZero
buildAdd' [x] = x
buildAdd' (x :: xs) = foldl Add x xs

buildMul' : List (RadExpr Rational) -> RadExpr Rational
buildMul' [] = Lit ratOne
buildMul' [x] = x
buildMul' (x :: xs) = foldl Mul x xs

||| Sort children of commutative operators into canonical order.
export
sortCommutative : RadExpr Rational -> RadExpr Rational
sortCommutative = go
  where
    go : RadExpr Rational -> RadExpr Rational
    go expr = case expr of
      Add _ _ =>
        let terms = flattenAdd' expr
            sorted = sort (map go terms)
        in buildAdd' sorted
      Mul _ _ =>
        let factors = flattenMul' expr
            sorted = sort (map go factors)
        in buildMul' sorted
      Neg a => Neg (go a)
      Inv a => Inv (go a)
      Root n a => Root n (go a)
      Pow a n => Pow (go a) n
      e => e

------------------------------------------------------------------------
-- distribute
------------------------------------------------------------------------

isSum : RadExpr k -> Bool
isSum (Add _ _) = True
isSum (Neg a) = isSum a
isSum _ = False

flattenS : RadExpr k -> List (RadExpr k)
flattenS (Add a b) = flattenS a ++ flattenS b
flattenS (Neg a) = map Neg (flattenS a)
flattenS e = [e]

flattenSLen : RadExpr k -> Nat
flattenSLen (Add a b) = flattenSLen a + flattenSLen b
flattenSLen (Neg a) = flattenSLen a
flattenSLen _ = 1

isTinySum : RadExpr k -> Bool
isTinySum e = isSum e && flattenSLen e <= 2

rebuildAddR : List (RadExpr Rational) -> RadExpr Rational
rebuildAddR [] = Lit ratZero
rebuildAddR [x] = x
rebuildAddR (x :: xs) = foldl Add x xs

||| Distribute scalar multiplication over addition.
export
distribute : RadExpr Rational -> RadExpr Rational
distribute = go
  where
    go : RadExpr Rational -> RadExpr Rational
    go expr = case expr of
      Mul (Lit c) r =>
        if isSum r
          then let terms = flattenS r
               in rebuildAddR (map (Mul (Lit c)) terms)
          else case r of
            Neg a => Neg (go (Mul (Lit c) a))
            _ => Mul (Lit c) (go r)
      Mul l (Lit c) =>
        if isSum l
          then let terms = flattenS l
               in rebuildAddR (map (\t => Mul t (Lit c)) terms)
          else case l of
            Neg a => Neg (go (Mul a (Lit c)))
            _ => Mul (go l) (Lit c)
      Mul l r =>
        if isTinySum l && not (isSum r)
          then let terms = flattenS l
               in rebuildAddR (map (\t => go (Mul t r)) terms)
          else if not (isSum l) && isTinySum r
            then let terms = flattenS r
                 in rebuildAddR (map (\t => go (Mul l t)) terms)
            else case (l, r) of
              (Neg a, _) => Neg (go (Mul a r))
              (_, Neg b) => Neg (go (Mul l b))
              _ => Mul (go l) (go r)
      Neg a => Neg (go a)
      Add a b => Add (go a) (go b)
      Inv a => Inv (go a)
      Root n a => Root n (go a)
      Pow a n => Pow (go a) n
      e => e

------------------------------------------------------------------------
-- fixN and top-level API
------------------------------------------------------------------------

fixN : Eq a => Nat -> (a -> a) -> a -> a
fixN Z _ x = x
fixN (S k) f x =
  let x' = f x
  in if x' == x then x else fixN k f x'

||| A single normalization pass (all sub-passes composed once).
export
normalizeOnce : RadExpr Rational -> RadExpr Rational
normalizeOnce =
  collectTerms
    . collectCoefficients
    . distribute
    . sortCommutative
    . extractPerfectPowers
    . simplifyPowers
    . foldConstants
    . flattenArith

||| Apply all normalization passes, iterated to a fixed point (up to 10 times).
export
normalize : RadExpr Rational -> RadExpr Rational
normalize = fixN 10 normalizeOnce
