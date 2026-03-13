--- Normalization passes for radical expressions.
---
--- These are explicit transformations the user applies as needed.
--- They do NOT denest — see denesting modules for that.
---
--- Uses association lists for term grouping (no Data.Map in Curry).
module Normalize
  ( normalize
  , normalizeOnce
  , flattenArith
  , foldConstants
  , simplifyPowers
  , extractPerfectPowers
  , collectCoefficients
  , collectTerms
  , distribute
  , sortCommutative
  ) where

import Data.List (sort, sortBy, partition)
import Rational
import RadExpr
import Positive (unsafePositive)
import PrimeFactors (factorise)

--- Shorthand for zero, one, negative one as Rational.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

rNegOne :: Rational
rNegOne = Rational.fromInt (negate 1)

--- Apply all normalization passes, iterated to a fixed point (max 10 times).
normalize :: RadExpr Rational -> RadExpr Rational
normalize = fixN 10 normalizeOnce

--- A single normalization pass (all sub-passes composed once).
normalizeOnce :: RadExpr Rational -> RadExpr Rational
normalizeOnce expr =
  collectTerms
    (collectCoefficients
      (distribute
        (sortCommutative
          (extractPerfectPowers
            (simplifyPowers
              (foldConstants
                (flattenArith expr)))))))

--- Iterate a function until fixed point or fuel runs out.
fixN :: Int -> (RadExpr Rational -> RadExpr Rational)
     -> RadExpr Rational -> RadExpr Rational
fixN n f x =
  if n <= 0
  then x
  else let x' = f x
       in if x' == x then x else fixN (n - 1) f x'

--- Flatten nested Add/Mul (associativity) and cancel double negations/inversions.
flattenArith :: RadExpr k -> RadExpr k
flattenArith expr = case expr of
  Neg (Neg a)   -> flattenArith a
  Neg a         -> Neg (flattenArith a)
  Add a b       -> Add (flattenArith a) (flattenArith b)
  Mul a b       -> Mul (flattenArith a) (flattenArith b)
  Inv (Inv a)   -> flattenArith a
  Inv a         -> Inv (flattenArith a)
  Root n a      -> Root n (flattenArith a)
  Pow a n       -> Pow (flattenArith a) n
  e             -> e

--- Fold constant subexpressions: evaluate pure-literal subtrees.
foldConstants :: RadExpr Rational -> RadExpr Rational
foldConstants expr = case expr of
  Lit r -> Lit r
  Neg a -> case foldConstants a of
    Lit r  -> Lit (ratNeg r)
    Neg a' -> a'
    a'     -> Neg a'
  Add a b -> foldConstantsAdd (foldConstants a) (foldConstants b)
  Mul a b -> foldConstantsMul (foldConstants a) (foldConstants b)
  Inv a -> case foldConstants a of
    Lit r  -> if not (ratEq r rZero)
              then Lit (ratDiv rOne r)
              else Inv (Lit r)
    Inv a' -> a'
    a'     -> Inv a'
  Root n a -> case foldConstants a of
    Lit r  -> if ratEq r rZero then Lit rZero
              else if ratEq r rOne then Lit rOne
              else Root n (Lit r)
    a'     -> Root n a'
  Pow a n -> case foldConstants a of
    Lit r  -> Lit (ratPow r n)
    a'     -> if n == 0 then Lit rOne
              else if n == 1 then a'
              else Pow a' n

--- Helper for folding Add constants.
foldConstantsAdd :: RadExpr Rational -> RadExpr Rational
                 -> RadExpr Rational
foldConstantsAdd a' b' = case (a', b') of
  (Lit r, Lit s)  -> Lit (ratAdd r s)
  (Lit r, _)      -> if ratEq r rZero then b' else Add a' b'
  (_, Lit s)      -> if ratEq s rZero then a' else Add a' b'
  (_, Neg bn)     -> if a' == bn then Lit rZero else Add a' (Neg bn)
  _               -> Add a' b'

--- Helper for folding Mul constants.
foldConstantsMul :: RadExpr Rational -> RadExpr Rational
                 -> RadExpr Rational
foldConstantsMul a' b' = case (a', b') of
  (Lit r, Lit s)  -> Lit (ratMul r s)
  (Lit r, _)      -> if ratEq r rZero then Lit rZero
                     else if ratEq r rOne then b'
                     else if ratEq r rNegOne then Neg b'
                     else Mul a' b'
  (_, Lit s)      -> if ratEq s rZero then Lit rZero
                     else if ratEq s rOne then a'
                     else if ratEq s rNegOne then Neg a'
                     else Mul a' b'
  _               -> Mul a' b'

--- Simplify power expressions.
simplifyPowers :: RadExpr Rational -> RadExpr Rational
simplifyPowers expr = case expr of
  Mul (Root 2 a) (Root 2 b) ->
    if a == b then simplifyPowers a
    else Mul (simplifyPowers (Root 2 a)) (simplifyPowers (Root 2 b))
  Pow (Pow a m) n -> simplifyPowers (Pow a (m * n))
  Pow (Root n a) m ->
    if m == n then simplifyPowers a
    else Pow (simplifyPowers (Root n a)) m
  Root n (Pow a m) ->
    if m == n then simplifyPowers a
    else Root n (simplifyPowers (Pow a m))
  Root m (Root n a) -> Root (m * n) (simplifyPowers a)
  Neg a     -> Neg (simplifyPowers a)
  Add a b   -> Add (simplifyPowers a) (simplifyPowers b)
  Mul a b   -> Mul (simplifyPowers a) (simplifyPowers b)
  Inv a     -> Inv (simplifyPowers a)
  Root n a  -> Root n (simplifyPowers a)
  Pow a n   -> Pow (simplifyPowers a) n
  e         -> e

--- Extract perfect nth powers from under radicals.
--- E.g., sqrt(12) = 2*sqrt(3).
extractPerfectPowers :: RadExpr Rational -> RadExpr Rational
extractPerfectPowers expr = case expr of
  Root n (Lit r) -> extractPerfectPowersLit n r
  Root n a       -> Root n (extractPerfectPowers a)
  Neg a          -> Neg (extractPerfectPowers a)
  Add a b        -> Add (extractPerfectPowers a) (extractPerfectPowers b)
  Mul a b        -> Mul (extractPerfectPowers a) (extractPerfectPowers b)
  Inv a          -> Inv (extractPerfectPowers a)
  Pow a n        -> Pow (extractPerfectPowers a) n
  e              -> e

--- Extract perfect nth powers from Root n (Lit r).
extractPerfectPowersLit :: Int -> Rational -> RadExpr Rational
extractPerfectPowersLit n r
  | ratGt r rZero =
      let num = numerator r
          den = denominator r
          (numOut, numIn) = extractNthPower n (absInt num)
          (denOut, denIn) = extractNthPower n (absInt den)
      in if denIn == 1
         then buildExtracted (ratDiv numOut denOut) numIn
         else let newInner = numIn * intPow denIn (n - 1)
                  newOuter = ratDiv numOut (ratMul denOut (Rational.fromInt denIn))
                  (numOut2, numIn2) = extractNthPower n newInner
              in buildExtracted (ratMul newOuter numOut2) numIn2
  | ratLt r rZero && odd n =
      Neg (extractPerfectPowers (Root n (Lit (ratNeg r))))
  | otherwise = Root n (Lit r)
  where
    buildExtracted outerCoeff innerVal =
      let outerIsOne = ratEq outerCoeff rOne
          innerIsOne = innerVal == 1
      in if outerIsOne && innerIsOne then Lit rOne
         else if outerIsOne then Root n (Lit (Rational.fromInt innerVal))
         else if innerIsOne then Lit outerCoeff
         else Mul (Lit outerCoeff) (Root n (Lit (Rational.fromInt innerVal)))

--- Integer power.
intPow :: Int -> Int -> Int
intPow b e
  | e == 0    = 1
  | otherwise = b * intPow b (e - 1)

--- Absolute value of an integer.
absInt :: Int -> Int
absInt x = if x < 0 then negate x else x

--- Given n and a positive integer m, extract the largest kth power
--- that divides m. Returns (extracted, remainder) such that
--- m = extracted^n * remainder.
extractNthPower :: Int -> Int -> (Rational, Int)
extractNthPower n m =
  let fs = factorise (unsafePositive (absInt m))
      extracted = foldl (\acc (p, e) -> acc * intPow p (e `div` n)) 1 fs
      remainder = foldl (\acc (p, e) -> acc * intPow p (e `mod` n)) 1 fs
  in (Rational.fromInt extracted, remainder)

--- Collect rational coefficients in products.
---
--- Flattens a product tree, multiplies all Lit factors together,
--- and produces a single Mul (Lit coeff) body (or just body if coeff = 1).
collectCoefficients :: RadExpr Rational -> RadExpr Rational
collectCoefficients expr = case expr of
  Mul _ _ ->
    let factors = flattenMulN expr
        (lits, rest) = partitionLitsN (map collectCoefficients factors)
        coeff = foldl ratMul rOne lits
        body = buildMulN rest
    in applyCoeffMul coeff body
  Neg a -> case collectCoefficients a of
    Lit r -> Lit (ratNeg r)
    a'    -> Neg a'
  Add a b  -> Add (collectCoefficients a) (collectCoefficients b)
  Inv a    -> Inv (collectCoefficients a)
  Root n a -> Root n (collectCoefficients a)
  Pow a n  -> Pow (collectCoefficients a) n
  e        -> e

--- Flatten a Mul tree into a flat list.
flattenMulN :: RadExpr Rational -> [RadExpr Rational]
flattenMulN expr = case expr of
  Mul a b -> flattenMulN a ++ flattenMulN b
  e       -> [e]

--- Partition a list into Lit values and non-Lit expressions.
partitionLitsN :: [RadExpr Rational] -> ([Rational], [RadExpr Rational])
partitionLitsN [] = ([], [])
partitionLitsN (x:xs) =
  let (ls, rs) = partitionLitsN xs
  in case x of
    Lit r -> (r : ls, rs)
    Inv (Lit r) -> if not (ratEq r rZero)
                   then (ratDiv rOne r : ls, rs)
                   else (ls, x : rs)
    _ -> (ls, x : rs)

--- Build a Mul from a list.
buildMulN :: [RadExpr Rational] -> RadExpr Rational
buildMulN xs = case xs of
  []    -> Lit rOne
  [x]   -> x
  (x:rest) -> Mul x (buildMulN rest)

--- Apply a rational coefficient to an expression.
applyCoeffMul :: Rational -> RadExpr Rational -> RadExpr Rational
applyCoeffMul c body
  | ratEq c rZero   = Lit rZero
  | ratEq c rOne    = body
  | ratEq c rNegOne = Neg body
  | otherwise = case body of
      Lit r -> Lit (ratMul c r)
      _     -> Mul (Lit c) body

--- Collect like terms in sums.
---
--- Flattens a sum tree, groups summands by their non-coefficient "base" part,
--- adds up coefficients within each group, and rebuilds.
collectTerms :: RadExpr Rational -> RadExpr Rational
collectTerms expr = case expr of
  Add _ _ ->
    let terms = flattenAddN expr
        processed = map collectTerms terms
        grouped = groupTermsAL processed
        sorted = sortBy (\(a, _) (b, _) -> a <= b) grouped
        rebuilt = concatMap (\(base, c) ->
                    if ratEq c rZero then []
                    else [applyCoeffAdd c base]) sorted
    in buildAddN rebuilt
  Neg a    -> Neg (collectTerms a)
  Mul a b  -> Mul (collectTerms a) (collectTerms b)
  Inv a    -> Inv (collectTerms a)
  Root n a -> Root n (collectTerms a)
  Pow a n  -> Pow (collectTerms a) n
  e        -> e

--- Flatten an Add tree into a flat list.
flattenAddN :: RadExpr Rational -> [RadExpr Rational]
flattenAddN expr = case expr of
  Add a b -> flattenAddN a ++ flattenAddN b
  e       -> [e]

--- Split a term into (coefficient, base). E.g., Mul (Lit 3) x -> (3, x).
splitCoeff :: RadExpr Rational -> (Rational, RadExpr Rational)
splitCoeff expr = case expr of
  Mul (Lit c) body -> (c, body)
  Neg e            -> let (c, b) = splitCoeff e in (ratNeg c, b)
  Lit r            -> (r, Lit rOne)
  _                -> (rOne, expr)

--- Group terms by base using an association list, summing coefficients.
groupTermsAL :: [RadExpr Rational]
             -> [(RadExpr Rational, Rational)]
groupTermsAL = foldl insertTerm []

--- Insert a term into the association list, summing if the base exists.
insertTerm :: [(RadExpr Rational, Rational)]
           -> RadExpr Rational
           -> [(RadExpr Rational, Rational)]
insertTerm al term =
  let (c, base) = splitCoeff term
  in insertWithAdd base c al

--- Insert into association list, adding to existing value if key present.
insertWithAdd :: RadExpr Rational -> Rational
              -> [(RadExpr Rational, Rational)]
              -> [(RadExpr Rational, Rational)]
insertWithAdd key val [] = [(key, val)]
insertWithAdd key val ((k,v):rest) =
  if key == k
  then (k, ratAdd v val) : rest
  else (k, v) : insertWithAdd key val rest

--- Apply a coefficient to a base expression (for sum reconstruction).
applyCoeffAdd :: Rational -> RadExpr Rational -> RadExpr Rational
applyCoeffAdd c base
  | ratEq c rOne && isLitOne base  = Lit rOne
  | isLitOne base                  = Lit c
  | ratEq c rOne                   = base
  | ratEq c rNegOne                = Neg base
  | otherwise                      = Mul (Lit c) base

--- Check if expression is Lit 1.
isLitOne :: RadExpr Rational -> Bool
isLitOne expr = case expr of
  Lit r -> ratEq r rOne
  _     -> False

--- Build an Add from a list.
buildAddN :: [RadExpr Rational] -> RadExpr Rational
buildAddN xs = case xs of
  []    -> Lit rZero
  [x]   -> x
  (x:rest) -> foldl Add x rest

--- Sort children of commutative operators into canonical order.
--- This ensures that a + b and b + a normalize to the same expression.
sortCommutative :: RadExpr Rational -> RadExpr Rational
sortCommutative expr = case expr of
  Add _ _ ->
    let terms = flattenAddS expr
        sorted = sort (map sortCommutative terms)
    in buildAddN sorted
  Mul _ _ ->
    let factors = flattenMulS expr
        sorted = sort (map sortCommutative factors)
    in buildMulS sorted
  Neg a    -> Neg (sortCommutative a)
  Inv a    -> Inv (sortCommutative a)
  Root n a -> Root n (sortCommutative a)
  Pow a n  -> Pow (sortCommutative a) n
  e        -> e

--- Flatten Add for sortCommutative.
flattenAddS :: RadExpr Rational -> [RadExpr Rational]
flattenAddS expr = case expr of
  Add a b -> flattenAddS a ++ flattenAddS b
  e       -> [e]

--- Flatten Mul for sortCommutative.
flattenMulS :: RadExpr Rational -> [RadExpr Rational]
flattenMulS expr = case expr of
  Mul a b -> flattenMulS a ++ flattenMulS b
  e       -> [e]

--- Build a Mul from a sorted list.
buildMulS :: [RadExpr Rational] -> RadExpr Rational
buildMulS xs = case xs of
  []    -> Lit rOne
  [x]   -> x
  (x:rest) -> foldl Mul x rest

--- Distribute scalar multiplication over addition.
---
--- Lit c * (a + b) becomes Lit c * a + Lit c * b,
--- enabling collectTerms to combine like terms.
distribute :: RadExpr Rational -> RadExpr Rational
distribute expr = case expr of
  Mul (Lit c) r ->
    if isSum r
    then let terms = flattenS r
         in rebuildAddD (map (\t -> Mul (Lit c) t) terms)
    else case r of
           Neg a -> Neg (distribute (Mul (Lit c) a))
           _     -> Mul (Lit c) (distribute r)
  Mul l (Lit c) ->
    if isSum l
    then let terms = flattenS l
         in rebuildAddD (map (\t -> Mul t (Lit c)) terms)
    else case l of
           Neg a -> Neg (distribute (Mul a (Lit c)))
           _     -> Mul (distribute l) (Lit c)
  Mul l r ->
    if isTinySum l && not (isSum r)
    then let terms = flattenS l
         in rebuildAddD (map (\t -> distribute (Mul t r)) terms)
    else if not (isSum l) && isTinySum r
    then let terms = flattenS r
         in rebuildAddD (map (\t -> distribute (Mul l t)) terms)
    else case (l, r) of
           (Neg a, _) -> Neg (distribute (Mul a r))
           (_, Neg b) -> Neg (distribute (Mul l b))
           _          -> Mul (distribute l) (distribute r)
  Neg a    -> Neg (distribute a)
  Add a b  -> Add (distribute a) (distribute b)
  Inv a    -> Inv (distribute a)
  Root n a -> Root n (distribute a)
  Pow a n  -> Pow (distribute a) n
  e        -> e

--- Check if an expression is a sum (Add or Neg of a sum).
isSum :: RadExpr k -> Bool
isSum expr = case expr of
  Add _ _ -> True
  Neg a   -> isSum a
  _       -> False

--- Check if an expression is a small sum (at most 2 terms).
isTinySum :: RadExpr k -> Bool
isTinySum e = isSum e && length (flattenS e) <= 2

--- Flatten a sum, distributing Neg into terms.
flattenS :: RadExpr k -> [RadExpr k]
flattenS expr = case expr of
  Add a b -> flattenS a ++ flattenS b
  Neg a   -> map Neg (flattenS a)
  e       -> [e]

--- Rebuild an Add from a non-empty list.
rebuildAddD :: [RadExpr k] -> RadExpr k
rebuildAddD xs = case xs of
  []       -> error "rebuildAddD: empty list"
  [x]      -> x
  (x:rest) -> foldl Add x rest
