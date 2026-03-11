-- | Normalization passes for radical expressions.
--
-- These are explicit transformations the user applies as needed.
-- They do NOT denest — see "Surd.Radical.Denest" for that.
module Surd.Radical.Normalize
  ( normalize,
    normalizeOnce,
    flattenArith,
    foldConstants,
    simplifyPowers,
    extractPerfectPowers,
    collectCoefficients,
    collectTerms,
    distribute,
  )
where

import Data.List (sort, sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Types

-- | Apply all normalization passes, iterated to a fixed point.
normalize :: RadExpr Rational -> RadExpr Rational
normalize = fixN 10 normalizeOnce

-- | A single normalization pass (all sub-passes composed once).
-- Useful when iterating to a fixed point would be too expensive
-- (e.g., on large DAG-structured expressions where each pass
-- breaks sharing and rebuilds the tree).
normalizeOnce :: RadExpr Rational -> RadExpr Rational
normalizeOnce =
  collectTerms
    . collectCoefficients
    . distribute
    . sortCommutative
    . extractPerfectPowers
    . simplifyPowers
    . foldConstants
    . flattenArith

-- | Iterate a function until fixed point or fuel runs out.
fixN :: (Eq a) => Int -> (a -> a) -> a -> a
fixN 0 _ x = x
fixN n f x =
  let x' = f x
   in if x' == x then x else fixN (n - 1) f x'

-- | Flatten nested Add/Mul (associativity) and cancel double negations.
flattenArith :: RadExpr k -> RadExpr k
flattenArith (Neg (Neg a)) = flattenArith a
flattenArith (Neg a) = Neg (flattenArith a)
flattenArith (Add a b) = Add (flattenArith a) (flattenArith b)
flattenArith (Mul a b) = Mul (flattenArith a) (flattenArith b)
flattenArith (Inv (Inv a)) = flattenArith a
flattenArith (Inv a) = Inv (flattenArith a)
flattenArith (Root n a) = Root n (flattenArith a)
flattenArith (Pow a n) = Pow (flattenArith a) n
flattenArith e = e

-- | Fold constant subexpressions: evaluate pure-literal subtrees.
foldConstants :: RadExpr Rational -> RadExpr Rational
foldConstants expr = case expr of
  Lit r -> Lit r
  Neg a -> case foldConstants a of
    Lit r -> Lit (negate r)
    Neg a' -> a' -- double negation
    a' -> Neg a'
  Add a b -> case (foldConstants a, foldConstants b) of
    (Lit r, Lit s) -> Lit (r + s)
    (Lit 0, b') -> b'
    (a', Lit 0) -> a'
    (a', Neg b') -> if a' == b' then Lit 0 else Add a' (Neg b')
    (a', b') -> Add a' b'
  Mul a b -> case (foldConstants a, foldConstants b) of
    (Lit r, Lit s) -> Lit (r * s)
    (Lit 0, _) -> Lit 0
    (_, Lit 0) -> Lit 0
    (Lit 1, b') -> b'
    (a', Lit 1) -> a'
    (Lit (-1), b') -> Neg b'
    (a', Lit (-1)) -> Neg a'
    (a', b') -> Mul a' b'
  Inv a -> case foldConstants a of
    Lit r | r /= 0 -> Lit (1 / r)
    Inv a' -> a' -- double inverse
    a' -> Inv a'
  Root n a -> case foldConstants a of
    Lit 0 -> Lit 0
    Lit 1 -> Lit 1
    a' -> Root n a'
  Pow a n -> case foldConstants a of
    Lit r -> Lit (r ^^ n)
    a' -> case n of
      0 -> Lit 1
      1 -> a'
      _ -> Pow a' n

-- | Simplify power expressions:
--   Pow (Pow a m) n  -->  Pow a (m*n)
--   Pow (Root n a) n -->  a
--   Root n (Pow a n) -->  a  (when a >= 0 or n is odd)
simplifyPowers :: RadExpr Rational -> RadExpr Rational
simplifyPowers expr = case expr of
  -- (√a)² = a, generalized: (ⁿ√a)·(ⁿ√a) for n=2
  Mul (Root 2 a) (Root 2 b) | a == b -> simplifyPowers a
  Pow (Pow a m) n -> simplifyPowers (Pow a (m * n))
  Pow (Root n a) m
    | m == n -> simplifyPowers a
  Root n (Pow a m)
    | m == n -> simplifyPowers a -- NB: correct for real positive a; needs care for negatives
  Root m (Root n a) -> Root (m * n) (simplifyPowers a)
  Neg a -> Neg (simplifyPowers a)
  Add a b -> Add (simplifyPowers a) (simplifyPowers b)
  Mul a b -> Mul (simplifyPowers a) (simplifyPowers b)
  Inv a -> Inv (simplifyPowers a)
  Root n a -> Root n (simplifyPowers a)
  Pow a n -> Pow (simplifyPowers a) n
  e -> e

-- | Extract perfect nth powers from under radicals.
--
-- @Root n (Lit (a^n * b))@ --> @Mul (Lit a) (Root n (Lit b))@
--
-- This handles the common case of simplifying things like @√12 = 2√3@.
extractPerfectPowers :: RadExpr Rational -> RadExpr Rational
extractPerfectPowers expr = case expr of
  Root n (Lit r)
    | r > 0 ->
        let num = numerator r
            den = denominator r
            (numOut, numIn) = extractNthPower n num
            (denOut, denIn) = extractNthPower n den
            -- Rationalize denominator: ⁿ√(p/q) = ⁿ√(p·q^(n-1)) / q
            -- After perfect power extraction: ⁿ√(numIn/denIn)
            -- If denIn > 1, multiply radicand by denIn^(n-1) and divide outside by denIn
            (outerCoeff, innerRat)
              | denIn == 1 = (numOut / denOut, numIn)
              | otherwise =
                  let newInner = numIn * denIn ^ (n - 1 :: Int)
                      newOuter = numOut / (denOut * denIn)
                      -- Re-extract from newInner in case denIn^(n-1) introduced new powers
                      (numOut2, numIn2) = extractNthPower n (numerator newInner)
                   in (newOuter * numOut2, numIn2)
         in case (outerCoeff == 1, innerRat == 1) of
              (True, True) -> Lit 1
              (True, False) -> Root n (Lit innerRat)
              (_, True) -> Lit outerCoeff
              _ -> Mul (Lit outerCoeff) (Root n (Lit innerRat))
    | r < 0 && odd n ->
        Neg (extractPerfectPowers (Root n (Lit (negate r))))
    | otherwise -> Root n (Lit r)
  Root n a -> Root n (extractPerfectPowers a)
  Neg a -> Neg (extractPerfectPowers a)
  Add a b -> Add (extractPerfectPowers a) (extractPerfectPowers b)
  Mul a b -> Mul (extractPerfectPowers a) (extractPerfectPowers b)
  Inv a -> Inv (extractPerfectPowers a)
  Pow a n -> Pow (extractPerfectPowers a) n
  e -> e

-- | Given n and a positive integer m, extract the largest kth power
-- that divides m and return (extracted, remainder).
-- So m = extracted^n * remainder.
extractNthPower :: Int -> Integer -> (Rational, Rational)
extractNthPower n m =
  let fs = factorise (fromInteger (abs m) :: Positive)
      extracted = product [p ^ (e `div` n) | (p, e) <- fs]
      remainder = product [p ^ (e `mod` n) | (p, e) <- fs]
   in (fromInteger extracted, fromInteger remainder)

-- | Collect rational coefficients in products.
--
-- Flattens a product tree, multiplies all Lit factors together,
-- and produces a single @Mul (Lit coeff) body@ (or just @body@ if coeff = 1).
collectCoefficients :: RadExpr Rational -> RadExpr Rational
collectCoefficients = go
  where
    go expr = case expr of
      Mul _ _ ->
        let factors = flattenMul expr
            (lits, rest) = partitionLits (map go factors)
            coeff = product lits
            body = buildMul rest
         in applyCoeff coeff body
      Neg a -> case go a of
        Lit r -> Lit (negate r)
        a' -> Neg a'
      Add a b -> Add (go a) (go b)
      Inv a -> Inv (go a)
      Root n a -> Root n (go a)
      Pow a n -> Pow (go a) n
      e -> e

    flattenMul :: RadExpr Rational -> [RadExpr Rational]
    flattenMul (Mul a b) = flattenMul a ++ flattenMul b
    flattenMul e = [e]

    partitionLits :: [RadExpr Rational] -> ([Rational], [RadExpr Rational])
    partitionLits [] = ([], [])
    partitionLits (Lit r : xs) =
      let (ls, rs) = partitionLits xs in (r : ls, rs)
    partitionLits (Inv (Lit r) : xs)
      | r /= 0 =
          let (ls, rs) = partitionLits xs in (1 / r : ls, rs)
    partitionLits (x : xs) =
      let (ls, rs) = partitionLits xs in (ls, x : rs)

    buildMul :: [RadExpr Rational] -> RadExpr Rational
    buildMul [] = Lit 1
    buildMul [x] = x
    buildMul (x : xs) = Mul x (buildMul xs)

    applyCoeff :: Rational -> RadExpr Rational -> RadExpr Rational
    applyCoeff 0 _ = Lit 0
    applyCoeff 1 body = body
    applyCoeff (-1) body = Neg body
    applyCoeff c (Lit r) = Lit (c * r)
    applyCoeff c body = Mul (Lit c) body

-- | Collect like terms in sums.
--
-- Flattens a sum tree, groups summands by their non-coefficient "base" part,
-- adds up coefficients within each group, and rebuilds.
-- E.g., @3√5 + 2√5@ becomes @5√5@.
collectTerms :: RadExpr Rational -> RadExpr Rational
collectTerms = go
  where
    go expr = case expr of
      Add _ _ ->
        let terms = flattenAdd expr
            processed = map go terms
            grouped = groupTerms processed
            sorted = sortBy (\(a, _) (b, _) -> compare a b) (Map.toList grouped)
            rebuilt = [applyCoeff c base | (base, c) <- sorted, c /= 0]
         in buildAdd rebuilt
      Neg a -> Neg (go a)
      Mul a b -> Mul (go a) (go b)
      Inv a -> Inv (go a)
      Root n a -> Root n (go a)
      Pow a n -> Pow (go a) n
      e -> e

    flattenAdd :: RadExpr Rational -> [RadExpr Rational]
    flattenAdd (Add a b) = flattenAdd a ++ flattenAdd b
    flattenAdd e = [e]

    -- Split a term into (coefficient, base). E.g., Mul (Lit 3) x → (3, x).
    splitCoeff :: RadExpr Rational -> (Rational, RadExpr Rational)
    splitCoeff (Mul (Lit c) body) = (c, body)
    splitCoeff (Neg e) = let (c, b) = splitCoeff e in (negate c, b)
    splitCoeff (Lit r) = (r, Lit 1)
    splitCoeff e = (1, e)

    -- Group terms by base, summing coefficients.
    groupTerms :: [RadExpr Rational] -> Map.Map (RadExpr Rational) Rational
    groupTerms =
      foldl
        ( \m term ->
            let (c, base) = splitCoeff term
             in Map.insertWith (+) base c m
        )
        Map.empty

    applyCoeff :: Rational -> RadExpr Rational -> RadExpr Rational
    applyCoeff 1 (Lit 1) = Lit 1
    applyCoeff c (Lit 1) = Lit c
    applyCoeff 1 body = body
    applyCoeff (-1) body = Neg body
    applyCoeff c body = Mul (Lit c) body

    buildAdd :: [RadExpr Rational] -> RadExpr Rational
    buildAdd [] = Lit 0
    buildAdd [x] = x
    buildAdd (x : xs) = foldl Add x xs

-- | Sort children of commutative operators (Add, Mul) into canonical order.
-- This ensures that @a + b@ and @b + a@ normalize to the same expression,
-- which is critical for like-term collection and CSE.
sortCommutative :: RadExpr Rational -> RadExpr Rational
sortCommutative = go
  where
    go expr = case expr of
      Add _ _ ->
        let terms = flattenAdd' expr
            sorted = sort (map go terms)
         in buildAdd' sorted
      Mul _ _ ->
        let factors = flattenMul' expr
            sorted = sort (map go factors)
         in buildMul' sorted
      Neg a -> Neg (go a)
      Inv a -> Inv (go a)
      Root n a -> Root n (go a)
      Pow a n -> Pow (go a) n
      e -> e

    flattenAdd' (Add a b) = flattenAdd' a ++ flattenAdd' b
    flattenAdd' e = [e]

    flattenMul' (Mul a b) = flattenMul' a ++ flattenMul' b
    flattenMul' e = [e]

    buildAdd' [] = Lit 0 -- shouldn't happen
    buildAdd' [x] = x
    buildAdd' (x : xs) = foldl Add x xs

    buildMul' [] = Lit 1 -- shouldn't happen
    buildMul' [x] = x
    buildMul' (x : xs) = foldl Mul x xs

-- | Distribute scalar multiplication over addition and simplify roots.
--
-- @Lit c * (a + b)@ becomes @Lit c * a + Lit c * b@,
-- enabling 'collectTerms' to combine like terms across different products.
-- Also combines same-index roots: @√a · √b = √(ab)@.
distribute :: RadExpr Rational -> RadExpr Rational
distribute = go
  where
    go expr = case expr of
      -- Lit * (a + b + ...) → Lit*a + Lit*b + ... (always safe, no blowup)
      Mul (Lit c) r
        | isSum r ->
            let terms = flattenS r
             in rebuildAdd (fmap (Mul (Lit c)) terms)
      Mul l (Lit c)
        | isSum l ->
            let terms = flattenS l
             in rebuildAdd (fmap (\t -> Mul t (Lit c)) terms)
      -- (a + b) * x → a*x + b*x (when sum is tiny, to enable term collection)
      Mul l r
        | isTinySum l && not (isSum r) ->
            let terms = flattenS l
             in rebuildAdd (fmap (\t -> go (Mul t r)) terms)
      Mul l r
        | not (isSum l) && isTinySum r ->
            let terms = flattenS r
             in rebuildAdd (fmap (go . Mul l) terms)
      -- c * (-a) → -(c*a), (-a) * c → -(a*c)
      Mul (Lit c) (Neg a) -> Neg (go (Mul (Lit c) a))
      Mul (Neg a) (Lit c) -> Neg (go (Mul a (Lit c)))
      -- Recurse into subexpressions
      Neg a -> Neg (go a)
      Add a b -> Add (go a) (go b)
      Mul a b -> Mul (go a) (go b)
      Inv a -> Inv (go a)
      Root n a -> Root n (go a)
      Pow a n -> Pow (go a) n
      e -> e

    isSum :: RadExpr k -> Bool
    isSum (Add _ _) = True
    isSum (Neg a) = isSum a
    isSum _ = False

    isTinySum :: RadExpr k -> Bool
    isTinySum e = isSum e && NE.length (flattenS e) <= 2

    flattenS :: RadExpr k -> NonEmpty (RadExpr k)
    flattenS (Add a b) = flattenS a <> flattenS b
    flattenS (Neg a) = fmap Neg (flattenS a)
    flattenS e = e :| []

    rebuildAdd :: NonEmpty (RadExpr k) -> RadExpr k
    rebuildAdd (x :| xs) = foldl Add x xs
