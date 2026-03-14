-- |
-- Module      : Surd.Radical.NormalForm
-- Description : Canonical normal form for radical expressions as Q-linear combinations of monomials
-- Stability   : experimental
--
-- Every expression in @Q[sqrt 2, sqrt 3, cbrt 5, ...]@ has a unique
-- representation as a sum of @(rational coefficient * monomial)@, where
-- each monomial is a product of radical /atoms/ raised to bounded powers.
--
-- This representation gives automatic like-term collection, canonical
-- ordering, and reliable structural equality -- all of which are
-- expensive or unreliable on the tree-based 'RadExpr'.
--
-- === Mathematical description
--
-- A 'NormExpr' is an element of the Q-vector space spanned by monomials
-- of the form @product(atom_i ^ e_i)@ where:
--
-- * Each 'Atom' is either a rational nth root ('RatRoot'), the imaginary
--   unit ('ImagUnit'), or a nested root ('NestedRoot') whose radicand is
--   itself a multi-term expression.
-- * Exponents are bounded: for @RatRoot n r@, @0 <= e < n@ (since
--   @(nth-root r)^n = r@ reduces to a coefficient). For 'ImagUnit',
--   @e in {0, 1}@ (since @i^2 = -1@).
-- * Coefficients are rational (from Q).
--
-- === Integral radical canonicalization
--
-- 'NestedRoot' radicands are canonicalized to coprime integer coefficients
-- (Besicovitch/Zippel form). The LCD is cleared, GCD content extracted,
-- and perfect nth powers factored out. For example:
--
-- @cbrt(-(7/432) + (7/144)*sqrt 3)@ becomes @(1/12) * cbrt 28 * cbrt(-1 + 3*sqrt 3)@.
module Surd.Radical.NormalForm
  ( -- * Types
    Atom (..),
    Monomial (..),
    NormExpr (..),

    -- * Construction
    normLit,
    normAtom,
    normRoot,

    -- * Arithmetic
    normAdd,
    normSub,
    normNeg,
    normMul,
    normScale,
    normPow,
    normInv,

    -- * Conversion
    toNormExpr,
    fromNormExpr,

    -- * Queries
    normIsZero,
    normCoeff,
  )
where

import Control.Exception (SomeException, evaluate, try)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Math.Internal.Interval (strictlyNegative)
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Radical.Eval (eval, evalInterval)
import Surd.Types (RadExpr (..))
import System.IO.Unsafe (unsafePerformIO)

-- | A radical atom: an irreducible nth root of a positive rational,
-- the imaginary unit, or a nested root of a multi-term radicand.
--
-- Negative radicands are factored as @sqrt(-r) = i * sqrt r@ where
-- @i@ is represented by 'ImagUnit'.
--
-- Invariant: the rational in 'RatRoot' is @> 0@ and nth-power-free
-- (no perfect nth power factors remain).
data Atom
  = -- | @RatRoot n r@: the principal nth root of a positive rational @r@.
    -- Invariant: @r > 0@ and @r@ is nth-power-free (e.g., square-free for @n=2@).
    RatRoot !Int !Rational
  | -- | The imaginary unit @i = sqrt(-1)@.
    -- Kept as a separate atom because @i^2 = -1@ is the key reduction rule
    -- (rather than treating it as @RatRoot 2 (-1)@ which would violate the
    -- positivity invariant).
    ImagUnit
  | -- | @NestedRoot n e@: the principal nth root of a multi-term expression @e@
    -- that cannot be decomposed into a product of simpler atoms.
    -- Exponent reduction applies: @(NestedRoot n e)^n@ extracts the radicand
    -- as a NormExpr and multiplies it in.
    NestedRoot !Int !(RadExpr Rational)
  deriving (Eq, Ord, Show)

-- | A monomial: a product of atoms raised to positive powers.
--
-- The empty map represents the multiplicative identity (1).
-- Invariant: all exponents are in @[1, n-1]@ for @RatRoot n r@ atoms
-- (since @(nth-root r)^n = r@, which reduces to a coefficient).
-- For 'ImagUnit', exponents are in @{0, 1}@ (since @i^2 = -1@).
newtype Monomial = Monomial {unMonomial :: Map Atom Int}
  deriving (Eq, Ord, Show)

-- | A normalized expression: a Q-linear combination of 'Monomial's.
--
-- Invariant: no zero coefficients appear in the map.
-- The empty map represents zero.
newtype NormExpr = NormExpr {unNormExpr :: Map Monomial Rational}
  deriving (Eq, Ord, Show)

-- | The zero expression.
normZero :: NormExpr
normZero = NormExpr Map.empty

-- | Construct a rational literal as a 'NormExpr'.
--
-- @normLit 0@ returns the zero expression (empty map).
normLit :: Rational -> NormExpr
normLit 0 = normZero
normLit r = NormExpr (Map.singleton unitMono r)

-- | The unit monomial (represents 1).
unitMono :: Monomial
unitMono = Monomial Map.empty

-- | Construct a 'NormExpr' containing a single atom with coefficient 1.
normAtom :: Atom -> NormExpr
normAtom a = NormExpr (Map.singleton (Monomial (Map.singleton a 1)) 1)

-- | Construct a normalized nth root of a rational number.
--
-- Extracts perfect nth powers and handles sign: @sqrt 12@ becomes
-- @2 * sqrt 3@, @sqrt(-5)@ becomes @i * sqrt 5@.
normRoot :: Int -> Rational -> NormExpr
normRoot n r
  | r == 0 = normZero
  | r == 1 = normLit 1
  | r < 0 && n == 2 =
      -- sqrt(-r) = i * sqrt(r)
      normMul (normAtom ImagUnit) (normRoot 2 (negate r))
  | r < 0 && even n =
      -- Even root of negative: i * nth-root(r)
      normMul (normAtom ImagUnit) (normRoot n (negate r))
  | r < 0 && odd n =
      -- Odd root of negative: -(nth-root(-r))
      normNeg (normRoot n (negate r))
  | otherwise =
      let num = numerator r
          den = denominator r
          (numOut, numIn) = extractNthPower n (abs num)
          (denOut, denIn) = extractNthPower n den
          -- Rationalize: nth-root(a/b) = (1/b) * nth-root(a * b^(n-1))
          coeff = numOut / (denOut * denIn)
          radicand = numIn * denIn ^ (n - 1)
       in if radicand == 1
            then normLit coeff
            else normScale coeff (normAtom (RatRoot n radicand))

-- | Compute the nth root of an atom raised to a power.
--
-- For @RatRoot m r@: @nth-root((mth-root r)^e) = ((m*n)th-root r)^e@
-- (composing roots). For 'ImagUnit': reduces @i^e@ first, then takes
-- the nth root.
rootOfAtomPow :: Int -> Atom -> Int -> NormExpr
rootOfAtomPow n (RatRoot m r) e =
  -- (mth-root r)^e under an nth root: nth-root((mth-root r)^e) = ((m*n)th-root r)^e
  -- This creates an atom with combined root index m*n.
  -- reduceMonomial will handle if e >= m*n.
  reduceMonomial (Monomial (Map.singleton (RatRoot (m * n) r) e)) 1
rootOfAtomPow n (NestedRoot m inner) e =
  -- nth-root((mth-root e)^k): compose roots -> ((m*n)th-root e)^k
  reduceMonomial (Monomial (Map.singleton (NestedRoot (m * n) inner) e)) 1
rootOfAtomPow n ImagUnit e =
  -- i^e under an nth root: nth-root(i^e)
  -- Reduce i^e first, then take nth root of the result.
  let e' = e `mod` 4
   in case e' of
        0 -> normLit 1 -- nth-root(1) = 1
        2 -> normRoot n (-1) -- nth-root(i^2) = nth-root(-1)
        _ ->
          -- nth-root(i) and nth-root(-i) require (2n)th roots of -1.
          -- i = sqrt(-1), so nth-root(i) = (2n)th-root(-1).
          -- -i = -(sqrt(-1)), so nth-root(-i) = -(2n)th-root(-1) when n is odd.
          let base = normRoot (2 * n) (-1) -- (2n)th-root(-1)
           in if e' == 1 then base else normNeg base

-- | Extract the largest nth power factor from a positive integer.
--
-- Returns @(extracted, remainder)@ such that @m = extracted^n * remainder@,
-- where @remainder@ is nth-power-free.
extractNthPower :: Int -> Integer -> (Rational, Rational)
extractNthPower n m
  | m == 0 = (0, 0)
  | m == 1 = (1, 1)
  | otherwise =
      let fs = factorise (fromInteger (abs m) :: Positive)
          extracted = product [p ^ (e `div` n) | (p, e) <- fs]
          remainder = product [p ^ (e `mod` n) | (p, e) <- fs]
       in (fromInteger extracted, fromInteger remainder)

-- | Add two normal form expressions.
--
-- Merges the monomial maps, summing coefficients for matching monomials
-- and filtering out any that become zero.
normAdd :: NormExpr -> NormExpr -> NormExpr
normAdd (NormExpr a) (NormExpr b) =
  NormExpr $ Map.filter (/= 0) $ Map.unionWith (+) a b

-- | Subtract two normal form expressions: @normSub a b = normAdd a (normNeg b)@.
normSub :: NormExpr -> NormExpr -> NormExpr
normSub a b = normAdd a (normNeg b)

-- | Negate a normal form expression (negate all coefficients).
normNeg :: NormExpr -> NormExpr
normNeg (NormExpr m) = NormExpr (Map.map negate m)

-- | Scale a normal form expression by a rational constant.
--
-- @normScale 0 _@ returns zero. Otherwise multiplies all coefficients by @c@.
normScale :: Rational -> NormExpr -> NormExpr
normScale 0 _ = normZero
normScale c (NormExpr m) = NormExpr $ Map.filter (/= 0) $ Map.map (* c) m

-- | Multiply two normal form expressions.
--
-- Distributes multiplication over sums (all pairs of monomials from @a@
-- and @b@), reducing exponents via 'reduceMonomial' after each product.
normMul :: NormExpr -> NormExpr -> NormExpr
normMul (NormExpr a) (NormExpr b) =
  let terms =
        [ mulMonoCoeff (m1, c1) (m2, c2)
          | (m1, c1) <- Map.toList a,
            (m2, c2) <- Map.toList b
        ]
   in foldl normAdd normZero terms

-- | Multiply two monomial-coefficient pairs, reducing exponents.
mulMonoCoeff :: (Monomial, Rational) -> (Monomial, Rational) -> NormExpr
mulMonoCoeff (Monomial m1, c1) (Monomial m2, c2) =
  let merged = Map.unionWith (+) m1 m2
      coeff = c1 * c2
   in reduceMonomial (Monomial merged) coeff

-- | Reduce a monomial by applying the reduction rules:
--
-- * @(nth-root r)^n = r@ (extracts radicand as coefficient)
-- * @i^2 = -1@ (reduces imaginary unit powers)
-- * @(NestedRoot n e)^n@ extracts the radicand as a 'NormExpr' factor
--
-- Returns a 'NormExpr' because reduction may produce sums (when a
-- 'NestedRoot' radicand is multi-term).
reduceMonomial :: Monomial -> Rational -> NormExpr
reduceMonomial (Monomial atoms) coeff =
  let (coeff', atoms') = Map.foldlWithKey' reduceAtom (coeff, Map.empty) atoms
   in if coeff' == 0
        then normZero
        else reduceNestedRoots coeff' atoms'

-- | Post-reduction pass: extract full powers of NestedRoot atoms.
-- When (NestedRoot n e)^k with k >= n, extract (toNormExpr e)^(k/n)
-- and keep the remainder as the atom exponent.
reduceNestedRoots :: Rational -> Map Atom Int -> NormExpr
reduceNestedRoots coeff atoms =
  case [(a, e) | (a@(NestedRoot n _), e) <- Map.toList atoms, e >= n] of
    [] -> NormExpr (Map.singleton (Monomial atoms) coeff)
    (NestedRoot n inner, e) : _ ->
      let (full, rem') = e `divMod` n
          atoms' =
            if rem' == 0
              then Map.delete (NestedRoot n inner) atoms
              else Map.insert (NestedRoot n inner) rem' atoms
          base = NormExpr (Map.singleton (Monomial atoms') coeff)
          innerNorm = normPow (toNormExpr inner) full
       in normMul base innerNorm
    _ -> NormExpr (Map.singleton (Monomial atoms) coeff)

-- | Reduce a single atom's exponent, extracting factors.
reduceAtom :: (Rational, Map Atom Int) -> Atom -> Int -> (Rational, Map Atom Int)
reduceAtom (c, m) ImagUnit e =
  -- i^0 = 1, i^1 = i, i^2 = -1, i^3 = -i (mod 4)
  let e' = e `mod` 4
      (signFactor, hasI) = case e' of
        0 -> (1, False)
        1 -> (1, True)
        2 -> (-1, False)
        3 -> (-1, True)
        _ -> (1, False)
      c' = c * signFactor
      m' = if hasI then Map.insert ImagUnit 1 m else m
   in (c', m')
reduceAtom (c, m) atom@(RatRoot n r) e =
  let (full, rem') = e `divMod` n
      -- Each full power of n extracts the radicand r
      c' = c * r ^^ full
      m' = if rem' == 0 then m else Map.insert atom rem' m
   in (c', m')
reduceAtom (c, m) atom@(NestedRoot _ _) e
  | e == 0 = (c, m) -- NestedRoot^0 = 1, drop it
  -- NestedRoot reduction is handled by reduceNestedRoots (post-pass)
  -- because extracting the radicand produces a NormExpr, not a Rational.
  | otherwise = (c, Map.insert atom e m)

-- | Raise a normal form expression to a non-negative integer power.
--
-- Uses binary exponentiation for efficiency. Negative exponents
-- should use 'normInv' instead.
normPow :: NormExpr -> Int -> NormExpr
normPow _ 0 = normLit 1
normPow e 1 = e
normPow e n
  | n < 0 = error "normPow: negative exponent (use Inv in RadExpr)"
  | even n = let half = normPow e (n `div` 2) in normMul half half
  | otherwise = normMul e (normPow e (n - 1))

-- | Compute the multiplicative inverse of a normal form expression.
--
-- For a single monomial @c * m@, inverts directly: @(1/c) * m^(-1)@
-- where @m^(-1)@ is computed by negating exponents and applying the
-- reduction rules @(nth-root r)^n = r@ and @i^4 = 1@.
--
-- For multi-term expressions (sums), iteratively rationalizes by
-- picking a radical atom @alpha@, viewing the expression as a polynomial
-- in @alpha@ over the sub-field generated by the remaining atoms, and
-- computing the inverse in the quotient ring @Q(...)[alpha]/(alpha^n - r)@
-- via the extended Euclidean algorithm on polynomials with 'NormExpr'
-- coefficients.
normInv :: NormExpr -> NormExpr
normInv ne = case Map.toList (unNormExpr ne) of
  [] -> error "normInv: division by zero"
  -- Single monomial: c * m -> (1/c) * m^(-1)
  [(mono, c)] -> normScale (1 / c) (invertMonomial mono)
  -- Multi-term: rationalize iteratively
  _ -> rationalizeInv ne

-- | Invert a monomial by negating all exponents.
-- reduceMonomial handles the modular arithmetic:
--   (nth-root r)^(-e) mod (nth-root r)^n = r  gives  (nth-root r)^(n-e) / r
--   i^(-e) mod i^4 = 1  gives the appropriate sign and power.
invertMonomial :: Monomial -> NormExpr
invertMonomial (Monomial atoms) =
  reduceMonomial (Monomial (Map.map negate atoms)) 1

-- | Rationalize the inverse of a multi-term NormExpr.
--
-- Strategy: pick a radical atom alpha = nth-root(r) appearing in the expression.
-- View the expression as a polynomial f(alpha) in Q(other atoms)[alpha].
-- The minimal polynomial of alpha is alpha^n - r.
-- Compute g(alpha) such that f(alpha)*g(alpha) = 1 (mod alpha^n - r)
-- using the extended Euclidean algorithm on polynomials.
-- The coefficients of g are NormExprs in the remaining atoms.
rationalizeInv :: NormExpr -> NormExpr
rationalizeInv ne =
  case findRadicalAtom ne of
    Nothing ->
      -- Purely rational: shouldn't reach here (handled by single-monomial case)
      -- but just in case:
      case normCoeff ne of
        Just r | r /= 0 -> normLit (1 / r)
        _ -> error "rationalizeInv: cannot invert"
    Just atom ->
      -- Express ne as polynomial in atom: ne = a_0 + a_1*alpha + a_2*alpha^2 + ...
      let n = atomDegree atom
          polyCoeffs = toAtomPoly atom ne -- [a_0, a_1, ..., a_{n-1}]
          minPoly = minPolyCoeffs n atom -- [negated radicand, 0, ..., 0, 1]
          -- Extended GCD to find inverse polynomial
          (_, inv, _) = normPolyExtGcd polyCoeffs minPoly
       in -- Rebuild: inv_0 + inv_1*alpha + inv_2*alpha^2 + ...
          fromAtomPoly atom inv

-- | Find any radical atom appearing in a NormExpr.
findRadicalAtom :: NormExpr -> Maybe Atom
findRadicalAtom (NormExpr m) =
  case concatMap (Map.keys . unMonomial . fst) (Map.toList m) of
    [] -> Nothing
    (a : _) -> Just a

-- | Degree of an atom's minimal polynomial.
atomDegree :: Atom -> Int
atomDegree ImagUnit = 2 -- i^2 + 1 = 0
atomDegree (RatRoot n _) = n -- alpha^n - r = 0
atomDegree (NestedRoot n _) = n -- alpha^n - e = 0

-- | Express a NormExpr as a polynomial in a given atom.
-- Returns coefficients [a_0, a_1, ..., a_{n-1}] where ne = sum(a_i * atom^i).
-- Each a_i is a NormExpr not containing the atom.
toAtomPoly :: Atom -> NormExpr -> [NormExpr]
toAtomPoly atom (NormExpr m) =
  let n = atomDegree atom
      -- For each monomial, extract the power of atom and the remainder
      grouped =
        Map.foldlWithKey'
          ( \acc mono c ->
              let (power, rest) = extractAtomPower atom mono
                  idx = power `mod` n
               in Map.insertWith normAdd idx (NormExpr (Map.singleton rest c)) acc
          )
          Map.empty
          m
   in -- Build coefficient list
      [Map.findWithDefault normZero i grouped | i <- [0 .. n - 1]]

-- | Extract the power of a specific atom from a monomial,
-- returning (power, monomial without that atom).
extractAtomPower :: Atom -> Monomial -> (Int, Monomial)
extractAtomPower atom (Monomial atoms) =
  case Map.lookup atom atoms of
    Nothing -> (0, Monomial atoms)
    Just e -> (e, Monomial (Map.delete atom atoms))

-- | Minimal polynomial coefficients for an atom.
-- For RatRoot n r: alpha^n - r = 0, coefficients [-r, 0, ..., 0, 1]
-- For ImagUnit: alpha^2 + 1 = 0, coefficients [1, 0, 1]
minPolyCoeffs :: Int -> Atom -> [NormExpr]
minPolyCoeffs _ ImagUnit =
  [normLit 1, normZero, normLit 1] -- alpha^2 + 1
minPolyCoeffs n (RatRoot _ r) =
  [normLit (negate r)] ++ replicate (n - 1) normZero ++ [normLit 1] -- alpha^n - r
minPolyCoeffs n (NestedRoot _ inner) =
  [normNeg (toNormExpr inner)] ++ replicate (n - 1) normZero ++ [normLit 1] -- alpha^n - e

-- | Reconstruct a NormExpr from polynomial coefficients in an atom.
-- Given [a_0, a_1, ...], computes a_0 + a_1*atom + a_2*atom^2 + ...
fromAtomPoly :: Atom -> [NormExpr] -> NormExpr
fromAtomPoly _ [] = normZero
fromAtomPoly atom coeffs =
  foldl
    normAdd
    normZero
    [normMul c (atomPow atom i) | (i, c) <- zip [0 ..] coeffs]

-- | Compute atom^k as a NormExpr (without reduction -- the monomial
-- will be reduced when multiplied).
atomPow :: Atom -> Int -> NormExpr
atomPow _ 0 = normLit 1
atomPow a k = NormExpr (Map.singleton (Monomial (Map.singleton a k)) 1)

-- --------------------------------------------------------------------------
-- Polynomial arithmetic over NormExpr coefficients
-- --------------------------------------------------------------------------

-- | Extended GCD for polynomials with NormExpr coefficients.
-- Returns (gcd, s, t) such that gcd = s*a + t*b.
normPolyExtGcd :: [NormExpr] -> [NormExpr] -> ([NormExpr], [NormExpr], [NormExpr])
normPolyExtGcd a b = go a b [normLit 1] [normZero] [normZero] [normLit 1]
  where
    go r0 r1 s0 s1 t0 t1
      | normPolyIsZero r1 =
          -- Make monic: divide by leading coefficient
          case normPolyLeadCoeff r0 of
            Nothing -> ([normLit 1], s0, t0)
            Just lc ->
              let lcInv = normInv lc
               in ( normPolyScale lcInv r0,
                    normPolyScale lcInv s0,
                    normPolyScale lcInv t0
                  )
      | otherwise =
          let (q, r) = normPolyDivMod r0 r1
              s2 = normPolySub s0 (normPolyMul q s1)
              t2 = normPolySub t0 (normPolyMul q t1)
           in go r1 r s1 s2 t1 t2

-- | Polynomial addition (NormExpr coefficients).
normPolyAdd :: [NormExpr] -> [NormExpr] -> [NormExpr]
normPolyAdd [] bs = bs
normPolyAdd as [] = as
normPolyAdd (a : as) (b : bs) = normAdd a b : normPolyAdd as bs

-- | Polynomial subtraction.
normPolySub :: [NormExpr] -> [NormExpr] -> [NormExpr]
normPolySub a b = normPolyAdd a (map normNeg b)

-- | Polynomial multiplication.
normPolyMul :: [NormExpr] -> [NormExpr] -> [NormExpr]
normPolyMul [] _ = []
normPolyMul _ [] = []
normPolyMul as bs =
  foldl normPolyAdd [] [shift i (map (normMul a) bs) | (i, a) <- zip [0 ..] as]
  where
    shift n xs = replicate n normZero ++ xs

-- | Scale a polynomial by a NormExpr.
normPolyScale :: NormExpr -> [NormExpr] -> [NormExpr]
normPolyScale c = map (normMul c)

-- | Polynomial division with remainder.
normPolyDivMod :: [NormExpr] -> [NormExpr] -> ([NormExpr], [NormExpr])
normPolyDivMod num den
  | normPolyIsZero den = error "normPolyDivMod: division by zero"
  | degNum < degDen = ([], num)
  | otherwise = go (replicate (degNum - degDen + 1) normZero) num
  where
    degNum = normPolyDeg num
    degDen = normPolyDeg den
    lcDen = case normPolyLeadCoeff den of
      Just c -> c
      Nothing -> error "normPolyDivMod: zero divisor"
    lcDenInv = normInv lcDen

    go q r
      | normPolyIsZero r || normPolyDeg r < degDen = (normPolyTrim q, normPolyTrim r)
      | otherwise =
          let dr = normPolyDeg r
              lcR = case normPolyLeadCoeff r of
                Just c -> c
                Nothing -> error "normPolyDivMod: unexpected zero"
              coeff = normMul lcR lcDenInv
              shift = dr - degDen
              q' = normPolySetCoeff shift coeff q
              sub = [if i == shift then coeff else normZero | i <- [0 .. shift]]
              r' = normPolyTrim $ normPolySub r (normPolyMul sub den)
           in go q' r'

-- | Set a coefficient in a polynomial (represented as list).
normPolySetCoeff :: Int -> NormExpr -> [NormExpr] -> [NormExpr]
normPolySetCoeff i c xs =
  let padded = xs ++ replicate (i + 1 - length xs) normZero
   in take i padded ++ [c] ++ drop (i + 1) padded

-- | Degree of a polynomial (index of highest non-zero coefficient).
normPolyDeg :: [NormExpr] -> Int
normPolyDeg = go (-1) 0
  where
    go d _ [] = d
    go d i (x : xs) = go (if normIsZero x then d else i) (i + 1) xs

-- | Leading coefficient (coefficient of highest degree term).
normPolyLeadCoeff :: [NormExpr] -> Maybe NormExpr
normPolyLeadCoeff xs =
  let d = normPolyDeg xs
   in if d < 0 then Nothing else Just (xs !! d)

-- | Is the polynomial zero?
normPolyIsZero :: [NormExpr] -> Bool
normPolyIsZero = all normIsZero

-- | Remove trailing zero coefficients.
normPolyTrim :: [NormExpr] -> [NormExpr]
normPolyTrim = reverse . dropWhile normIsZero . reverse

-- | Test whether a 'NormExpr' is zero (empty monomial map).
normIsZero :: NormExpr -> Bool
normIsZero (NormExpr m) = Map.null m

-- | Extract the rational coefficient if the expression is purely rational
-- (no radical atoms). Returns 'Nothing' if the expression contains any
-- radical atoms.
normCoeff :: NormExpr -> Maybe Rational
normCoeff (NormExpr m) = case Map.toList m of
  [] -> Just 0
  [(Monomial a, c)] | Map.null a -> Just c
  _ -> Nothing

-- --------------------------------------------------------------------------
-- Conversion from RadExpr
-- --------------------------------------------------------------------------

-- | Convert a 'RadExpr' to normal form.
--
-- Handles all constructors including 'Inv' (via 'normInv' rationalization)
-- and negative 'Pow' (via inversion). Multi-term radicands under 'Root'
-- are canonicalized to coprime integer coefficients (Besicovitch/Zippel form):
-- LCD cleared, GCD content extracted, perfect nth powers factored out.
--
-- For even roots of negative radicands, factors out 'ImagUnit':
-- @sqrt(-x)@ becomes @i * sqrt(x)@.
toNormExpr :: RadExpr Rational -> NormExpr
toNormExpr (Lit r) = normLit r
toNormExpr (Neg a) = normNeg (toNormExpr a)
toNormExpr (Add a b) = normAdd (toNormExpr a) (toNormExpr b)
toNormExpr (Mul a b) = normMul (toNormExpr a) (toNormExpr b)
toNormExpr (Root n (Lit r)) = normRoot n r
toNormExpr (Root n a) =
  -- For non-literal radicands, first normalize the inner expression,
  -- then check if it reduced to a rational or single monomial.
  let inner = toNormExpr a
   in case normCoeff inner of
        Just r -> normRoot n r
        Nothing -> case Map.toList (unNormExpr inner) of
          -- Single monomial c * product(atoms^e): distribute the nth root.
          -- nth-root(c * product(alpha_i^e_i)) = nth-root(c) * product(nth-root(alpha_i^e_i))
          -- where nth-root(alpha_i^e_i) = ((mth-root r_i)^e_i)^(1/n) = ((m*n)th-root r_i)^e_i
          [(Monomial atoms, c)] ->
            let rootC = normRoot n c
                rootAtoms =
                  Map.foldlWithKey'
                    ( \acc atom e ->
                        normMul acc (rootOfAtomPow n atom e)
                    )
                    (normLit 1)
                    atoms
             in normMul rootC rootAtoms
          -- Multi-term radicand: can't decompose into atoms.
          -- Canonicalize: clear denominators and extract common content
          -- so the radicand has integer, coprime coefficients.
          -- nth-root(E/d) = (1/d) * nth-root(E * d^(n-1))  where E has integer coefficients.
          -- Then extract GCD content: nth-root(g * E') = nth-root(g) * nth-root(E').
          terms ->
            let coeffs = map snd terms
                -- LCD of all coefficient denominators
                lcd = foldl lcm 1 (map denominator coeffs)
                -- Scale radicand so all coefficients are integers: E = lcd * inner
                intCoeffs = [numerator (c * fromInteger lcd) | (_, c) <- terms]
                -- GCD of all integer coefficients (the "content" of E)
                g = foldl gcd 0 (map abs intCoeffs)
                -- nth-root(E / lcd) = nth-root(E * lcd^(n-1)) / lcd
                -- E * lcd^(n-1) = g * lcd^(n-1) * E'  where E' = E/g coprime
                -- nth-root(g * lcd^(n-1) * E') / lcd = nth-root(g * lcd^(n-1)) * nth-root(E') / lcd
                -- Factor g * lcd^(n-1) into nth power:
                totalContent = g * lcd ^ (n - 1)
                (nthOut, nthRem) = extractNthPower n totalContent
                -- = (nthOut / lcd) * nth-root(nthRem) * nth-root(E')
                outerCoeff0 = nthOut / fromInteger lcd
                -- E' = E/g has coprime integer coefficients
                cleanedTerms =
                  [(mono, c * fromInteger lcd / fromInteger g) | (mono, c) <- terms]
                cleanedRadicand0 = NormExpr $ Map.fromList cleanedTerms
                -- For odd roots, ensure positive radicand: nth-root(-x) = -(nth-root(x)).
                -- Uses Double evaluation for sign determination.
                -- Note: this identity only holds for REAL radicands. For complex
                -- radicands (containing i), cbrt(-z) /= -(cbrt(z)) in general because
                -- arg(-z)/n /= arg(z)/n + pi. We skip complex radicands (eval -> NaN).
                --
                -- A stronger canonicalization could restrict complex radicands to
                -- a canonical wedge (e.g., arg in (-pi/n, pi/n] for nth roots) by
                -- factoring out the appropriate nth root of unity omega = e^{2*pi*i*k/n}.
                -- This would give a unique representative for each branch but adds
                -- complexity (omega prefactors) without clear readability gains, and
                -- the Cardano casus irreducibilis radicands are already in a natural
                -- conjugate-pair form that would be disrupted.
                (outerCoeff, cleanedRadicand, evenRootNeg)
                  | odd n =
                      let v = eval (fromNormExpr cleanedRadicand0)
                       in if not (isNaN v) && v < 0
                            then (negate outerCoeff0, normNeg cleanedRadicand0, False)
                            else (outerCoeff0, cleanedRadicand0, False)
                  | even n =
                      -- For even roots, check if the radicand is strictly negative.
                      -- If so, sqrt(-x) = i*sqrt(x) -- factor out ImagUnit.
                      -- Use Double eval as fast path, rigorous interval as fallback.
                      let isNeg = isRadicandNegative (fromNormExpr cleanedRadicand0)
                       in if isNeg
                            then (outerCoeff0, normNeg cleanedRadicand0, True)
                            else (outerCoeff0, cleanedRadicand0, False)
                  | otherwise = (outerCoeff0, cleanedRadicand0, False)
                -- nth-root(nthRem) as a separate atom (integer, nth-power-free).
                -- For odd roots, nth-root(a*b) = nth-root(a) * nth-root(b) always holds.
                -- For even roots, only split if nthRem = 1 (to avoid sign issues).
                canSplitRem = odd n || nthRem == 1
                -- Wrap result with ImagUnit if we factored out negativity from even root
                wrapI expr =
                  if evenRootNeg
                    then normMul (normAtom ImagUnit) expr
                    else expr
             in if normIsZero cleanedRadicand || g == 0
                  then normZero
                  else
                    if not canSplitRem
                      then
                        -- Even root with nthRem > 1: keep nthRem inside the radicand
                        let fullRadicand = normScale nthRem cleanedRadicand
                         in normScale outerCoeff $
                              wrapI $
                                normAtom (NestedRoot n (fromNormExpr fullRadicand))
                      else case normCoeff cleanedRadicand of
                        Just r ->
                          -- Radicand simplified to a single rational
                          normScale outerCoeff $
                            wrapI $
                              normMul (normRoot n nthRem) (normRoot n r)
                        Nothing ->
                          -- Coprime integer-coefficient radicand under NestedRoot
                          let nestedPart = normAtom (NestedRoot n (fromNormExpr cleanedRadicand))
                           in if nthRem == 1
                                then normScale outerCoeff $ wrapI nestedPart
                                else
                                  normScale outerCoeff $
                                    wrapI $
                                      normMul (normRoot n nthRem) nestedPart
toNormExpr (Inv a) = normInv (toNormExpr a)
toNormExpr (Pow a n)
  | n >= 0 = normPow (toNormExpr a) n
  | otherwise = normInv (normPow (toNormExpr a) (negate n))

-- --------------------------------------------------------------------------
-- Conversion to RadExpr
-- --------------------------------------------------------------------------

-- | Convert a normal form expression back to 'RadExpr'.
--
-- Groups monomials by their 'NestedRoot' atoms and factors out common
-- nested subexpressions, producing a more readable output.
-- For example, @a * cbrt(z) + b * i * cbrt(z)@ becomes @(a + b*i) * cbrt(z)@.
fromNormExpr :: NormExpr -> RadExpr Rational
fromNormExpr (NormExpr m) = case Map.toList m of
  [] -> Lit 0
  terms ->
    -- Group monomials by their NestedRoot atoms (Groebner-style elimination
    -- ordering: NestedRoot atoms are "variables", simple atoms are
    -- "coefficients"). This factors out common NestedRoot subexpressions,
    -- e.g., a*cbrt(z) + b*i*cbrt(z) -> (a + b*i)*cbrt(z).
    let grouped = Map.toList (groupByNested terms)
     in buildSum
          [ nestedGroupToExpr nestedAtoms coeffTerms
            | (nestedAtoms, coeffTerms) <- grouped
          ]
  where
    -- Partition a monomial's atoms into NestedRoot and simple (RatRoot/ImagUnit).
    splitAtoms :: Map.Map Atom Int -> (Map.Map Atom Int, Map.Map Atom Int)
    splitAtoms = Map.partitionWithKey (\a _ -> isNested a)
      where
        isNested (NestedRoot _ _) = True
        isNested _ = False

    -- Group monomials by their NestedRoot atom signature.
    -- Each group collects the (simple-atoms, coeff) pairs that share
    -- the same NestedRoot factor.
    groupByNested ::
      [(Monomial, Rational)] ->
      Map.Map (Map.Map Atom Int) [(Map.Map Atom Int, Rational)]
    groupByNested =
      foldl
        ( \acc (Monomial atoms, coeff) ->
            let (nested, simple) = splitAtoms atoms
             in Map.insertWith (++) nested [(simple, coeff)] acc
        )
        Map.empty

    -- Convert a group: product of NestedRoot atoms * sum of simple terms.
    nestedGroupToExpr ::
      Map.Map Atom Int ->
      [(Map.Map Atom Int, Rational)] ->
      RadExpr Rational
    nestedGroupToExpr nestedAtoms coeffTerms =
      let nestedProd = buildProd [atomToExpr a e | (a, e) <- Map.toList nestedAtoms]
          coeffExpr =
            buildSum
              [ applyCoeff
                  c
                  ( buildProd
                      [ atomToExpr a e
                        | (a, e) <- Map.toList simple
                      ]
                  )
                | (simple, c) <- coeffTerms
              ]
       in case (nestedProd, coeffExpr) of
            (Lit 1, _) -> coeffExpr
            (_, Lit 1) -> nestedProd
            _ -> Mul coeffExpr nestedProd

    atomToExpr :: Atom -> Int -> RadExpr Rational
    atomToExpr _ 0 = Lit 1
    atomToExpr ImagUnit 1 = Root 2 (Lit (-1))
    atomToExpr ImagUnit e = Pow (Root 2 (Lit (-1))) e
    atomToExpr (RatRoot n r) 1 = Root n (Lit r)
    atomToExpr (RatRoot n r) e = Pow (Root n (Lit r)) e
    atomToExpr (NestedRoot n inner) 1 = Root n inner
    atomToExpr (NestedRoot n inner) e = Pow (Root n inner) e

    applyCoeff :: Rational -> RadExpr Rational -> RadExpr Rational
    applyCoeff 1 body = body
    applyCoeff (-1) body = Neg body
    applyCoeff c (Lit 1) = Lit c
    applyCoeff c body = Mul (Lit c) body

    buildProd :: [RadExpr Rational] -> RadExpr Rational
    buildProd [] = Lit 1
    buildProd [x] = x
    buildProd (x : xs) = foldl Mul x xs

    buildSum :: [RadExpr Rational] -> RadExpr Rational
    buildSum [] = Lit 0
    buildSum [x] = x
    buildSum (x : xs) = foldl Add x xs

-- | Check if a radical expression is strictly negative.
--
-- Used for factoring @i@ out of even-root radicands: @sqrt(-x) = i*sqrt(x)@.
--
-- Uses 'Double' evaluation as a fast path (works for real radicands).
-- Falls back to rigorous interval arithmetic ('evalInterval' +
-- 'strictlyNegative') for expressions where 'Double' returns @NaN@
-- (complex intermediates like @sqrt(-7)@).
-- Returns 'False' when the sign cannot be determined.
isRadicandNegative :: RadExpr Rational -> Bool
isRadicandNegative expr =
  let v = eval expr
   in if not (isNaN v)
        then v < 0
        else -- Double eval failed (complex intermediates). Try interval arithmetic.
          unsafePerformIO $ do
            result <- try (evaluate (evalInterval expr))
            case result of
              Left (_ :: SomeException) -> return False
              Right iv -> return (strictlyNegative iv)
