-- | Normal form for radical expressions: Q-linear combinations of
-- products of radical atoms.
--
-- Every expression in Q[√2, √3, ∛5, ...] has a unique representation
-- as a sum of (rational coefficient × monomial), where each monomial
-- is a product of radical atoms raised to bounded powers.
--
-- This representation gives automatic like-term collection, canonical
-- ordering, and reliable structural equality — all of which are
-- expensive or unreliable on the tree-based 'RadExpr'.
module Surd.Radical.NormalForm
  ( Atom(..)
  , Monomial(..)
  , NormExpr(..)
  -- * Construction
  , normLit
  , normAtom
  , normRoot
  -- * Arithmetic
  , normAdd
  , normSub
  , normNeg
  , normMul
  , normScale
  , normPow
  , normInv
  -- * Conversion
  , toNormExpr
  , fromNormExpr
  -- * Queries
  , normIsZero
  , normCoeff
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio (numerator, denominator)
import Surd.Types (RadExpr(..))
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)

-- | A radical atom: an irreducible nth root of a positive rational.
--
-- Negative radicands are factored as @√(-r) = i·√r@ where @i = √(-1)@
-- is represented by 'ImagUnit'.
--
-- Invariant: the rational in 'RatRoot' is > 0, square-free (for n=2),
-- cube-free (for n=3), etc. — i.e., no perfect nth power factors remain.
data Atom
  = RatRoot !Int !Rational
    -- ^ @RatRoot n r@: the principal nth root of r (r > 0, reduced).
  | ImagUnit
    -- ^ @√(-1) = i@. Kept separate since i² = -1 is the key reduction.
  | NestedRoot !Int !(RadExpr Rational)
    -- ^ @NestedRoot n e@: ⁿ√e where e is a non-rational expression
    -- that cannot be decomposed into atoms.  Exponent reduction
    -- applies: @(NestedRoot n e)^n@ extracts the radicand as a NormExpr.
  deriving (Eq, Ord, Show)

-- | A monomial: product of atoms raised to positive powers.
--
-- The empty map represents the multiplicative identity (1).
-- Invariant: all exponents are in [1, n-1] for @RatRoot n r@ atoms
-- (since @(ⁿ√r)ⁿ = r@, which is reduced to a coefficient).
-- For 'ImagUnit', exponents are in [0, 1] (since i² = -1).
newtype Monomial = Monomial { unMonomial :: Map Atom Int }
  deriving (Eq, Ord, Show)

-- | A normalized expression: Q-linear combination of monomials.
--
-- Invariant: no zero coefficients in the map.
newtype NormExpr = NormExpr { unNormExpr :: Map Monomial Rational }
  deriving (Eq, Ord, Show)

-- | The zero expression.
normZero :: NormExpr
normZero = NormExpr Map.empty

-- | A rational literal.
normLit :: Rational -> NormExpr
normLit 0 = normZero
normLit r = NormExpr (Map.singleton unitMono r)

-- | The unit monomial (represents 1).
unitMono :: Monomial
unitMono = Monomial Map.empty

-- | A single atom with coefficient 1.
normAtom :: Atom -> NormExpr
normAtom a = NormExpr (Map.singleton (Monomial (Map.singleton a 1)) 1)

-- | Create a normalized nth root of a positive rational.
-- Extracts perfect nth powers: √12 → 2√3.
normRoot :: Int -> Rational -> NormExpr
normRoot n r
  | r == 0    = normZero
  | r == 1    = normLit 1
  | r < 0 && n == 2 =
      -- √(-r) = i · √r
      normMul (normAtom ImagUnit) (normRoot 2 (negate r))
  | r < 0 && even n =
      -- Even root of negative: i · ⁿ√r
      normMul (normAtom ImagUnit) (normRoot n (negate r))
  | r < 0 && odd n =
      -- Odd root of negative: -ⁿ√(-r)
      normNeg (normRoot n (negate r))
  | otherwise =
      let num = numerator r
          den = denominator r
          (numOut, numIn) = extractNthPower n (abs num)
          (denOut, denIn) = extractNthPower n den
          coeff = numOut / denOut
          radicand = numIn / denIn
      in if radicand == 1
         then normLit coeff
         else normScale coeff (normAtom (RatRoot n radicand))

-- | Compute ⁿ√(atom^e).
-- For RatRoot m r: ⁿ√((ᵐ√r)^e) = ((mn)√r)^e  (composing roots)
-- For ImagUnit: ⁿ√(i^e) — reduce i^e first, then take nth root.
rootOfAtomPow :: Int -> Atom -> Int -> NormExpr
rootOfAtomPow n (RatRoot m r) e =
  -- (ᵐ√r)^e under an nth root: ⁿ√((ᵐ√r)^e) = ((m·n)√r)^e
  -- This creates an atom with combined root index m*n.
  -- reduceMonomial will handle if e >= m*n.
  reduceMonomial (Monomial (Map.singleton (RatRoot (m * n) r) e)) 1
rootOfAtomPow n (NestedRoot m inner) e =
  -- ⁿ√((ᵐ√e)^k): compose roots → ((m·n)√e)^k
  reduceMonomial (Monomial (Map.singleton (NestedRoot (m * n) inner) e)) 1
rootOfAtomPow n ImagUnit e =
  -- i^e under an nth root: ⁿ√(i^e)
  -- Reduce i^e first, then take nth root of the result.
  let e' = e `mod` 4
  in case e' of
    0 -> normLit 1            -- ⁿ√(1) = 1
    2 -> normRoot n (-1)      -- ⁿ√(i²) = ⁿ√(-1)
    _ ->
      -- ⁿ√(i) and ⁿ√(-i) require (2n)th roots of -1.
      -- i = (√(-1)), so ⁿ√(i) = (2n)√(-1).
      -- -i = -(√(-1)), so ⁿ√(-i) = -(2n)√(-1) when n is odd.
      let base = normRoot (2 * n) (-1)  -- (2n)√(-1)
      in if e' == 1 then base else normNeg base

-- | Extract the largest nth power factor.
extractNthPower :: Int -> Integer -> (Rational, Rational)
extractNthPower n m
  | m == 0    = (0, 0)
  | m == 1    = (1, 1)
  | otherwise =
      let fs = factorise (fromInteger (abs m) :: Positive)
          extracted = product [ p ^ (e `div` n) | (p, e) <- fs ]
          remainder = product [ p ^ (e `mod` n) | (p, e) <- fs ]
      in (fromInteger extracted, fromInteger remainder)

-- | Addition.
normAdd :: NormExpr -> NormExpr -> NormExpr
normAdd (NormExpr a) (NormExpr b) =
  NormExpr $ Map.filter (/= 0) $ Map.unionWith (+) a b

-- | Subtraction.
normSub :: NormExpr -> NormExpr -> NormExpr
normSub a b = normAdd a (normNeg b)

-- | Negation.
normNeg :: NormExpr -> NormExpr
normNeg (NormExpr m) = NormExpr (Map.map negate m)

-- | Scale by a rational constant.
normScale :: Rational -> NormExpr -> NormExpr
normScale 0 _ = normZero
normScale c (NormExpr m) = NormExpr $ Map.filter (/= 0) $ Map.map (* c) m

-- | Multiplication.
normMul :: NormExpr -> NormExpr -> NormExpr
normMul (NormExpr a) (NormExpr b) =
  let terms = [ mulMonoCoeff (m1, c1) (m2, c2)
              | (m1, c1) <- Map.toList a
              , (m2, c2) <- Map.toList b
              ]
  in foldl normAdd normZero terms

-- | Multiply two monomial-coefficient pairs, reducing exponents.
mulMonoCoeff :: (Monomial, Rational) -> (Monomial, Rational) -> NormExpr
mulMonoCoeff (Monomial m1, c1) (Monomial m2, c2) =
  let merged = Map.unionWith (+) m1 m2
      coeff = c1 * c2
  in reduceMonomial (Monomial merged) coeff

-- | Reduce a monomial by applying atom^n = radicand and i^2 = -1.
-- For NestedRoot atoms with exponent >= n, extracts the radicand
-- as a NormExpr factor (since the radicand is non-rational).
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
          atoms' = if rem' == 0 then Map.delete (NestedRoot n inner) atoms
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
        0 -> (1,     False)
        1 -> (1,     True)
        2 -> ((-1),  False)
        3 -> ((-1),  True)
        _ -> (1,     False)
      c' = c * signFactor
      m' = if hasI then Map.insert ImagUnit 1 m else m
  in (c', m')
reduceAtom (c, m) atom@(RatRoot n r) e =
  let (full, rem') = e `divMod` n
      -- Each full power of n extracts the radicand r
      c' = c * r ^^ full
      m' = if rem' == 0 then m else Map.insert atom rem' m
  in (c', m')
reduceAtom (c, m) atom@(NestedRoot _ _) e =
  -- NestedRoot reduction is handled by reduceNestedRoots (post-pass)
  -- because extracting the radicand produces a NormExpr, not a Rational.
  (c, Map.insert atom e m)

-- | Integer power.
normPow :: NormExpr -> Int -> NormExpr
normPow _ 0 = normLit 1
normPow e 1 = e
normPow e n
  | n < 0     = error "normPow: negative exponent (use Inv in RadExpr)"
  | even n    = let half = normPow e (n `div` 2) in normMul half half
  | otherwise = normMul e (normPow e (n - 1))

-- | Multiplicative inverse.
--
-- For a single monomial, inverts directly by negating exponents
-- (using the reduction rules (ⁿ√r)ⁿ = r and i⁴ = 1).
--
-- For multi-term expressions (sums), iteratively rationalizes by
-- picking a radical atom α, viewing the expression as a polynomial
-- in α over the sub-field generated by the remaining atoms, and
-- computing the inverse in the quotient ring Q(...)[α]/(αⁿ - r).
normInv :: NormExpr -> NormExpr
normInv ne = case Map.toList (unNormExpr ne) of
  [] -> error "normInv: division by zero"
  -- Single monomial: c * m → (1/c) * m^(-1)
  [(mono, c)] -> normScale (1 / c) (invertMonomial mono)
  -- Multi-term: rationalize iteratively
  _ -> rationalizeInv ne

-- | Invert a monomial by negating all exponents.
-- reduceMonomial handles the modular arithmetic:
--   (ⁿ√r)^(-e) mod (ⁿ√r)^n = r  gives  (ⁿ√r)^(n-e) / r
--   i^(-e) mod i^4 = 1  gives the appropriate sign and power.
invertMonomial :: Monomial -> NormExpr
invertMonomial (Monomial atoms) =
  reduceMonomial (Monomial (Map.map negate atoms)) 1

-- | Rationalize the inverse of a multi-term NormExpr.
--
-- Strategy: pick a radical atom α = ⁿ√r appearing in the expression.
-- View the expression as a polynomial f(α) in Q(other atoms)[α].
-- The minimal polynomial of α is αⁿ - r.
-- Compute g(α) such that f(α)·g(α) ≡ 1 (mod αⁿ - r)
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
      -- Express ne as polynomial in atom: ne = a₀ + a₁·α + a₂·α² + ...
      let n = atomDegree atom
          polyCoeffs = toAtomPoly atom ne  -- [a₀, a₁, ..., a_{n-1}]
          minPoly = minPolyCoeffs n atom   -- [negated radicand, 0, ..., 0, 1]
          -- Extended GCD to find inverse polynomial
          (_, inv, _) = normPolyExtGcd polyCoeffs minPoly
          -- Rebuild: inv₀ + inv₁·α + inv₂·α² + ...
      in fromAtomPoly atom inv

-- | Find any radical atom appearing in a NormExpr.
findRadicalAtom :: NormExpr -> Maybe Atom
findRadicalAtom (NormExpr m) =
  case concatMap (Map.keys . unMonomial . fst) (Map.toList m) of
    []    -> Nothing
    (a:_) -> Just a

-- | Degree of an atom's minimal polynomial.
atomDegree :: Atom -> Int
atomDegree ImagUnit          = 2    -- i² + 1 = 0
atomDegree (RatRoot n _)     = n    -- αⁿ - r = 0
atomDegree (NestedRoot n _)  = n    -- αⁿ - e = 0

-- | Express a NormExpr as a polynomial in a given atom.
-- Returns coefficients [a₀, a₁, ..., a_{n-1}] where ne = Σ aᵢ·atom^i.
-- Each aᵢ is a NormExpr not containing the atom.
toAtomPoly :: Atom -> NormExpr -> [NormExpr]
toAtomPoly atom (NormExpr m) =
  let n = atomDegree atom
      -- For each monomial, extract the power of atom and the remainder
      grouped = Map.foldlWithKey' (\acc mono c ->
        let (power, rest) = extractAtomPower atom mono
            idx = power `mod` n
        in Map.insertWith normAdd idx (NormExpr (Map.singleton rest c)) acc
        ) Map.empty m
      -- Build coefficient list
  in [Map.findWithDefault normZero i grouped | i <- [0..n-1]]

-- | Extract the power of a specific atom from a monomial,
-- returning (power, monomial without that atom).
extractAtomPower :: Atom -> Monomial -> (Int, Monomial)
extractAtomPower atom (Monomial atoms) =
  case Map.lookup atom atoms of
    Nothing -> (0, Monomial atoms)
    Just e  -> (e, Monomial (Map.delete atom atoms))

-- | Minimal polynomial coefficients for an atom.
-- For RatRoot n r: αⁿ - r = 0, coefficients [-r, 0, ..., 0, 1]
-- For ImagUnit: α² + 1 = 0, coefficients [1, 0, 1]
minPolyCoeffs :: Int -> Atom -> [NormExpr]
minPolyCoeffs _ ImagUnit =
  [normLit 1, normZero, normLit 1]           -- α² + 1
minPolyCoeffs n (RatRoot _ r) =
  [normLit (negate r)] ++ replicate (n - 1) normZero ++ [normLit 1]  -- αⁿ - r
minPolyCoeffs n (NestedRoot _ inner) =
  [normNeg (toNormExpr inner)] ++ replicate (n - 1) normZero ++ [normLit 1]  -- αⁿ - e

-- | Reconstruct a NormExpr from polynomial coefficients in an atom.
-- Given [a₀, a₁, ...], computes a₀ + a₁·atom + a₂·atom² + ...
fromAtomPoly :: Atom -> [NormExpr] -> NormExpr
fromAtomPoly _ [] = normZero
fromAtomPoly atom coeffs =
  foldl normAdd normZero
    [ normMul c (atomPow atom i) | (i, c) <- zip [0..] coeffs ]

-- | Compute atom^k as a NormExpr (without reduction — the monomial
-- will be reduced when multiplied).
atomPow :: Atom -> Int -> NormExpr
atomPow _ 0 = normLit 1
atomPow a k = NormExpr (Map.singleton (Monomial (Map.singleton a k)) 1)

-- --------------------------------------------------------------------------
-- Polynomial arithmetic over NormExpr coefficients
-- --------------------------------------------------------------------------

-- | Extended GCD for polynomials with NormExpr coefficients.
-- Returns (gcd, s, t) such that gcd = s·a + t·b.
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
              in ( normPolyScale lcInv r0
                 , normPolyScale lcInv s0
                 , normPolyScale lcInv t0
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
normPolyAdd (a:as) (b:bs) = normAdd a b : normPolyAdd as bs

-- | Polynomial subtraction.
normPolySub :: [NormExpr] -> [NormExpr] -> [NormExpr]
normPolySub a b = normPolyAdd a (map normNeg b)

-- | Polynomial multiplication.
normPolyMul :: [NormExpr] -> [NormExpr] -> [NormExpr]
normPolyMul [] _ = []
normPolyMul _ [] = []
normPolyMul as bs =
  foldl normPolyAdd [] [shift i (map (normMul a) bs) | (i, a) <- zip [0..] as]
  where
    shift n xs = replicate n normZero ++ xs

-- | Scale a polynomial by a NormExpr.
normPolyScale :: NormExpr -> [NormExpr] -> [NormExpr]
normPolyScale c = map (normMul c)

-- | Polynomial division with remainder.
normPolyDivMod :: [NormExpr] -> [NormExpr] -> ([NormExpr], [NormExpr])
normPolyDivMod num den
  | normPolyIsZero den = error "normPolyDivMod: division by zero"
  | degNum < degDen    = ([], num)
  | otherwise          = go (replicate (degNum - degDen + 1) normZero) num
  where
    degNum = normPolyDeg num
    degDen = normPolyDeg den
    lcDen = case normPolyLeadCoeff den of
              Just c  -> c
              Nothing -> error "normPolyDivMod: zero divisor"
    lcDenInv = normInv lcDen

    go q r
      | normPolyIsZero r || normPolyDeg r < degDen = (normPolyTrim q, normPolyTrim r)
      | otherwise =
          let dr = normPolyDeg r
              lcR = case normPolyLeadCoeff r of
                      Just c  -> c
                      Nothing -> error "normPolyDivMod: unexpected zero"
              coeff = normMul lcR lcDenInv
              shift = dr - degDen
              q' = normPolySetCoeff shift coeff q
              sub = [if i == shift then coeff else normZero | i <- [0..shift]]
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
    go d _ []     = d
    go d i (x:xs) = go (if normIsZero x then d else i) (i + 1) xs

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

-- | Is the expression zero?
normIsZero :: NormExpr -> Bool
normIsZero (NormExpr m) = Map.null m

-- | Get the rational coefficient (if the expression is purely rational).
normCoeff :: NormExpr -> Maybe Rational
normCoeff (NormExpr m) = case Map.toList m of
  []                 -> Just 0
  [(Monomial a, c)] | Map.null a -> Just c
  _                  -> Nothing

-- --------------------------------------------------------------------------
-- Conversion from RadExpr
-- --------------------------------------------------------------------------

-- | Convert a 'RadExpr' to normal form.
--
-- Handles all cases except 'Inv' of non-rational expressions,
-- which would require rationalization (conjugate multiplication).
-- For 'Inv' of non-rationals, falls back to keeping the expression
-- unnormalized.
toNormExpr :: RadExpr Rational -> NormExpr
toNormExpr (Lit r)    = normLit r
toNormExpr (Neg a)    = normNeg (toNormExpr a)
toNormExpr (Add a b)  = normAdd (toNormExpr a) (toNormExpr b)
toNormExpr (Mul a b)  = normMul (toNormExpr a) (toNormExpr b)
toNormExpr (Root n (Lit r)) = normRoot n r
toNormExpr (Root n a) =
  -- For non-literal radicands, first normalize the inner expression,
  -- then check if it reduced to a rational or single monomial.
  let inner = toNormExpr a
  in case normCoeff inner of
    Just r  -> normRoot n r
    Nothing -> case Map.toList (unNormExpr inner) of
      -- Single monomial c * ∏(atoms^e): distribute the nth root.
      -- ⁿ√(c · ∏ αᵢ^eᵢ) = ⁿ√c · ∏ ⁿ√(αᵢ^eᵢ)
      -- where ⁿ√(αᵢ^eᵢ) = (ⁿ√(ᵐ√rᵢ))^eᵢ = ((mn)√rᵢ)^eᵢ
      [(Monomial atoms, c)] ->
        let rootC = normRoot n c
            rootAtoms = Map.foldlWithKey' (\acc atom e ->
              normMul acc (rootOfAtomPow n atom e)) (normLit 1) atoms
        in normMul rootC rootAtoms
      -- Multi-term radicand: can't decompose into atoms.
      -- Wrap as an opaque NestedRoot atom. Exponent reduction still works:
      -- (NestedRoot n e)^n extracts e as a NormExpr factor.
      _ -> normAtom (NestedRoot n (fromNormExpr inner))
toNormExpr (Inv a) = normInv (toNormExpr a)
toNormExpr (Pow a n)
  | n >= 0    = normPow (toNormExpr a) n
  | otherwise = normInv (normPow (toNormExpr a) (negate n))

-- --------------------------------------------------------------------------
-- Conversion to RadExpr
-- --------------------------------------------------------------------------

-- | Convert a normal form expression back to 'RadExpr'.
fromNormExpr :: NormExpr -> RadExpr Rational
fromNormExpr (NormExpr m) = case Map.toList m of
  []           -> Lit 0
  terms        -> buildSum (map monoToExpr terms)
  where
    monoToExpr :: (Monomial, Rational) -> RadExpr Rational
    monoToExpr (Monomial atoms, coeff) =
      let atomExprs = [ atomToExpr a e | (a, e) <- Map.toList atoms ]
      in applyCoeff coeff (buildProd atomExprs)

    atomToExpr :: Atom -> Int -> RadExpr Rational
    atomToExpr ImagUnit 1       = Root 2 (Lit (-1))
    atomToExpr ImagUnit e       = Pow (Root 2 (Lit (-1))) e
    atomToExpr (RatRoot n r) 1  = Root n (Lit r)
    atomToExpr (RatRoot n r) e  = Pow (Root n (Lit r)) e
    atomToExpr (NestedRoot n inner) 1 = Root n inner
    atomToExpr (NestedRoot n inner) e = Pow (Root n inner) e

    applyCoeff :: Rational -> RadExpr Rational -> RadExpr Rational
    applyCoeff 1 body    = body
    applyCoeff (-1) body = Neg body
    applyCoeff c (Lit 1) = Lit c
    applyCoeff c body    = Mul (Lit c) body

    buildProd :: [RadExpr Rational] -> RadExpr Rational
    buildProd []     = Lit 1
    buildProd [x]    = x
    buildProd (x:xs) = foldl Mul x xs

    buildSum :: [RadExpr Rational] -> RadExpr Rational
    buildSum []     = Lit 0
    buildSum [x]    = x
    buildSum (x:xs) = foldl Add x xs
