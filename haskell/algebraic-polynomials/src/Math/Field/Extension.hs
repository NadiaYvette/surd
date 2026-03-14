-- |
-- Module      : Math.Field.Extension
-- Description : Arithmetic in simple algebraic extension fields K(alpha)
-- Stability   : experimental
--
-- Implements arithmetic in simple algebraic extension fields
-- \(K(\alpha) \cong K[x] / (m_\alpha(x))\), where \(\alpha\) is a root of
-- an irreducible polynomial \(m_\alpha(x) \in K[x]\).
--
-- Elements of \(K(\alpha)\) are represented as polynomials in \(\alpha\) of
-- degree less than \(\deg m_\alpha\). Arithmetic is performed modulo
-- \(m_\alpha\), with multiplicative inverse computed via the extended
-- Euclidean algorithm.
--
-- 'ExtElem' has 'Num' and 'Fractional' instances, enabling natural
-- arithmetic syntax and nesting for field towers:
-- @ExtElem (ExtElem Rational)@ represents \(\mathbb{Q}(\alpha)(\beta)\).
module Math.Field.Extension
  ( ExtField(..)
  , ExtElem(..)
  , mkExtField
  , embed
  , generator
  , extAdd
  , extSub
  , extNeg
  , extMul
  , extInv
  , extDiv
  , extPow
  , extEq
  , isSentinelField
  , extGcd
  ) where

import Math.Polynomial.Univariate

-- | A simple algebraic extension \(K(\alpha)/K\) defined by the minimal
-- polynomial of \(\alpha\) over \(K\).
data ExtField k = ExtField
  { genMinPoly :: !(Poly k)
    -- ^ Minimal polynomial of the generator (monic, irreducible over \(K\)).
  , extDegree  :: !Int
    -- ^ Degree of the extension, equal to \(\deg m_\alpha\).
  , extName    :: String
    -- ^ Display name for the generator (used in pretty-printing).
  } deriving (Show)

-- | An element of an extension field \(K(\alpha)\), represented as a
-- polynomial in \(\alpha\) of degree \(< \deg m_\alpha\).
data ExtElem k = ExtElem
  { elemPoly  :: !(Poly k)
    -- ^ Polynomial representation in the generator.
  , elemField :: !(ExtField k)
    -- ^ The extension field this element belongs to.
  } deriving (Show)

-- | Construct an extension field from an irreducible polynomial and a
-- name for the generator.
--
-- The polynomial is made monic. It should be irreducible over \(K\)
-- (this is not checked).
--
-- >>> let f = mkExtField (mkPoly [-2, 0, 1] :: Poly Rational) "sqrt2"
-- >>> extDegree f
-- 2
mkExtField :: (Eq k, Fractional k) => Poly k -> String -> ExtField k
mkExtField p name =
  let mp = monicPoly p
  in ExtField
    { genMinPoly = mp
    , extDegree  = degree mp
    , extName    = name
    }

-- | Embed a base field element into the extension as a constant polynomial.
--
-- >>> embed field (3 :: Rational)  -- the element 3 in Q(alpha)
embed :: (Eq k, Num k) => ExtField k -> k -> ExtElem k
embed field c = ExtElem (constPoly c) field

-- | The generator \(\alpha\) of the extension field, represented as the
-- polynomial \(x\) modulo \(m_\alpha\).
generator :: Num k => ExtField k -> ExtElem k
generator field = ExtElem monoX field

-- | Reduce a polynomial modulo the minimal polynomial of the extension.
--
-- For sentinel fields (used by literal constructors), no reduction is performed.
reduce :: (Eq k, Fractional k) => ExtField k -> Poly k -> Poly k
reduce field p
  | isSentinelField field = p  -- no reduction needed for literals
  | otherwise = snd $ divModPoly p (genMinPoly field)

-- | Addition in \(K(\alpha)\).
extAdd :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extAdd (ExtElem a f) (ExtElem b _) = ExtElem (reduce f $ addPoly a b) f

-- | Subtraction in \(K(\alpha)\).
extSub :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extSub (ExtElem a f) (ExtElem b _) = ExtElem (reduce f $ subPoly a b) f

-- | Negation in \(K(\alpha)\).
extNeg :: (Eq k, Num k) => ExtElem k -> ExtElem k
extNeg (ExtElem a f) = ExtElem (scalePoly (-1) a) f

-- | Multiplication in \(K(\alpha)\): polynomial multiplication followed
-- by reduction modulo \(m_\alpha\).
extMul :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extMul (ExtElem a f) (ExtElem b _) = ExtElem (reduce f $ mulPoly a b) f

-- | Multiplicative inverse in \(K(\alpha)\) via the extended Euclidean
-- algorithm.
--
-- Finds \(s\) such that \(s \cdot a \equiv 1 \pmod{m_\alpha}\)
-- by computing \(\gcd(a, m_\alpha) = 1 = s \cdot a + t \cdot m_\alpha\).
--
-- Throws an error for the zero element.
extInv :: (Eq k, Fractional k) => ExtElem k -> ExtElem k
extInv (ExtElem (Poly []) _) = error "extInv: division by zero"
extInv (ExtElem a f) =
  -- Find s such that s*a = 1 (mod minPoly)
  -- by extended GCD: gcd(a, minPoly) = 1 = s*a + t*minPoly
  let (_, s, _) = extGcd a (genMinPoly f)
  in ExtElem (reduce f s) f

-- | Division in \(K(\alpha)\): @extDiv a b = a * b^(-1)@.
extDiv :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extDiv a b = extMul a (extInv b)

-- | Exponentiation in \(K(\alpha)\) by an integer exponent.
--
-- Uses binary exponentiation for positive exponents.
-- Negative exponents compute the inverse first.
extPow :: (Eq k, Fractional k) => ExtElem k -> Int -> ExtElem k
extPow e 0 = embed (elemField e) 1
extPow e n
  | n < 0     = extPow (extInv e) (-n)
  | even n    = let half = extPow e (n `div` 2) in extMul half half
  | otherwise = extMul e (extPow e (n - 1))

-- | Equality test for extension field elements: compares their polynomial
-- representations.
extEq :: (Eq k, Num k) => ExtElem k -> ExtElem k -> Bool
extEq (ExtElem a _) (ExtElem b _) = a == b

-- | A sentinel field used by 'fromInteger' and 'fromRational' in the
-- 'Num' and 'Fractional' instances.
--
-- Binary operations pick the non-sentinel field from either operand,
-- allowing expressions like @3 * alpha@ where @3@ is a literal and
-- @alpha@ is a generator.
sentinelField :: ExtField k
sentinelField = ExtField (Poly []) 0 "<literal>"

-- | Check whether a field is the sentinel (placeholder for literals).
--
-- Sentinel fields have degree 0 and are used internally by 'fromInteger'
-- and 'fromRational'.
isSentinelField :: ExtField k -> Bool
isSentinelField f = extDegree f == 0

-- | Pick the "real" field from two operands. Prefers non-sentinel.
pickField :: ExtField k -> ExtField k -> ExtField k
pickField f g
  | isSentinelField f = g
  | otherwise         = f

-- | Equality of extension field elements.
instance (Eq k, Fractional k) => Eq (ExtElem k) where
  (==) = extEq

-- | Arithmetic in \(K(\alpha)\) via 'Num'.
--
-- 'fromInteger' creates a literal element with a sentinel field;
-- the actual field is resolved when the literal participates in
-- arithmetic with a proper extension element.
--
-- 'abs' and 'signum' are not meaningful and will throw errors.
instance (Eq k, Fractional k) => Num (ExtElem k) where
  ExtElem a fa + ExtElem b fb =
    let f = pickField fa fb
    in ExtElem (reduce f $ addPoly a b) f
  ExtElem a fa * ExtElem b fb =
    let f = pickField fa fb
    in ExtElem (reduce f $ mulPoly a b) f
  negate (ExtElem a f) = ExtElem (scalePoly (-1) a) f
  abs    = error "ExtElem: abs not meaningful for field elements"
  signum = error "ExtElem: signum not meaningful for field elements"
  fromInteger n = ExtElem (constPoly (fromInteger n)) sentinelField

-- | 'Fractional' instance enables division and 'fromRational' literals.
instance (Eq k, Fractional k) => Fractional (ExtElem k) where
  recip = extInv
  fromRational r = ExtElem (constPoly (fromRational r)) sentinelField

-- | Extended Euclidean algorithm for polynomials.
--
-- @extGcd a b@ returns @(g, s, t)@ such that \(g = s \cdot a + t \cdot b\)
-- and \(g = \gcd(a, b)\), with \(g\) made monic.
--
-- Used internally by 'extInv' to compute multiplicative inverses.
extGcd :: (Eq k, Fractional k) => Poly k -> Poly k -> (Poly k, Poly k, Poly k)
extGcd a b = go a b (constPoly 1) zeroPoly zeroPoly (constPoly 1)
  where
    go r0 r1 s0 s1 t0 t1
      | unPoly r1 == [] =
          let lc = case leadCoeff r0 of Just c -> c; Nothing -> 1
              s  = scalePoly (1/lc) s0
              t  = scalePoly (1/lc) t0
              g  = scalePoly (1/lc) r0
          in (g, s, t)
      | otherwise =
          let (q, r) = divModPoly r0 r1
              s2 = subPoly s0 (mulPoly q s1)
              t2 = subPoly t0 (mulPoly q t1)
          in go r1 r s1 s2 t1 t2
