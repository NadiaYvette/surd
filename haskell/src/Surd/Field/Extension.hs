-- | Arithmetic in simple algebraic extension fields K(α),
-- where α is a root of an irreducible polynomial over K.
--
-- Elements of K(α) are represented as polynomials in α of degree
-- less than the degree of the minimal polynomial.
module Surd.Field.Extension
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
  ) where

import Surd.Polynomial.Univariate

-- | A simple algebraic extension K(α)/K defined by the minimal
-- polynomial of α over K.
data ExtField k = ExtField
  { genMinPoly :: !(Poly k)   -- ^ Minimal polynomial of the generator (monic, irreducible)
  , extDegree  :: !Int        -- ^ Degree of the extension = degree of minPoly
  , extName    :: String      -- ^ Display name for the generator
  } deriving (Show)

-- | An element of an extension field, represented as a polynomial
-- in the generator of degree < extDegree.
data ExtElem k = ExtElem
  { elemPoly  :: !(Poly k)       -- ^ Polynomial representation
  , elemField :: !(ExtField k)   -- ^ The field this element lives in
  } deriving (Show)

-- | Construct an extension field from an irreducible polynomial.
mkExtField :: (Eq k, Fractional k) => Poly k -> String -> ExtField k
mkExtField p name =
  let mp = monicPoly p
  in ExtField
    { genMinPoly = mp
    , extDegree  = degree mp
    , extName    = name
    }

-- | Embed a base field element into the extension.
embed :: (Eq k, Num k) => ExtField k -> k -> ExtElem k
embed field c = ExtElem (constPoly c) field

-- | The generator α of the extension.
generator :: Num k => ExtField k -> ExtElem k
generator field = ExtElem monoX field

-- | Reduce a polynomial modulo the minimal polynomial.
reduce :: (Eq k, Fractional k) => ExtField k -> Poly k -> Poly k
reduce field p = snd $ divModPoly p (genMinPoly field)

extAdd :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extAdd (ExtElem a f) (ExtElem b _) = ExtElem (reduce f $ addPoly a b) f

extSub :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extSub (ExtElem a f) (ExtElem b _) = ExtElem (reduce f $ subPoly a b) f

extNeg :: (Eq k, Num k) => ExtElem k -> ExtElem k
extNeg (ExtElem a f) = ExtElem (scalePoly (-1) a) f

extMul :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extMul (ExtElem a f) (ExtElem b _) = ExtElem (reduce f $ mulPoly a b) f

-- | Multiplicative inverse via the extended Euclidean algorithm.
extInv :: (Eq k, Fractional k) => ExtElem k -> ExtElem k
extInv (ExtElem (Poly []) _) = error "extInv: division by zero"
extInv (ExtElem a f) =
  -- Find s such that s*a ≡ 1 (mod minPoly)
  -- by extended GCD: gcd(a, minPoly) = 1 = s*a + t*minPoly
  let (_, s, _) = extGcd a (genMinPoly f)
  in ExtElem (reduce f s) f

extDiv :: (Eq k, Fractional k) => ExtElem k -> ExtElem k -> ExtElem k
extDiv a b = extMul a (extInv b)

extPow :: (Eq k, Fractional k) => ExtElem k -> Int -> ExtElem k
extPow e 0 = embed (elemField e) 1
extPow e n
  | n < 0     = extPow (extInv e) (-n)
  | even n    = let half = extPow e (n `div` 2) in extMul half half
  | otherwise = extMul e (extPow e (n - 1))

extEq :: (Eq k, Num k) => ExtElem k -> ExtElem k -> Bool
extEq (ExtElem a _) (ExtElem b _) = a == b

-- | Extended Euclidean algorithm for polynomials.
-- Returns (g, s, t) such that g = s*a + t*b, with g = gcd(a, b) made monic.
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
