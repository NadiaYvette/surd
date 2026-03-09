-- | Sparse multivariate polynomials over an arbitrary coefficient ring.
--
-- Representation: @Map Mono k@ where each monomial maps to its
-- non-zero coefficient.  Monomials are products of variables
-- raised to positive integer powers.
module Surd.Polynomial.Multivariate
  ( Var(..)
  , Mono(..)
  , MPoly(..)
  -- * Construction
  , constPoly
  , varPoly
  , zeroPoly
  , onePoly
  , isZero
  -- * Arithmetic
  , addPoly
  , subPoly
  , mulPoly
  , negatePoly
  , scalePoly
  -- * Queries
  , totalDegree
  , degreeIn
  , variables
  , numTerms
  -- * Evaluation and substitution
  , evalPoly
  , substVar
  -- * Conversion
  , toUnivariate
  , fromUnivariate
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Surd.Polynomial.Univariate as U

-- | Variable identifier.
newtype Var = Var Int
  deriving (Eq, Ord, Show)

-- | Monomial: product of variables with positive exponents.
-- Invariant: no zero exponents in the map.
newtype Mono = Mono { unMono :: Map Var Int }
  deriving (Eq, Ord, Show)

-- | The constant monomial (empty product = 1).
monoOne :: Mono
monoOne = Mono Map.empty

-- | Monomial from a single variable raised to a power.
monoVar :: Var -> Int -> Mono
monoVar _ 0 = monoOne
monoVar v n = Mono (Map.singleton v n)

-- | Multiply two monomials.
monoMul :: Mono -> Mono -> Mono
monoMul (Mono a) (Mono b) = Mono (Map.filter (/= 0) (Map.unionWith (+) a b))

-- | Total degree of a monomial.
monoDegree :: Mono -> Int
monoDegree (Mono m) = sum (Map.elems m)

-- | Sparse multivariate polynomial over coefficient ring @k@.
-- Invariant: no zero coefficients.
newtype MPoly k = MPoly { unMPoly :: Map Mono k }
  deriving (Eq, Ord, Show, Functor)

-- | Remove zero coefficients to maintain the invariant.
clean :: (Eq k, Num k) => Map Mono k -> MPoly k
clean = MPoly . Map.filter (/= 0)

-- | The zero polynomial.
zeroPoly :: MPoly k
zeroPoly = MPoly Map.empty

-- | The constant polynomial 1.
onePoly :: (Eq k, Num k) => MPoly k
onePoly = constPoly 1

-- | Test if a polynomial is zero.
isZero :: MPoly k -> Bool
isZero (MPoly m) = Map.null m

-- | Constant polynomial.
constPoly :: (Eq k, Num k) => k -> MPoly k
constPoly 0 = zeroPoly
constPoly c = MPoly (Map.singleton monoOne c)

-- | A single variable as a polynomial.
varPoly :: Num k => Var -> MPoly k
varPoly v = MPoly (Map.singleton (monoVar v 1) 1)

-- | Add two polynomials.
addPoly :: (Eq k, Num k) => MPoly k -> MPoly k -> MPoly k
addPoly (MPoly a) (MPoly b) = clean (Map.unionWith (+) a b)

-- | Subtract two polynomials.
subPoly :: (Eq k, Num k) => MPoly k -> MPoly k -> MPoly k
subPoly a b = addPoly a (negatePoly b)

-- | Negate a polynomial.
negatePoly :: Num k => MPoly k -> MPoly k
negatePoly = fmap negate

-- | Multiply two polynomials.
mulPoly :: (Eq k, Num k) => MPoly k -> MPoly k -> MPoly k
mulPoly (MPoly a) (MPoly b) = clean $ Map.fromListWith (+)
  [ (monoMul m1 m2, c1 * c2)
  | (m1, c1) <- Map.toList a
  , (m2, c2) <- Map.toList b
  ]

-- | Scale a polynomial by a constant.
scalePoly :: (Eq k, Num k) => k -> MPoly k -> MPoly k
scalePoly 0 _ = zeroPoly
scalePoly c (MPoly m) = clean (fmap (* c) m)

-- | Total degree (maximum sum of exponents over all terms).
totalDegree :: MPoly k -> Int
totalDegree (MPoly m)
  | Map.null m = 0
  | otherwise  = maximum (map monoDegree (Map.keys m))

-- | Degree in a specific variable.
degreeIn :: Var -> MPoly k -> Int
degreeIn v (MPoly m)
  | Map.null m = 0
  | otherwise  = maximum (0 : map (fromMaybe 0 . Map.lookup v . unMono) (Map.keys m))

-- | Set of all variables appearing in the polynomial.
variables :: MPoly k -> Set.Set Var
variables (MPoly m) = foldMap (Map.keysSet . unMono) (Map.keys m)

-- | Number of terms.
numTerms :: MPoly k -> Int
numTerms (MPoly m) = Map.size m

-- | Evaluate a polynomial by substituting values for all variables.
evalPoly :: (Eq k, Num k) => (Var -> k) -> MPoly k -> k
evalPoly env (MPoly m) = sum
  [ c * evalMono env mono
  | (mono, c) <- Map.toList m
  ]
  where
    evalMono e (Mono vars) = product
      [ e v ^ n
      | (v, n) <- Map.toList vars
      ]

-- | Substitute a single variable with a polynomial.
substVar :: (Eq k, Num k) => Var -> MPoly k -> MPoly k -> MPoly k
substVar v replacement (MPoly m) = Map.foldlWithKey' step zeroPoly m
  where
    step acc mono c =
      let (varPart, restMono) = extractVar v mono
          restPoly = MPoly (Map.singleton restMono c)
          substPart = powPoly replacement varPart
      in addPoly acc (mulPoly restPoly substPart)

    extractVar var (Mono vars) =
      case Map.lookup var vars of
        Nothing -> (0, Mono vars)
        Just n  -> (n, Mono (Map.delete var vars))

    powPoly _ 0 = onePoly
    powPoly p n = mulPoly p (powPoly p (n - 1))

-- | Convert to a univariate polynomial in the given variable,
-- with multivariate polynomial coefficients.
-- Returns @Nothing@ if the polynomial involves other variables
-- besides the given one (in the coefficients).
toUnivariate :: (Eq k, Num k) => Var -> MPoly k -> U.Poly (MPoly k)
toUnivariate v (MPoly m) =
  let groups = Map.foldlWithKey' groupByDeg Map.empty m
      maxDeg = if Map.null groups then 0 else fst (Map.findMax groups)
      coeffs = [fromMaybe zeroPoly (Map.lookup i groups) | i <- [0..maxDeg]]
  in U.mkPoly coeffs
  where
    groupByDeg acc (Mono vars) c =
      let deg = fromMaybe 0 (Map.lookup v vars)
          restMono = Mono (Map.delete v vars)
          coeff = MPoly (Map.singleton restMono c)
      in Map.insertWith addPoly deg coeff acc

-- | Convert a univariate polynomial (with scalar coefficients)
-- to a multivariate polynomial in the given variable.
fromUnivariate :: (Eq k, Num k) => Var -> U.Poly k -> MPoly k
fromUnivariate v (U.Poly coeffs) = clean $ Map.fromList
  [ (monoVar v i, c)
  | (i, c) <- zip [0..] coeffs
  , c /= 0
  ]

-- Num instance

instance (Eq k, Num k) => Num (MPoly k) where
  (+) = addPoly
  (*) = mulPoly
  negate = negatePoly
  abs    = error "MPoly: abs not meaningful"
  signum = error "MPoly: signum not meaningful"
  fromInteger = constPoly . fromInteger
