{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Surd.Radical.Eval
-- Description : Numerical evaluation of radical expressions at various precisions
-- Stability   : experimental
--
-- Provides numerical evaluation of 'RadExpr' at several precision levels:
--
-- * 'eval' -- fast 'Double' evaluation (15 decimal digits)
-- * 'evalComplex' -- 'Complex Double' with StableName memoization for DAG sharing
-- * 'evalExact' -- ~60 decimal digits via lazy Cauchy sequences ('CReal')
-- * 'evalComplexExact' -- exact complex evaluation
-- * 'evalInterval' -- rigorous rational interval enclosures
-- * 'evalComplexInterval' -- rigorous complex interval enclosures
--
-- === When to use each
--
-- * 'eval': fastest; good for sanity checks and testing. Returns NaN for
--   even roots of negative numbers.
-- * 'evalComplex': use when the expression passes through complex
--   intermediates (e.g., @sqrt(-3)@ or casus irreducibilis Cardano forms).
--   Memoizes via StableName to handle DAG-structured sharing efficiently.
-- * 'evalExact': ~60 decimal digits; useful for high-precision comparison
--   when the expression is purely real.
-- * 'evalComplexExact': same precision as 'evalExact' but handles complex
--   intermediates.
-- * 'evalInterval': rigorous bounds via rational interval arithmetic.
--   For purely real expressions.
-- * 'evalComplexInterval': rigorous complex interval bounds. Handles
--   complex intermediates but is slower for general complex nth roots.
module Surd.Radical.Eval
  ( eval,
    evalComplex,
    evalExact,
    evalComplexExact,
    evalInterval,
    evalComplexInterval,
    ExactReal,
    ExactComplex,
  )
where

import Data.CReal (CReal)
import Data.Complex (Complex (..), magnitude, mkPolar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Math.Internal.Interval (ComplexInterval (..), Interval (..))
import Math.Internal.Interval qualified as I
import Surd.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)

-- | Exact real type with ~60 decimal digits of precision.
--
-- Uses lazy Cauchy sequences internally; comparisons are reliable
-- as long as values differ by more than @2^{-200}@.
type ExactReal = CReal 200

-- | Exact complex type (pair of 'ExactReal').
type ExactComplex = Complex ExactReal

-- | Evaluate a radical expression to a 'Double'.
--
-- Uses floating-point arithmetic -- fast but inexact (about 15 significant
-- decimal digits). Primarily useful for sanity checks, testing, and as
-- a starting point for Newton refinement.
--
-- __Warning:__ expressions involving even roots of negative numbers
-- (e.g., @sqrt(-3)@) will produce @NaN@. Use 'evalComplex' for
-- expressions that pass through complex intermediates.
eval :: RadExpr Rational -> Double
eval (Lit r) = fromRational r
eval (Neg a) = negate (eval a)
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Inv a) = 1 / eval a
eval (Root n a) = eval a ** (1 / fromIntegral n)
eval (Pow a n) = eval a ^^ n

-- | Evaluate a radical expression to an 'ExactReal' (~60 decimal digits).
--
-- Unlike 'eval', handles even roots of negative numbers via the 'Floating'
-- instance (which uses complex intermediates internally).
--
-- __Caveat:__ for expressions with genuinely complex intermediates
-- (e.g., @sqrt(-3)@), the 'Floating' @(**)@ may not give the principal
-- branch. Use 'evalComplexExact' and take 'Data.Complex.realPart' if
-- precision is critical for such cases.
evalExact :: RadExpr Rational -> ExactReal
evalExact (Lit r) = fromRational r
evalExact (Neg a) = negate (evalExact a)
evalExact (Add a b) = evalExact a + evalExact b
evalExact (Mul a b) = evalExact a * evalExact b
evalExact (Inv a) = 1 / evalExact a
evalExact (Root n a) = evalExact a ** (1 / fromIntegral n)
evalExact (Pow a n)
  | n >= 0 = evalExact a ^ n
  | otherwise = 1 / (evalExact a ^ negate n)

-- | Evaluate a radical expression to an 'ExactComplex' (~60 decimal digits).
--
-- Handles expressions that pass through complex intermediates.
-- Uses manual complex inverse to avoid CReal's unsupported 'RealFloat'
-- operations (scaleFloat/exponent), which would crash at runtime.
evalComplexExact :: RadExpr Rational -> ExactComplex
evalComplexExact (Lit r) = fromRational r :+ 0
evalComplexExact (Neg a) = negate (evalComplexExact a)
evalComplexExact (Add a b) = evalComplexExact a + evalComplexExact b
evalComplexExact (Mul a b) = evalComplexExact a * evalComplexExact b
evalComplexExact (Inv a) = cinv (evalComplexExact a)
evalComplexExact (Root n a) = complexNthRootExact n (evalComplexExact a)
evalComplexExact (Pow a n)
  | n >= 0 = evalComplexExact a ^ n
  | otherwise = 1 / (evalComplexExact a ^ negate n)

-- | Complex inversion without RealFloat.
-- Data.Complex uses scaleFloat in division, which CReal doesn't support.
cinv :: ExactComplex -> ExactComplex
cinv (re :+ im) =
  let d = re * re + im * im
   in (re / d) :+ (negate im / d)

-- | Principal nth root of an exact complex number.
-- Uses manual magnitude to avoid CReal's unsupported RealFloat operations.
complexNthRootExact :: Int -> ExactComplex -> ExactComplex
complexNthRootExact n (re :+ im) =
  let r = sqrt (re * re + im * im)
      theta = atan2 im re
      rn = r ** (1 / fromIntegral n)
      an = theta / fromIntegral n
   in (rn * cos an) :+ (rn * sin an)

-- | Evaluate a radical expression to a 'Complex Double'.
--
-- Handles expressions that pass through complex intermediates,
-- such as those arising from the casus irreducibilis in the
-- Gauss period descent for roots of unity.
--
-- For example, @cos(2*pi/7)@ is expressed using @sqrt(-3)@ and cube roots
-- of complex numbers, but the final result is real.
--
-- Uses StableName-based memoization to handle DAG-structured expressions
-- efficiently: when the same Haskell thunk (shared subexpression) is
-- encountered multiple times, it is evaluated only once.
evalComplex :: RadExpr Rational -> Complex Double
evalComplex expr = unsafePerformIO $ do
  -- Cache: hash -> [(StableName, value)] for collision handling
  cacheRef <- newIORef (IntMap.empty :: IntMap.IntMap [(StableName (RadExpr Rational), Complex Double)])
  let go e = do
        sn <- makeStableName e
        let h = hashStableName sn
        cache <- readIORef cacheRef
        let bucket = fromMaybe [] (IntMap.lookup h cache)
        case lookup' sn bucket of
          Just v -> return v
          Nothing -> do
            v <- case e of
              Lit r -> return $ fromRational r :+ 0
              Neg a -> negate <$> go a
              Add a b -> (+) <$> go a <*> go b
              Mul a b -> (*) <$> go a <*> go b
              Inv a -> (1 /) <$> go a
              Root n a -> complexNthRoot n <$> go a
              Pow a n
                | n >= 0 -> (^ n) <$> go a
                | otherwise -> (\x -> 1 / (x ^ negate n)) <$> go a
            -- Re-read cache after recursive calls to get latest state
            cache' <- readIORef cacheRef
            let bucket' = fromMaybe [] (IntMap.lookup h cache')
            writeIORef cacheRef (IntMap.insert h ((sn, v) : bucket') cache')
            return v
      lookup' _ [] = Nothing
      lookup' sn ((sn', v) : rest)
        | eqStableName sn sn' = Just v
        | otherwise = lookup' sn rest
  go expr

-- | Principal nth root of a complex number.
-- Uses polar form: @z = r*exp(i*theta)@ gives @z^(1/n) = r^(1/n)*exp(i*theta/n)@.
complexNthRoot :: Int -> Complex Double -> Complex Double
complexNthRoot n z =
  let r = magnitude z
      theta = atan2 (imagPart z) (realPart z)
   in mkPolar (r ** (1 / fromIntegral n)) (theta / fromIntegral n)
  where
    realPart (x :+ _) = x
    imagPart (_ :+ y) = y

-- | Evaluate a radical expression to a rigorous rational 'Interval' enclosure.
--
-- Each operation widens the interval to account for rounding, so the
-- true value is guaranteed to lie within the returned bounds.
-- Useful for sign determination and rigorous equality testing.
--
-- __Note:__ only handles purely real expressions. For expressions with
-- complex intermediates, use 'evalComplexInterval'.
evalInterval :: RadExpr Rational -> Interval
evalInterval (Lit r) = I.fromRational' r
evalInterval (Neg a) =
  let Interval lo hi = evalInterval a
   in Interval (negate hi) (negate lo)
evalInterval (Add a b) = I.iadd (evalInterval a) (evalInterval b)
evalInterval (Mul a b) = I.imul (evalInterval a) (evalInterval b)
evalInterval (Inv a) = I.iinv (evalInterval a)
evalInterval (Root n a) = I.inth n (evalInterval a)
evalInterval (Pow a n) = I.ipow (evalInterval a) n

-- | Evaluate a radical expression to a rigorous 'ComplexInterval' enclosure.
--
-- Handles expressions with complex intermediates (e.g., @sqrt(-3)@).
-- For even roots of negative real intervals, introduces the imaginary unit
-- automatically. General complex nth roots use rigorous interval polar
-- decomposition.
evalComplexInterval :: RadExpr Rational -> ComplexInterval
evalComplexInterval (Lit r) = I.ciFromRational r
evalComplexInterval (Neg a) = I.cineg (evalComplexInterval a)
evalComplexInterval (Add a b) = I.ciadd (evalComplexInterval a) (evalComplexInterval b)
evalComplexInterval (Mul a b) = I.cimul (evalComplexInterval a) (evalComplexInterval b)
evalComplexInterval (Inv a) = I.ciinv (evalComplexInterval a)
evalComplexInterval (Pow a n) = I.cipow (evalComplexInterval a) n
evalComplexInterval (Root n a) =
  let ci = evalComplexInterval a
      rePart = I.ciReal ci
      imPart = I.ciImag ci
   in if I.lo imPart >= 0
        && I.hi imPart <= 0 -- imaginary part is [0,0] (or very close)
        && I.lo rePart >= 0 -- non-negative real
        then I.ciFromReal (I.inth n rePart)
        else
          if I.lo imPart >= 0
            && I.hi imPart <= 0
            && I.hi rePart <= 0 -- negative real
            && odd n
            then -- Odd root of negative real
              let pos = I.inth n (Interval (negate (I.hi rePart)) (negate (I.lo rePart)))
               in ComplexInterval (Interval (negate (I.hi pos)) (negate (I.lo pos))) (I.fromRational' 0)
            else
              if I.lo imPart >= 0
                && I.hi imPart <= 0
                && I.hi rePart <= 0
                && n == 2
                then -- sqrt(negative) = i*sqrt(|x|)
                  let pos = I.isqrt (Interval (negate (I.hi rePart)) (negate (I.lo rePart)))
                   in ComplexInterval (I.fromRational' 0) pos
                else -- General complex root: use rigorous interval nth root
                  I.cinthroot n ci
