-- | Numerical evaluation of radical expressions to arbitrary precision.
--
-- Used for testing, equality verification, and ordering.
module Surd.Radical.Eval
  ( eval
  , evalComplex
  , evalInterval
  ) where

import Data.Complex (Complex(..), magnitude, mkPolar)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName, hashStableName, eqStableName)
import Surd.Types
import Surd.Internal.Interval (Interval(..))
import qualified Surd.Internal.Interval as I

-- | Evaluate a radical expression to a 'Double'.
-- Uses floating-point arithmetic — fast but inexact.
-- Primarily useful for sanity checks and testing.
--
-- Note: expressions involving even roots of negative numbers (e.g., √(-3))
-- will produce NaN. Use 'evalComplex' for expressions that pass through
-- complex intermediates.
eval :: RadExpr Rational -> Double
eval (Lit r)    = fromRational r
eval (Neg a)    = negate (eval a)
eval (Add a b)  = eval a + eval b
eval (Mul a b)  = eval a * eval b
eval (Inv a)    = 1 / eval a
eval (Root n a) = eval a ** (1 / fromIntegral n)
eval (Pow a n)  = eval a ^^ n

-- | Evaluate a radical expression to a 'Complex Double'.
--
-- Handles expressions that pass through complex intermediates,
-- such as those arising from the casus irreducibilis in the
-- Gauss period descent for roots of unity.
--
-- For example, @cos(2π\/7)@ is expressed using @√(-3)@ and cube roots
-- of complex numbers, but the final result is real.
--
-- Uses StableName-based memoization to handle DAG-structured expressions
-- efficiently (e.g., Chebyshev polynomial trees with shared sub-expressions).
evalComplex :: RadExpr Rational -> Complex Double
evalComplex expr = unsafePerformIO $ do
  -- Cache: hash -> [(StableName, value)] for collision handling
  cacheRef <- newIORef (IntMap.empty :: IntMap.IntMap [(StableName (RadExpr Rational), Complex Double)])
  let go e = do
        sn <- makeStableName e
        let h = hashStableName sn
        cache <- readIORef cacheRef
        let bucket = maybe [] id (IntMap.lookup h cache)
        case lookup' sn bucket of
          Just v  -> return v
          Nothing -> do
            v <- case e of
              Lit r    -> return $ fromRational r :+ 0
              Neg a    -> negate <$> go a
              Add a b  -> (+) <$> go a <*> go b
              Mul a b  -> (*) <$> go a <*> go b
              Inv a    -> (1 /) <$> go a
              Root n a -> complexNthRoot n <$> go a
              Pow a n
                | n >= 0    -> (^ n) <$> go a
                | otherwise -> (\x -> 1 / (x ^ negate n)) <$> go a
            -- Re-read cache after recursive calls to get latest state
            cache' <- readIORef cacheRef
            let bucket' = maybe [] id (IntMap.lookup h cache')
            writeIORef cacheRef (IntMap.insert h ((sn, v) : bucket') cache')
            return v
      lookup' _ [] = Nothing
      lookup' sn ((sn', v) : rest)
        | eqStableName sn sn' = Just v
        | otherwise           = lookup' sn rest
  go expr

-- | Principal nth root of a complex number.
-- Uses polar form: z = r·e^(iθ) → z^(1/n) = r^(1/n)·e^(iθ/n)
complexNthRoot :: Int -> Complex Double -> Complex Double
complexNthRoot n z =
  let r     = magnitude z
      theta = atan2 (imagPart z) (realPart z)
  in mkPolar (r ** (1 / fromIntegral n)) (theta / fromIntegral n)
  where
    realPart (x :+ _) = x
    imagPart (_ :+ y) = y

-- | Evaluate a radical expression to an interval enclosure.
-- Repeatedly refining gives arbitrary precision.
evalInterval :: RadExpr Rational -> Interval
evalInterval (Lit r)    = I.fromRational' r
evalInterval (Neg a)    =
  let Interval lo hi = evalInterval a
  in Interval (negate hi) (negate lo)
evalInterval (Add a b)  = I.iadd (evalInterval a) (evalInterval b)
evalInterval (Mul a b)  = I.imul (evalInterval a) (evalInterval b)
evalInterval (Inv a)    = I.iinv (evalInterval a)
evalInterval (Root 2 a) = I.isqrt (evalInterval a)
evalInterval (Root n a) =
  -- For general nth roots, use floating-point bounds and widen slightly.
  -- This is a pragmatic initial implementation; can be replaced with
  -- proper interval nth root later.
  let d = eval (Root n a)
      eps = abs d * 1e-15 + 1e-300
  in Interval (toRational (d - eps)) (toRational (d + eps))
evalInterval (Pow a n)  = I.ipow (evalInterval a) n
