-- | Arbitrary-precision complex evaluation of radical DAGs using MPBall
-- (multi-precision ball arithmetic from aern2-mp).
--
-- This replaces both:
-- - dagEvalComplex (Complex Double) — which loses precision at depth > 50
-- - dagEvalComplexInterval (rational intervals) — which is impractically
--   slow for complex nth roots (rational atan2/cos/sin via Taylor series)
--
-- MPBall provides native sqrt as a ball operation. For general complex
-- nth roots we use Newton's method: w_{k+1} = ((n-1)·w_k + z/w_k^(n-1))/n
-- starting from a Complex Double estimate, converging quadratically.
module Surd.Radical.EvalMP
  ( dagEvalComplexMP
  , dagEvalRealMP
  , mpBallToInterval
  , complexMPToComplexInterval
  ) where

import Data.Complex (Complex(..), magnitude)
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMap
import AERN2.MP.Ball (MPBall, endpoints, mpBallP)
import AERN2.MP.Precision (Precision, prec)
import Surd.Radical.DAG (RadDAG(..), RadNodeOp(..))
import Surd.Internal.Interval (Interval(..), ComplexInterval(..))

-- | A complex number represented as a pair of MPBalls (real, imaginary).
data ComplexMP = ComplexMP !MPBall !MPBall

-- | Zero as an MPBall at given precision.
zeroBall :: Precision -> MPBall
zeroBall p = mpBallP p (0 :: Integer)

-- | One as an MPBall at given precision.
oneBall :: Precision -> MPBall
oneBall p = mpBallP p (1 :: Integer)

-- | Evaluate a DAG to a complex MPBall result at the given precision (in bits).
--
-- Each node is evaluated exactly once (O(n) in DAG size).
-- Precision is specified in bits — 200 gives ~60 decimal digits,
-- which is more than enough for depth-85 expressions.
dagEvalComplexMP :: Int -> RadDAG Rational -> ComplexInterval
dagEvalComplexMP bits dag =
  let p = prec (fromIntegral bits)
      result = evalDAG p dag
  in complexMPToComplexInterval result

-- | Evaluate a DAG and return just the real-part interval.
dagEvalRealMP :: Int -> RadDAG Rational -> Interval
dagEvalRealMP bits dag =
  let p = prec (fromIntegral bits)
      ComplexMP re _ = evalDAG p dag
  in mpBallToInterval re

-- | Convert an MPBall to our Interval type (rational endpoints).
mpBallToInterval :: MPBall -> Interval
mpBallToInterval ball =
  let (l, h) = endpoints ball
  in Interval (toRational l) (toRational h)

-- | Convert a ComplexMP to our ComplexInterval type.
complexMPToComplexInterval :: ComplexMP -> ComplexInterval
complexMPToComplexInterval (ComplexMP re im) =
  ComplexInterval (mpBallToInterval re) (mpBallToInterval im)

-- | Core DAG evaluation at a given precision.
evalDAG :: Precision -> RadDAG Rational -> ComplexMP
evalDAG p (RadDAG nodes rootId) = mpVals IntMap.! rootId
  where
    -- Double-precision evaluation for Newton starting points
    dblVals = IntMapL.map evDbl nodes
    evDbl (NLit r)    = fromRational r :+ 0
    evDbl (NNeg a)    = negate (dblVals IntMap.! a)
    evDbl (NAdd a b)  = (dblVals IntMap.! a) + (dblVals IntMap.! b)
    evDbl (NMul a b)  = (dblVals IntMap.! a) * (dblVals IntMap.! b)
    evDbl (NInv a)    = 1 / (dblVals IntMap.! a)
    evDbl (NRoot n a) = dblNthRoot n (dblVals IntMap.! a)
    evDbl (NPow a n)
      | n >= 0    = (dblVals IntMap.! a) ^ n
      | otherwise = 1 / ((dblVals IntMap.! a) ^ negate n)

    dblNthRoot :: Int -> Complex Double -> Complex Double
    dblNthRoot n z =
      let r     = magnitude z
          theta = atan2 (imagPart z) (realPart z)
      in mkPolarD (r ** (1 / fromIntegral n)) (theta / fromIntegral n)
      where
        realPart (x :+ _) = x
        imagPart (_ :+ y) = y
        mkPolarD r theta = (r * cos theta) :+ (r * sin theta)

    -- MPBall evaluation
    mpVals = IntMapL.map ev nodes

    ev :: RadNodeOp Rational -> ComplexMP
    ev (NLit r)    = ComplexMP (mpBallP p r) (zeroBall p)
    ev (NNeg a)    = cneg (mpVals IntMap.! a)
    ev (NAdd a b)  = cadd (mpVals IntMap.! a) (mpVals IntMap.! b)
    ev (NMul a b)  = cmul (mpVals IntMap.! a) (mpVals IntMap.! b)
    ev (NInv a)    = cinv p (mpVals IntMap.! a)
    ev (NPow a n)  = cpow p (mpVals IntMap.! a) n
    ev (NRoot n a) =
      let z = mpVals IntMap.! a
          z0 = dblVals IntMap.! a  -- Double estimate for Newton start
      in cnthroot p n z z0

-- Complex arithmetic on MPBall pairs

cneg :: ComplexMP -> ComplexMP
cneg (ComplexMP r i) = ComplexMP (negate r) (negate i)

cadd :: ComplexMP -> ComplexMP -> ComplexMP
cadd (ComplexMP r1 i1) (ComplexMP r2 i2) = ComplexMP (r1 + r2) (i1 + i2)

cmul :: ComplexMP -> ComplexMP -> ComplexMP
cmul (ComplexMP r1 i1) (ComplexMP r2 i2) =
  ComplexMP (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)

cinv :: Precision -> ComplexMP -> ComplexMP
cinv _ (ComplexMP r i) =
  let magSq = r * r + i * i
  in ComplexMP (r / magSq) (negate i / magSq)

cpow :: Precision -> ComplexMP -> Int -> ComplexMP
cpow p _ 0 = ComplexMP (oneBall p) (zeroBall p)
cpow _ z 1 = z
cpow p z n
  | n < 0     = cpow p (cinv p z) (negate n)
  | even n    = let half = cpow p z (n `div` 2) in cmul half half
  | otherwise = cmul z (cpow p z (n - 1))

-- | Complex nth root via Newton's method.
-- w_{k+1} = ((n-1)·w_k + z/w_k^(n-1)) / n
-- Starting from Complex Double estimate, converges quadratically.
cnthroot :: Precision -> Int -> ComplexMP -> Complex Double -> ComplexMP
cnthroot p n z z0Dbl
  -- Real non-negative: use real nth root
  | isNonNegReal =
      ComplexMP (nthRootMP p n re) (zeroBall p)
  -- Real negative, odd n: negate and root
  | isNegReal && odd n =
      ComplexMP (negate (nthRootMP p n (negate re))) (zeroBall p)
  -- Real negative, n=2: √(-x) = i·√x
  | isNegReal && n == 2 =
      ComplexMP (zeroBall p) (sqrt (negate re))
  -- General complex root via Newton
  | otherwise =
      let -- Start from Double estimate
          w0Dbl = dblNthRoot n z0Dbl
          w0 = ComplexMP (mpBallP p (toRational (realP w0Dbl)))
                         (mpBallP p (toRational (imagP w0Dbl)))
          nBall = mpBallP p (fromIntegral n :: Integer)
          n1Ball = mpBallP p (fromIntegral (n - 1) :: Integer)
          nC = ComplexMP nBall (zeroBall p)
          n1C = ComplexMP n1Ball (zeroBall p)
          step w = let wn1 = cpow p w (n - 1)
                   in cdiv p (cadd (cmul n1C w) (cdiv p z wn1)) nC
      in iterate step w0 !! 20
  where
    ComplexMP re im = z
    isNonNegReal = isZeroBall im && isNonNegBall re
    isNegReal = isZeroBall im && isNegBall re

    realP (x :+ _) = x
    imagP (_ :+ y) = y

    dblNthRoot :: Int -> Complex Double -> Complex Double
    dblNthRoot m w =
      let r     = magnitude w
          theta = atan2 (imagP w) (realP w)
      in (r ** (1 / fromIntegral m) * cos (theta / fromIntegral m))
         :+ (r ** (1 / fromIntegral m) * sin (theta / fromIntegral m))

cdiv :: Precision -> ComplexMP -> ComplexMP -> ComplexMP
cdiv p a b = cmul a (cinv p b)

-- | Compute the real nth root of a non-negative MPBall.
nthRootMP :: Precision -> Int -> MPBall -> MPBall
nthRootMP _ 2 x = sqrt x
nthRootMP p n x = newtonNthRoot p n x

-- | Newton's method for real nth root.
newtonNthRoot :: Precision -> Int -> MPBall -> MPBall
newtonNthRoot p n x
  | isZeroBall x = zeroBall p
  | otherwise =
      let (xlo, _) = endpoints x
          xd = fromRational (toRational xlo) :: Double
          y0d = xd ** (1 / fromIntegral n)
          y0 = mpBallP p (toRational y0d)
          nBall = mpBallP p (fromIntegral n :: Integer)
          n1Ball = mpBallP p (fromIntegral (n - 1) :: Integer)
          step y = (n1Ball * y + x / y ^ (n - 1)) / nBall
      in iterate step y0 !! 20

-- Predicates on MPBalls (checking endpoints)

isZeroBall :: MPBall -> Bool
isZeroBall b =
  let (l, h) = endpoints b
  in toRational l >= 0 && toRational h <= 0

isNonNegBall :: MPBall -> Bool
isNonNegBall b =
  let (l, _) = endpoints b
  in toRational l >= 0

isNegBall :: MPBall -> Bool
isNegBall b =
  let (_, h) = endpoints b
  in toRational h < 0
