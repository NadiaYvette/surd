-- |
-- Module      : Surd.Radical.EvalMP
-- Description : Arbitrary-precision DAG evaluation via MPBall (aern2-mp)
-- Stability   : experimental
--
-- Provides arbitrary-precision complex evaluation of radical DAGs using
-- multi-precision ball arithmetic from the @aern2-mp@ package. This
-- replaces both:
--
-- * 'Surd.Radical.DAG.dagEvalComplex' ('Complex Double') -- which loses
--   precision at depth > 50
-- * 'Surd.Radical.DAG.dagEvalComplexInterval' (rational intervals) --
--   which is impractically slow for complex nth roots (rational
--   atan2\/cos\/sin via Taylor series)
--
-- MPBall provides native @sqrt@ as a ball operation. For general complex
-- nth roots, Newton's method is used:
--
-- @
-- w_{k+1} = ((n-1) * w_k + z / w_k^(n-1)) / n
-- @
--
-- Starting from a 'Complex Double' estimate (computed from the MPBall
-- midpoint, not the raw Double evaluation chain, to avoid phase errors
-- for near-zero values), this converges quadratically in 20 iterations.
module Surd.Radical.EvalMP
  ( -- * DAG evaluation
    dagEvalComplexMP,
    dagEvalRealMP,

    -- * Conversion utilities
    mpBallToInterval,
    complexMPToComplexInterval,

    -- * DFT coefficients at high precision
    dftCoeffsMP,
  )
where

import AERN2.MP.Ball (MPBall, endpoints, mpBallP)
import AERN2.MP.Precision (Precision, prec)
import Data.Complex (Complex (..), magnitude)
import Data.IntMap.Lazy qualified as IntMapL
import Data.IntMap.Strict qualified as IntMap
import Math.Internal.Interval (ComplexInterval (..), Interval (..))
import Surd.Radical.DAG (RadDAG (..), RadNodeOp (..))

-- | A complex number represented as a pair of MPBalls (real, imaginary).
data ComplexMP = ComplexMP !MPBall !MPBall

-- | Zero as an MPBall at given precision.
zeroBall :: Precision -> MPBall
zeroBall p = mpBallP p (0 :: Integer)

-- | One as an MPBall at given precision.
oneBall :: Precision -> MPBall
oneBall p = mpBallP p (1 :: Integer)

-- | Evaluate a DAG to a complex result at the given precision (in bits).
--
-- Each node is evaluated exactly once (O(n) in DAG size). The result is
-- returned as a 'ComplexInterval' (rational endpoint pairs).
--
-- Precision is specified in bits: 200 gives ~60 decimal digits, 500
-- gives ~150 decimal digits. 200 bits is typically more than enough
-- for expressions of depth up to 85.
dagEvalComplexMP :: Int -> RadDAG Rational -> ComplexInterval
dagEvalComplexMP bits dag =
  let p = prec (fromIntegral bits)
      result = evalDAG p dag
   in complexMPToComplexInterval result

-- | Evaluate a DAG and return just the real-part interval.
--
-- Convenience wrapper around 'dagEvalComplexMP' for expressions known
-- to be real-valued.
dagEvalRealMP :: Int -> RadDAG Rational -> Interval
dagEvalRealMP bits dag =
  let p = prec (fromIntegral bits)
      ComplexMP re _ = evalDAG p dag
   in mpBallToInterval re

-- | Convert an 'MPBall' to a rational 'Interval' (extracting endpoints).
mpBallToInterval :: MPBall -> Interval
mpBallToInterval ball =
  let (l, h) = endpoints ball
   in Interval (toRational l) (toRational h)

-- | Convert a 'ComplexMP' to a 'ComplexInterval'.
complexMPToComplexInterval :: ComplexMP -> ComplexInterval
complexMPToComplexInterval (ComplexMP re im) =
  ComplexInterval (mpBallToInterval re) (mpBallToInterval im)

-- | Core DAG evaluation at a given precision.
evalDAG :: Precision -> RadDAG Rational -> ComplexMP
evalDAG p (RadDAG nodes rootId) = mpVals IntMap.! rootId
  where
    -- Double-precision evaluation for Newton starting points
    dblVals = IntMapL.map evDbl nodes
    evDbl (NLit r) = fromRational r :+ 0
    evDbl (NNeg a) = negate (dblVals IntMap.! a)
    evDbl (NAdd a b) = (dblVals IntMap.! a) + (dblVals IntMap.! b)
    evDbl (NMul a b) = (dblVals IntMap.! a) * (dblVals IntMap.! b)
    evDbl (NInv a) = 1 / (dblVals IntMap.! a)
    evDbl (NRoot n a) = dblNthRoot n (dblVals IntMap.! a)
    evDbl (NPow a n)
      | n >= 0 = (dblVals IntMap.! a) ^ n
      | otherwise = 1 / ((dblVals IntMap.! a) ^ negate n)

    dblNthRoot :: Int -> Complex Double -> Complex Double
    dblNthRoot n z =
      let r = magnitude z
          theta = atan2 (imagPart z) (realPart z)
       in mkPolarD (r ** (1 / fromIntegral n)) (theta / fromIntegral n)
      where
        realPart (x :+ _) = x
        imagPart (_ :+ y) = y
        mkPolarD r theta = (r * cos theta) :+ (r * sin theta)

    -- MPBall evaluation
    mpVals = IntMapL.map ev nodes

    ev :: RadNodeOp Rational -> ComplexMP
    ev (NLit r) = ComplexMP (mpBallP p r) (zeroBall p)
    ev (NNeg a) = cneg (mpVals IntMap.! a)
    ev (NAdd a b) = cadd (mpVals IntMap.! a) (mpVals IntMap.! b)
    ev (NMul a b) = cmul (mpVals IntMap.! a) (mpVals IntMap.! b)
    ev (NInv a) = cinv p (mpVals IntMap.! a)
    ev (NPow a n) = cpow p (mpVals IntMap.! a) n
    ev (NRoot n a) =
      let z = mpVals IntMap.! a
          z0 = dblVals IntMap.! a -- Double estimate for Newton start
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
  | n < 0 = cpow p (cinv p z) (negate n)
  | even n = let half = cpow p z (n `div` 2) in cmul half half
  | otherwise = cmul z (cpow p z (n - 1))

-- | Complex nth root via Newton's method.
--
-- Uses the iteration @w_{k+1} = ((n-1)*w_k + z/w_k^(n-1)) / n@
-- starting from a 'Complex Double' estimate derived from the MPBall
-- midpoint (using 'complexMPToDouble'). Converges quadratically;
-- 20 iterations suffice for any practical precision.
--
-- __Important:__ the starting point must come from the MPBall midpoint
-- (not the raw Double evaluation chain), because for near-zero
-- resolvent values @R_j^q@, Double's phase can be garbage, causing
-- Newton to converge to the wrong root.
--
-- Special cases (real non-negative, real negative odd, sqrt of negative)
-- are handled directly without Newton iteration.
cnthroot :: Precision -> Int -> ComplexMP -> Complex Double -> ComplexMP
cnthroot p n z _z0Dbl
  -- Real non-negative: use real nth root
  | isNonNegReal =
      ComplexMP (nthRootMP p n re) (zeroBall p)
  -- Real negative, odd n: negate and root
  | isNegReal && odd n =
      ComplexMP (negate (nthRootMP p n (negate re))) (zeroBall p)
  -- Real negative, n=2: sqrt(-x) = i*sqrt(x)
  | isNegReal && n == 2 =
      ComplexMP (zeroBall p) (sqrt (negate re))
  -- General complex root via Newton
  | otherwise =
      let -- Start from MPBall midpoint (more accurate than Double chain
          -- for near-zero values where Double's phase is garbage).
          zMid = complexMPToDouble z
          w0Dbl = dblNthRoot n zMid
          w0 =
            ComplexMP
              (mpBallP p (toRational (realP w0Dbl)))
              (mpBallP p (toRational (imagP w0Dbl)))
          nBall = mpBallP p (fromIntegral n :: Integer)
          n1Ball = mpBallP p (fromIntegral (n - 1) :: Integer)
          nC = ComplexMP nBall (zeroBall p)
          n1C = ComplexMP n1Ball (zeroBall p)
          step w =
            let wn1 = cpow p w (n - 1)
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
      let r = magnitude w
          theta = atan2 (imagP w) (realP w)
       in (r ** (1 / fromIntegral m) * cos (theta / fromIntegral m))
            :+ (r ** (1 / fromIntegral m) * sin (theta / fromIntegral m))

cdiv :: Precision -> ComplexMP -> ComplexMP -> ComplexMP
cdiv p a b = cmul a (cinv p b)

-- | Compute the real nth root of a non-negative MPBall.
-- Uses native @sqrt@ for @n=2@, Newton's method for general @n@.
nthRootMP :: Precision -> Int -> MPBall -> MPBall
nthRootMP _ 2 x = sqrt x
nthRootMP p n x = newtonNthRoot p n x

-- | Newton's method for real nth root of a non-negative MPBall.
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

-- | Compute DFT coefficients @d_s@ at high precision (1000-bit MPBall).
--
-- Given @q@ sub-periods (as lists of exponents of @zeta = e^{2*pi*i/p}@),
-- computes:
--
-- 1. @eta_k = sum(zeta^elem)@ for each sub-period (period values at 1000-bit precision)
-- 2. @R_j = sum(omega_q^{j*k} * eta_k)@ (Lagrange resolvents)
-- 3. @R_j^q@ (qth powers of resolvents)
-- 4. @d_s = (1/q) * sum(omega_q^{-j*s} * R_j^q)@ (inverse DFT)
--
-- Also computes period values for additional element lists (@periodElemLists@),
-- using the same @zeta@ powers for precision consistency.
--
-- @d_s@ and period values are returned as @(Rational, Rational)@ pairs
-- (real, imaginary midpoints) to preserve full precision for integer
-- coefficient matching. For @p=89, q=11@, @d_s ~ 10^8@ and coefficients
-- @~ 10^7@, requiring > 15 significant digits (beyond Double's capacity).
--
-- @R_j@ values are returned as 'Complex Double' (sufficient for branch
-- selection during Lagrange resolvent construction).
dftCoeffsMP ::
  Int ->
  Integer ->
  [[Integer]] ->
  [[Integer]] ->
  ([(Rational, Rational)], [Complex Double], [Complex Double], [(Rational, Rational)])
dftCoeffsMP q p subPeriodElems periodElemLists =
  let pr = prec 1000
      -- zeta = e^{2*pi*i/p} at high precision
      twoPiOverP = mpBallP pr (2 :: Integer) * piMP pr / mpBallP pr (fromIntegral p :: Integer)
      zetaRe = cos twoPiOverP
      zetaIm = sin twoPiOverP
      zeta = ComplexMP zetaRe zetaIm
      oneC = ComplexMP (oneBall pr) (zeroBall pr)
      -- zeta^k computed via repeated multiplication
      -- Pre-compute all needed powers of zeta
      zetaPow :: Integer -> ComplexMP
      zetaPow k =
        let k' = k `mod` p
         in cpow pr oneC 0 `seq` zetaPows !! fromIntegral k'
      zetaPows = take (fromIntegral p) $ iterate (cmul zeta) oneC
      -- Sub-period values: eta_k = sum(zeta^elem)
      valsMP =
        [ foldl1 cadd [zetaPow e | e <- elems]
          | elems <- subPeriodElems
        ]
      -- Period values using the same zeta powers (precision-consistent)
      periodVals =
        [ complexMPToRatPair (foldl1 cadd [zetaPow e | e <- elems])
          | elems <- periodElemLists
        ]
      -- omega_q = e^{2*pi*i/q} at high precision
      twoPiOverQ = mpBallP pr (2 :: Integer) * piMP pr / mpBallP pr (fromIntegral q :: Integer)
      omegaQ = ComplexMP (cos twoPiOverQ) (sin twoPiOverQ)
      omegaPowsMP = take q $ iterate (cmul omegaQ) oneC
      -- R_j = sum_{k=0}^{q-1} omega_q^{j*k} * eta_k
      resolventMP j =
        foldl1
          cadd
          [ cmul (omegaPowsMP !! ((j * k) `mod` q)) (valsMP !! k)
            | k <- [0 .. q - 1]
          ]
      resolventsMP = [resolventMP j | j <- [0 .. q - 1]]
      -- R_j^q
      resolventPowsMP = map (cpowMP pr q) resolventsMP
      -- d_s = (1/q) sum_j omega_q^{-j*s} * R_j^q
      recipQ = ComplexMP (mpBallP pr (recip (fromIntegral q) :: Rational)) (zeroBall pr)
      dCoeffMP s =
        cmul recipQ $
          foldl1
            cadd
            [ cmul
                (omegaPowsMP !! ((q - ((j * s) `mod` q)) `mod` q))
                (resolventPowsMP !! j)
              | j <- [0 .. q - 1]
            ]
      dCoeffs = [complexMPToRatPair (dCoeffMP s) | s <- [0 .. q - 1]]
      resolvents = map complexMPToDouble resolventsMP
      resolventPows = map complexMPToDouble resolventPowsMP
   in (dCoeffs, resolvents, resolventPows, periodVals)

-- | Convert ComplexMP to (Rational, Rational) midpoints, preserving full precision.
complexMPToRatPair :: ComplexMP -> (Rational, Rational)
complexMPToRatPair (ComplexMP re im) =
  let mid b =
        let (l, h) = endpoints b
         in (toRational l + toRational h) / 2
   in (mid re, mid im)

-- | Compute pi as an MPBall at given precision.
-- Uses Newton refinement of @sin(x) = 0@ starting from Double pi:
-- @x_{n+1} = x_n - sin(x_n)/cos(x_n) = x_n - tan(x_n)@.
-- Quadratic convergence: 53 bits -> 106 -> 212 -> ... exceeds any
-- practical precision in ~15 iterations.
piMP :: Precision -> MPBall
piMP p =
  let x0 = mpBallP p (toRational (pi :: Double))
      step x = x - tan x
   in iterate step x0 !! 15

-- | Complex power by repeated squaring (MPBall version).
cpowMP :: Precision -> Int -> ComplexMP -> ComplexMP
cpowMP p n z = cpow p z n

-- | Convert a 'ComplexMP' to 'Complex Double' (using midpoints of the balls).
--
-- This extracts the centre of each MPBall as a Double, discarding the
-- error bounds. Used primarily as a starting point for Newton iteration
-- or for branch selection in Lagrange resolvents.
complexMPToDouble :: ComplexMP -> Complex Double
complexMPToDouble (ComplexMP re im) =
  let mid b =
        let (l, h) = endpoints b
         in fromRational ((toRational l + toRational h) / 2) :: Double
   in mid re :+ mid im
