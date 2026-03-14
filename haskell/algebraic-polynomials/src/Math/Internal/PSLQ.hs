-- |
-- Module      : Math.Internal.PSLQ
-- Description : PSLQ integer relation algorithm
-- Stability   : experimental
--
-- Implementation of the PSLQ algorithm for finding integer relations
-- among real numbers.
--
-- Given a vector \(\mathbf{x} = (x_1, \ldots, x_n)\) of real numbers,
-- PSLQ finds an integer vector \(\mathbf{m} = (m_1, \ldots, m_n)\) such
-- that \(\mathbf{m} \cdot \mathbf{x} = \sum m_i x_i = 0\), or determines
-- that no such relation exists with coefficients below a given bound.
--
-- __Primary application:__ given a real algebraic number \(\alpha\) and a
-- maximum degree \(d\), find its minimal polynomial by searching for an
-- integer relation among \((1, \alpha, \alpha^2, \ldots, \alpha^d)\).
--
-- __Algorithm:__ simplified one-level PSLQ following Ferguson & Bailey (1999).
-- Uses an \(n \times (n-1)\) lower trapezoidal matrix \(H\) and integer
-- transformation matrices \(A\), \(B\). At each iteration:
--
--   1. Select pivot \(m\) maximising \(\gamma^{j+1} |H_{jj}|\).
--   2. Swap rows \(m\) and \(m+1\) in \(H\) and \(A\), columns in \(B\).
--   3. Restore \(H\) to lower trapezoidal form via Givens rotation.
--   4. Hermite-reduce \(H\) (and update \(A\), \(B\), \(\mathbf{y}\)).
--   5. Check columns of \(B\) for an integer relation.
--
-- Matrix operations use @linear-massiv@ for \(O(1)\) indexing and
-- numerically stable Givens rotations.
--
-- Reference: Ferguson & Bailey, \"A Polynomial Time, Numerically
-- Stable Integer Relation Algorithm\" (1999).
{-# LANGUAGE AllowAmbiguousTypes #-}
module Math.Internal.PSLQ
  ( pslq
  , findMinPoly
  ) where

import Data.Proxy (Proxy)
import GHC.TypeNats (KnownNat)
import qualified Data.Massiv.Array as M

import Numeric.LinearAlgebra.Massiv.Types (Matrix, Vector)
import Numeric.LinearAlgebra.Massiv.Internal
  (makeMatrix, makeVector, reifyDim2, (!), (!.))
import Numeric.LinearAlgebra.Massiv.Orthogonal.Givens (givensRotation)

import Math.Polynomial.Univariate (Poly(..), mkPoly, monicPoly)
import Math.Polynomial.Factoring (factorSquareFree)

-- | Find an integer relation among a vector of real numbers.
--
-- Given reals \(x_1, \ldots, x_n\) (as 'Double' approximations),
-- searches for integers \(m_1, \ldots, m_n\) (not all zero) such that
-- \(\sum m_i x_i = 0\).
--
-- Returns @'Just' [m1, ..., mn]@ if a relation is found within the
-- iteration limit, or @'Nothing'@ otherwise.
--
-- The input vector must have at least 2 elements, contain no NaN or
-- Infinity values, and have non-negligible norm.
--
-- >>> pslq [1, -0.5, 0.5] 1000
-- Just [1,1,-1]   -- meaning 1 - 0.5 + (-1)*0.5 = 0... but not quite
pslq :: [Double]    -- ^ Input vector x
     -> Int         -- ^ Maximum number of iterations
     -> Maybe [Int] -- ^ Integer relation, if found
pslq xs maxIter
  | n < 2 = Nothing
  | any isNaN xs || any isInfinite xs = Nothing
  | normX < 1e-20 = Nothing
  | otherwise =
      -- Reify dimensions: n rows, (n-1) columns for H; n x n for A, B.
      reifyDim2 n (n - 1) $ \(_ :: Proxy m) (_ :: Proxy nh) ->
        pslqGo @m @nh xs maxIter n normX
  where
    n = length xs
    normX = sqrt (sum (map (^(2::Int)) xs))

-- | Core PSLQ loop, parameterised by type-level dimensions.
--
-- @m@ = n (vector\/matrix dimension), @nh@ = n-1 (H column count).
pslqGo :: forall m nh.
          (KnownNat m, KnownNat nh)
       => [Double] -> Int -> Int -> Double -> Maybe [Int]
pslqGo xs maxIter n normX = go y0 h0 a0 b0 0
  where
    x = map (/ normX) xs

    -- Partial sums of squares: s_j = sqrt(sum_{k=j}^{n-1} x_k^2)
    xArr = M.fromList M.Seq x :: M.Array M.P M.Ix1 Double
    ss = makeVector @m @M.P $ \j ->
      sqrt (sum [M.index' xArr k ^ (2::Int) | k <- [j..n-1]])

    -- Initial y = x (normalised)
    y0 = makeVector @m @M.P $ \i -> M.index' xArr i

    -- Initial H: n x (n-1) lower trapezoidal matrix
    h0 :: Matrix m nh M.P Double
    h0 = makeMatrix @m @nh @M.P $ \i j ->
      let sj  = ss !. j
          sj1 = ss !. (j + 1)
      in if i < j then 0
         else if i == j then
           if sj < 1e-20 then 0 else sj1 / sj
         else
           if sj < 1e-20 || sj1 < 1e-20 then 0
           else -(M.index' xArr i) * (M.index' xArr j) / (sj * sj1)

    -- Initial A = I, B = I
    a0 :: Matrix m m M.P Double
    a0 = makeMatrix @m @m @M.P $ \i j -> if i == j then 1 else 0

    b0 :: Matrix m m M.P Double
    b0 = a0

    gam = sqrt 2 :: Double  -- gamma > 2/sqrt(3)

    go :: Vector m M.P Double
       -> Matrix m nh M.P Double
       -> Matrix m m M.P Double
       -> Matrix m m M.P Double
       -> Int -> Maybe [Int]
    go y h a b iter
      | iter >= maxIter = Nothing
      | otherwise =
          -- Step 1: Select m to maximize gamma^(j+1) |H_jj|
          let findBestDiag best j
                | j >= n - 1 = best
                | otherwise =
                    let val = gam ^^ (j + 1) * abs (h ! (j, j))
                    in findBestDiag (if val > snd best then (j, val) else best) (j + 1)
              (pivotM, _) = findBestDiag (0, gam * abs (h ! (0, 0))) 1

          -- Step 2: Exchange y_m <-> y_{m+1}, rows m<->m+1 of H and A,
          -- cols m<->m+1 of B
              y1 = makeVector @m @M.P $ \i ->
                if i == pivotM then y !. (pivotM + 1)
                else if i == pivotM + 1 then y !. pivotM
                else y !. i
              h1 = makeMatrix @m @nh @M.P $ \i j ->
                if i == pivotM then h ! (pivotM + 1, j)
                else if i == pivotM + 1 then h ! (pivotM, j)
                else h ! (i, j)
              a1 = makeMatrix @m @m @M.P $ \i j ->
                if i == pivotM then a ! (pivotM + 1, j)
                else if i == pivotM + 1 then a ! (pivotM, j)
                else a ! (i, j)
              b1 = makeMatrix @m @m @M.P $ \i j ->
                if j == pivotM then b ! (i, pivotM + 1)
                else if j == pivotM + 1 then b ! (i, pivotM)
                else b ! (i, j)

          -- Step 3: Remove the corner in H via Givens rotation.
              h2 = if pivotM < n - 2
                   then let hA = h1 ! (pivotM, pivotM)
                            hB = h1 ! (pivotM, pivotM + 1)
                            (co, si) = givensRotation hA hB
                        in makeMatrix @m @nh @M.P $ \i j ->
                             if j == pivotM then
                               co * h1 ! (i, pivotM) + si * safeIdx h1 i (pivotM + 1) (n - 1)
                             else if j == pivotM + 1 then
                               -si * h1 ! (i, pivotM) + co * safeIdx h1 i (pivotM + 1) (n - 1)
                             else h1 ! (i, j)
                   else h1

          -- Step 4: Hermite reduction
              (h3, a2, b2, y2) = hermiteReduce @m @nh n h2 a1 b1 y1

          -- Step 5: Check for termination
              checkCol j
                | j >= n = Nothing
                | otherwise =
                    let col = [round (b2 ! (i, j)) :: Int | i <- [0..n-1]]
                        nonZero = any (/= 0) col
                        dot = sum [fromIntegral (col !! i) * (xs !! i) | i <- [0..n-1]]
                    in if nonZero && abs dot < 1e-10
                       then Just col
                       else checkCol (j + 1)

          in case checkCol 0 of
               Just rel -> Just rel
               Nothing  -> go y2 h3 a2 b2 (iter + 1)

-- | Safe column index for H (n x (n-1)): returns 0 for out-of-bounds columns.
safeIdx :: M.Manifest r e => Num e => Matrix m1 n1 r e -> Int -> Int -> Int -> e
safeIdx h i j nCols
  | j < nCols = h ! (i, j)
  | otherwise = 0

-- | Hermite reduction of the H matrix.
--
-- For \(i = 2, \ldots, n\) and \(j = i-1, \ldots, 1\):
-- round \(H_{ij}/H_{jj}\) to the nearest integer \(t\), then subtract
-- \(t\) times row \(j\) from row \(i\) in \(H\) and \(A\), and add
-- \(t\) times column \(i\) to column \(j\) in \(B\).
hermiteReduce :: forall m nh.
                 (KnownNat m, KnownNat nh)
              => Int
              -> Matrix m nh M.P Double
              -> Matrix m m M.P Double
              -> Matrix m m M.P Double
              -> Vector m M.P Double
              -> (Matrix m nh M.P Double, Matrix m m M.P Double,
                  Matrix m m M.P Double, Vector m M.P Double)
hermiteReduce n h0 a0 b0 y0 = foldl step (h0, a0, b0, y0) pairs
  where
    pairs = [(i, j) | i <- [1..n-1], j <- [i-1, i-2..0]]

    step (h, a, b, y) (i, j) =
      let hjj = h ! (j, j)
          t = if abs hjj > 1e-20
              then fromIntegral (round (h ! (i, j) / hjj) :: Int)
              else 0 :: Double
      in if t == 0 then (h, a, b, y)
         else
           ( makeMatrix @m @nh @M.P $ \r c ->
               if r == i then h ! (i, c) - t * h ! (j, c) else h ! (r, c)
           , makeMatrix @m @m @M.P $ \r c ->
               if r == i then a ! (i, c) - t * a ! (j, c) else a ! (r, c)
           , makeMatrix @m @m @M.P $ \r c ->
               if c == j then b ! (r, j) + t * b ! (r, i) else b ! (r, c)
           , makeVector @m @M.P $ \k ->
               if k == j then y !. j - t * y !. i else y !. k
           )

-- | Find the minimal polynomial of a real algebraic number.
--
-- Given a numerical approximation \(\alpha\) (as a 'Double') and a maximum
-- degree \(d\), searches for an integer polynomial
-- \(p(x) = c_0 + c_1 x + \cdots + c_d x^d\) such that \(p(\alpha) = 0\).
--
-- Uses PSLQ to find integer relations among the powers
-- \((1, \alpha, \alpha^2, \ldots, \alpha^d)\), trying each degree from 1
-- up to @maxDeg@. Candidates are validated by checking:
--
--   * Non-zero leading coefficient
--   * Coefficients bounded by 10000 (rejects spurious relations from
--     limited 'Double' precision)
--   * Small relative residual (\(< 10^{-8}\))
--   * Irreducibility over \(\mathbb{Q}\) (via 'factorSquareFree')
--
-- Returns the coefficient list @[c0, c1, ..., cd]@ (constant term first),
-- or @Nothing@ if no relation is found.
findMinPoly :: Double   -- ^ Approximate value alpha
            -> Int      -- ^ Maximum degree to search
            -> Maybe [Int]
findMinPoly alpha maxDeg = tryDegree 1
  where
    tryDegree d
      | d > maxDeg = Nothing
      | otherwise =
          let powers = [alpha ^ (i :: Int) | i <- [0..d]]
          in case pslq powers 2000 of
               Just rel | not (all (== 0) rel) && isMinPoly rel ->
                 Just rel
               _ -> tryDegree (d + 1)

    -- Verify the candidate polynomial
    isMinPoly rel =
      let d = length rel - 1
          leadingNonZero = rel !! d /= 0
          maxCoeff = maximum (map abs rel)
          smallCoeffs = maxCoeff <= 10000
          residual = abs (sum [fromIntegral c * alpha ^ (i :: Int) | (i, c) <- zip [0..] rel])
          relResidual = residual / max 1 (fromIntegral maxCoeff)
          poly = mkPoly (map fromIntegral rel) :: Poly Rational
          factors = factorSquareFree (monicPoly poly)
          isIrreducible = length factors == 1
      in leadingNonZero && smallCoeffs && relResidual < 1e-8 && isIrreducible
