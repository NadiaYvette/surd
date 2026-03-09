-- | PSLQ algorithm for finding integer relations.
--
-- Given a vector x = (x₁, ..., xₙ) of real numbers, PSLQ finds
-- an integer vector m = (m₁, ..., mₙ) such that m·x = 0, or
-- determines that no such relation exists with coefficients below
-- a given bound.
--
-- Primary application: given α (a real algebraic number), find its
-- minimal polynomial by searching for an integer relation among
-- (1, α, α², ..., αᵈ).
--
-- Reference: Ferguson & Bailey, "A Polynomial Time, Numerically
-- Stable Integer Relation Algorithm" (1999).
module Surd.Internal.PSLQ
  ( pslq
  , findMinPoly
  ) where

import Surd.Polynomial.Univariate (Poly(..), mkPoly, monicPoly)
import Surd.Polynomial.Factoring (factorSquareFree)

-- | Simplified PSLQ (one-level) for finding integer relations.
--
-- Given reals x₁, ..., xₙ with Σ mᵢxᵢ = 0 for unknown integers mᵢ,
-- finds the integer vector m.
--
-- Returns @Just coeffs@ or @Nothing@ if no relation found within
-- iteration limit.
pslq :: [Double]    -- ^ Input vector x
     -> Int         -- ^ Maximum number of iterations
     -> Maybe [Int] -- ^ Integer relation, if found
pslq xs maxIter
  | n < 2 = Nothing
  | any isNaN xs || any isInfinite xs = Nothing
  | normX < 1e-20 = Nothing
  | otherwise = go y0 h0 a0 b0 0
  where
    n = length xs
    normX = sqrt (sum (map (^(2::Int)) xs))
    x = map (/ normX) xs

    -- Partial sums of squares: s_j = sqrt(Σ_{k=j}^{n-1} x_k²)
    ss = [sqrt (sum [x !! k ^ (2::Int) | k <- [j..n-1]]) | j <- [0..n-1]]

    -- Initial y = x (copy)
    y0 = x

    -- Initial H: n × (n-1) lower trapezoidal
    h0 = [[hij i j | j <- [0..n-2]] | i <- [0..n-1]]
    hij i j
      | i < j     = 0
      | i == j    = let sj = ss !! j; sj1 = ss !! (j+1)
                    in if sj < 1e-20 then 0 else sj1 / sj
      | otherwise = let sj = ss !! j; sj1 = ss !! (j+1)
                    in if sj < 1e-20 || sj1 < 1e-20 then 0
                       else -(x !! i) * (x !! j) / (sj * sj1)

    -- Initial A = I, B = I (as Double for now, rounded at end)
    a0 = [[if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]
    b0 = a0

    gam = sqrt 2  -- γ > 2/√3

    go y h a b iter
      | iter >= maxIter = Nothing
      | otherwise =
          -- Step 1: Select m to maximize γ^(j+1) |H_jj|
          let diags = [(j, gam ^^ (j+1) * abs (h !! j !! j)) | j <- [0..n-2]]
              m = fst $ foldl1 (\p q -> if snd p >= snd q then p else q) diags

          -- Step 2: Exchange y_m ↔ y_{m+1}, rows m↔m+1 of H and A,
          -- cols m↔m+1 of B
              y1 = swap m (m+1) y
              h1 = swapRows m (m+1) h
              a1 = swapRows m (m+1) a
              b1 = swapCols m (m+1) b

          -- Step 3: Remove the corner in H via Givens rotation
              h2 = if m < n - 2
                   then givens m h1
                   else h1

          -- Step 4: Hermite reduction
              (h3, a2, b2, y2) = hermiteReduce n h2 a1 b1 y1

          -- Step 5: Check for termination
          -- Check each column of B: does it give an integer relation?
              candidates = [col |
                             j <- [0..n-1],
                             let col = [round (b2 !! i !! j) :: Int | i <- [0..n-1]],
                             not (all (== 0) col),
                             let dot = sum [fromIntegral (col !! i) * (xs !! i) | i <- [0..n-1]],
                             abs dot < 1e-10]

          in case candidates of
               (rel:_) -> Just rel
               []      -> go y2 h3 a2 b2 (iter + 1)

-- | Givens rotation to zero out H[m+1][m].
givens :: Int -> [[Double]] -> [[Double]]
givens m h =
  let a = h !! m !! m
      b = h !! m !! (m+1)
      r = sqrt (a*a + b*b)
  in if r < 1e-20 then h
     else
       let co = a / r
           si = b / r
           nCols = length (head h)
           nRows = length h
           -- Rotate columns m and m+1 for all rows i ≥ m
           rotRow i
             | i < m = h !! i
             | otherwise =
                 [if j == m then co * (h !! i !! m) + si * idx (h !! i) (m+1)
                  else if j == m+1 then -si * (h !! i !! m) + co * idx (h !! i) (m+1)
                  else h !! i !! j
                 | j <- [0..nCols-1]]
       in [rotRow i | i <- [0..nRows-1]]

idx :: [Double] -> Int -> Double
idx xs i = if i < length xs then xs !! i else 0

-- | Hermite reduction: for i = 2..n, j = i-1..1:
-- t = nint(H[i][j] / H[j][j])
-- y[j] -= t * y[i]
-- H[i][k] -= t * H[j][k]
-- A[i][k] -= t * A[j][k]
-- B[k][j] += t * B[k][i]
hermiteReduce :: Int -> [[Double]] -> [[Double]] -> [[Double]] -> [Double]
               -> ([[Double]], [[Double]], [[Double]], [Double])
hermiteReduce n h a b y = foldl step (h, a, b, y) pairs
  where
    pairs = [(i, j) | i <- [1..n-1], j <- [i-1, i-2..0]]
    nH = n - 1  -- number of columns in H

    step (h', a', b', y') (i, j) =
      let hjj = h' !! j !! j
          t = if abs hjj > 1e-20
              then fromIntegral (round (h' !! i !! j / hjj) :: Int)
              else 0
      in if t == 0 then (h', a', b', y')
         else
           let y'' = updElem j (\yj -> yj - t * (y' !! i)) y'
               h'' = updRow i (\row -> [row !! k - t * (h' !! j !! k) | k <- [0..nH-1]]) h'
               a'' = updRow i (\row -> [row !! k - t * (a' !! j !! k) | k <- [0..n-1]]) a'
               b'' = updCol j (\k -> (b' !! k !! j) + t * (b' !! k !! i)) n b'
           in (h'', a'', b'', y'')

-- | Find the minimal polynomial of a real algebraic number.
--
-- Given a numerical approximation α and a maximum degree d,
-- searches for an integer polynomial p(x) = c₀ + c₁x + ... + cₐxᵈ
-- such that p(α) = 0.
--
-- Returns the coefficient list [c₀, c₁, ..., cₐ] (constant term first),
-- or Nothing if no relation is found up to the given degree.
findMinPoly :: Double   -- ^ Approximate value α
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

    -- Verify the candidate polynomial:
    -- 1. Leading coefficient must be non-zero
    -- 2. Coefficients must be reasonably small (spurious rational
    --    approximations from Double precision have huge coefficients)
    -- 3. Residual must be small relative to coefficient magnitude
    -- 4. Must be irreducible over Q (single factor from factorSquareFree)
    isMinPoly rel =
      let d = length rel - 1
          leadingNonZero = rel !! d /= 0
          maxCoeff = maximum (map abs rel)
          -- Reject relations with huge coefficients — these are
          -- artifacts of Double's ~15 digits of precision, not true
          -- minimal polynomials.  True minimal polynomials of
          -- algebraic numbers arising in practice have small coefficients.
          smallCoeffs = maxCoeff <= 10000
          -- Residual check scaled by coefficient magnitude
          residual = abs (sum [fromIntegral c * alpha ^ (i :: Int) | (i, c) <- zip [0..] rel])
          relResidual = residual / max 1 (fromIntegral maxCoeff)
          -- Irreducibility check: polynomial must factor into a single piece
          poly = mkPoly (map fromIntegral rel) :: Poly Rational
          factors = factorSquareFree (monicPoly poly)
          isIrreducible = length factors == 1
      in leadingNonZero && smallCoeffs && relResidual < 1e-8 && isIrreducible

-- Helpers

swap :: Int -> Int -> [a] -> [a]
swap i j xs =
  [if k == i then xs !! j else if k == j then xs !! i else xs !! k
  | k <- [0..length xs - 1]]

swapRows :: Int -> Int -> [[a]] -> [[a]]
swapRows = swap

swapCols :: Int -> Int -> [[a]] -> [[a]]
swapCols i j = map (swap i j)

updElem :: Int -> (a -> a) -> [a] -> [a]
updElem i f xs = [if k == i then f (xs !! k) else xs !! k | k <- [0..length xs - 1]]

updRow :: Int -> ([a] -> [a]) -> [[a]] -> [[a]]
updRow i f rows = [if k == i then f (rows !! k) else rows !! k | k <- [0..length rows - 1]]

updCol :: Int -> (Int -> a) -> Int -> [[a]] -> [[a]]
updCol j f nRows rows =
  [let row = rows !! k
   in [if l == j then f k else row !! l | l <- [0..length row - 1]]
  | k <- [0..nRows - 1]]
