{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Math.Polynomial.Univariate
-- Description : Dense univariate polynomials over arbitrary rings
-- Stability   : experimental
--
-- Univariate polynomial arithmetic with coefficients in any 'Num' instance.
-- Polynomials are represented as coefficient lists, low-degree first:
-- @[a0, a1, ..., an]@ means \(a_0 + a_1 x + \cdots + a_n x^n\).
--
-- __Invariant:__ trailing zeros are stripped (the leading coefficient is nonzero),
-- except for the zero polynomial which is @[]@.
--
-- Pattern synonyms allow convenient construction and matching:
--
-- >>> X + 3 :: Poly Rational       -- the polynomial x + 3
-- >>> case p of { X -> "is x"; _ -> "other" }
module Math.Polynomial.Univariate
  ( Poly(..)
  , mkPoly
  , zeroPoly
  , constPoly
  , monoX
  , pattern X
  , degree
  , leadCoeff
  , evalPoly
  , scalePoly
  , addPoly
  , subPoly
  , mulPoly
  , divModPoly
  , gcdPoly
  , monicPoly
  , diffPoly
  , composePoly
  , squareFree
  ) where

-- | A univariate polynomial with coefficients in @k@.
--
-- Stored as a list of coefficients in ascending degree order:
-- @Poly [a0, a1, a2]@ represents \(a_0 + a_1 x + a_2 x^2\).
--
-- Use 'mkPoly' for construction (strips trailing zeros) or the 'Num'
-- instance for arithmetic.
newtype Poly k = Poly { unPoly :: [k] }
  deriving (Eq, Show, Functor)

-- | Polynomial arithmetic via 'Num'. Enables natural syntax:
--
-- >>> (X + 1) * (X - 1) :: Poly Rational
-- Poly [-1, 0, 1]
instance (Eq k, Num k) => Num (Poly k) where
  (+) = addPoly
  (-) = subPoly
  (*) = mulPoly
  negate (Poly cs) = Poly (map negate cs)
  abs    = id
  signum _ = Poly [1]
  fromInteger n
    | n == 0    = Poly []
    | otherwise = Poly [fromInteger n]

-- | Smart constructor: build a polynomial from a coefficient list (ascending
-- degree order), stripping trailing zeros to maintain the representation
-- invariant.
--
-- >>> mkPoly [1, 0, 0]
-- Poly [1]
-- >>> mkPoly [0, 0, 0]
-- Poly []
mkPoly :: (Eq k, Num k) => [k] -> Poly k
mkPoly = Poly . stripZeros

stripZeros :: (Eq k, Num k) => [k] -> [k]
stripZeros = reverse . dropWhile (== 0) . reverse

-- | The zero polynomial (no terms).
zeroPoly :: Poly k
zeroPoly = Poly []

-- | Construct a constant polynomial.
-- Returns 'zeroPoly' for a zero coefficient.
--
-- >>> constPoly 5 :: Poly Rational
-- Poly [5 % 1]
constPoly :: (Eq k, Num k) => k -> Poly k
constPoly 0 = zeroPoly
constPoly c = Poly [c]

-- | The polynomial \(x\).
monoX :: Num k => Poly k
monoX = Poly [0, 1]

-- | Bidirectional pattern for the indeterminate \(x\).
--
-- __Construction:__ @X :: Poly Rational@ builds the polynomial \(x\).
--
-- __Matching:__ @case p of X -> ...; _ -> ...@ matches when @p@ is exactly \(x\).
pattern X :: (Eq k, Num k) => Poly k
pattern X <- (isX -> True)
  where X = Poly [0, 1]

isX :: (Eq k, Num k) => Poly k -> Bool
isX (Poly [a, b]) = a == 0 && b == 1
isX _             = False

-- | Degree of the polynomial.
--
-- Returns @-1@ for the zero polynomial, following the convention that
-- \(\deg(0) = -1\) (or \(-\infty\) in some texts).
--
-- >>> degree (mkPoly [1, 2, 3] :: Poly Rational)
-- 2
-- >>> degree (zeroPoly :: Poly Rational)
-- -1
degree :: Poly k -> Int
degree (Poly []) = -1
degree (Poly cs) = length cs - 1

-- | Leading coefficient (coefficient of the highest-degree term).
--
-- Returns @Nothing@ for the zero polynomial.
--
-- >>> leadCoeff (mkPoly [1, 2, 3] :: Poly Rational)
-- Just 3
leadCoeff :: Poly k -> Maybe k
leadCoeff (Poly []) = Nothing
leadCoeff (Poly cs) = Just (last cs)

-- | Evaluate a polynomial at a point using Horner's method.
--
-- \(p(x) = a_0 + x(a_1 + x(a_2 + \cdots))\)
--
-- This is numerically stable and uses \(O(n)\) multiplications
-- where \(n\) is the degree.
--
-- >>> evalPoly (mkPoly [1, 2, 1] :: Poly Rational) 3
-- 16 % 1
evalPoly :: Num k => Poly k -> k -> k
evalPoly (Poly []) _ = 0
evalPoly (Poly cs) x = foldr (\c acc -> c + x * acc) 0 cs

-- | Multiply a polynomial by a scalar.
--
-- >>> scalePoly 3 (mkPoly [1, 2] :: Poly Rational)
-- Poly [3 % 1,6 % 1]
scalePoly :: (Eq k, Num k) => k -> Poly k -> Poly k
scalePoly 0 _ = zeroPoly
scalePoly s (Poly cs) = mkPoly (map (* s) cs)

-- | Add two polynomials.
addPoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
addPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault 0 (+) as bs)

-- | Subtract two polynomials.
subPoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
subPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault 0 (-) as bs)

zipWithDefault :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipWithDefault _   _ []     []     = []
zipWithDefault def f (a:as) []     = f a def : zipWithDefault def f as []
zipWithDefault def f []     (b:bs) = f def b : zipWithDefault def f [] bs
zipWithDefault def f (a:as) (b:bs) = f a b   : zipWithDefault def f as bs

-- | Polynomial multiplication via the schoolbook (convolution) algorithm.
--
-- Complexity: \(O(n \cdot m)\) where \(n\) and \(m\) are the degrees.
mulPoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
mulPoly (Poly []) _ = zeroPoly
mulPoly _ (Poly []) = zeroPoly
mulPoly (Poly as) (Poly bs) = mkPoly $ foldl' addCoeffs (replicate rlen 0) terms
  where
    rlen = length as + length bs - 1
    terms = [ (i + j, a * b)
            | (i, a) <- zip [0..] as
            , (j, b) <- zip [0..] bs
            ]
    addCoeffs cs (idx, c) =
      case splitAt idx cs of
        (pre, v:post) -> pre ++ (v + c) : post
        _             -> cs

-- | Polynomial long division with remainder.
--
-- @divModPoly f g = (q, r)@ such that \(f = g \cdot q + r\) and
-- \(\deg(r) < \deg(g)\).
--
-- __Precondition:__ the leading coefficient of @g@ must be invertible
-- (i.e., the coefficient type must be a field).
--
-- Throws an error if @g@ is the zero polynomial.
divModPoly :: (Eq k, Fractional k) => Poly k -> Poly k -> (Poly k, Poly k)
divModPoly _ (Poly []) = error "divModPoly: division by zero polynomial"
divModPoly f g
  | degree f < degree g = (zeroPoly, f)
  | otherwise           = go zeroPoly f
  where
    dg = degree g
    lc = case leadCoeff g of
           Just c  -> c
           Nothing -> error "impossible"

    go q r
      | degree r < dg = (q, r)
      | otherwise =
          let dr  = degree r
              lr  = case leadCoeff r of Just lrc -> lrc; Nothing -> error "impossible"
              c   = lr / lc
              d   = dr - dg
              -- c * x^d
              term = Poly (replicate d 0 ++ [c])
              r'   = subPoly r (mulPoly term g)
          in go (addPoly q term) r'

-- | GCD of two polynomials via the Euclidean algorithm.
--
-- The result is always monic (leading coefficient 1) when the
-- coefficient type supports division.
--
-- >>> gcdPoly (mkPoly [1, 0, -1]) (mkPoly [1, -1]) :: Poly Rational
-- Poly [(-1) % 1,1 % 1]
gcdPoly :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
gcdPoly a (Poly []) = monicPoly a
gcdPoly a b         = gcdPoly b (snd $ divModPoly a b)

-- | Make a polynomial monic by dividing all coefficients by the leading
-- coefficient. Returns 'zeroPoly' for the zero polynomial.
monicPoly :: (Eq k, Fractional k) => Poly k -> Poly k
monicPoly (Poly []) = zeroPoly
monicPoly p@(Poly cs) =
  case leadCoeff p of
    Nothing -> zeroPoly
    Just lc -> Poly (map (/ lc) cs)

-- | Formal derivative of a polynomial.
--
-- \(\frac{d}{dx}(a_0 + a_1 x + a_2 x^2 + \cdots) = a_1 + 2 a_2 x + \cdots\)
--
-- >>> diffPoly (mkPoly [1, 2, 3] :: Poly Rational)
-- Poly [2 % 1,6 % 1]
diffPoly :: (Eq k, Num k) => Poly k -> Poly k
diffPoly (Poly [])     = zeroPoly
diffPoly (Poly (_:cs)) = mkPoly $ zipWith (\i c -> fromIntegral i * c) [1 :: Int ..] cs

-- | Compose two polynomials: @composePoly f g@ computes \(f(g(x))\).
--
-- >>> composePoly (mkPoly [1, 0, 1]) (mkPoly [0, 2]) :: Poly Rational
-- Poly [1 % 1,0 % 1,4 % 1]
composePoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
composePoly (Poly []) _ = zeroPoly
composePoly (Poly cs) g = foldr (\c acc -> addPoly (constPoly c) (mulPoly g acc)) zeroPoly cs

-- | Square-free factorisation via Yun's algorithm.
--
-- Returns a list of @(factor, multiplicity)@ pairs such that the original
-- polynomial equals the product \(\prod f_i^{m_i}\) (up to a scalar).
--
-- __Precondition:__ characteristic 0 (the algorithm uses 'diffPoly' and
-- 'gcdPoly', which require the derivative to detect repeated factors).
--
-- >>> squareFree (mkPoly [1, 2, 1] :: Poly Rational)
-- [(Poly [1 % 1,1 % 1],2)]
squareFree :: (Eq k, Fractional k) => Poly k -> [(Poly k, Int)]
squareFree (Poly []) = []
squareFree f =
  let f'  = diffPoly f
      c   = gcdPoly f f'
      w   = fst $ divModPoly f c
  in go w c 1
  where
    go w c i
      | degree w == 0 =
          if degree c > 0 then [(c, i)]  -- remaining factor
          else []
      | otherwise =
          let y  = gcdPoly w c
              z  = fst $ divModPoly w y
              c' = fst $ divModPoly c y
              rest = go y c' (i + 1)
          in if degree z > 0 then (z, i) : rest else rest
