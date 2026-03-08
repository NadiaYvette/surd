-- | Dense univariate polynomial arithmetic over an arbitrary field.
--
-- Polynomials are represented as coefficient lists, low-degree first:
-- @[a0, a1, ..., an]@ means @a0 + a1*x + ... + an*x^n@.
-- Invariant: trailing zeros are stripped (the leading coefficient is nonzero),
-- except for the zero polynomial which is @[]@.
module Surd.Polynomial.Univariate
  ( Poly(..)
  , mkPoly
  , zeroPoly
  , constPoly
  , monoX
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
-- Stored as a list of coefficients, low-degree first.
newtype Poly k = Poly { unPoly :: [k] }
  deriving (Eq, Show, Functor)

-- | Smart constructor: strip trailing zeros.
mkPoly :: (Eq k, Num k) => [k] -> Poly k
mkPoly = Poly . stripZeros

stripZeros :: (Eq k, Num k) => [k] -> [k]
stripZeros = reverse . dropWhile (== 0) . reverse

zeroPoly :: Poly k
zeroPoly = Poly []

constPoly :: (Eq k, Num k) => k -> Poly k
constPoly 0 = zeroPoly
constPoly c = Poly [c]

-- | The polynomial @x@.
monoX :: Num k => Poly k
monoX = Poly [0, 1]

-- | Degree of the polynomial. Returns @-1@ for the zero polynomial.
degree :: Poly k -> Int
degree (Poly []) = -1
degree (Poly cs) = length cs - 1

-- | Leading coefficient. Returns @Nothing@ for the zero polynomial.
leadCoeff :: Poly k -> Maybe k
leadCoeff (Poly []) = Nothing
leadCoeff (Poly cs) = Just (last cs)

-- | Evaluate a polynomial at a point using Horner's method.
evalPoly :: Num k => Poly k -> k -> k
evalPoly (Poly []) _ = 0
evalPoly (Poly cs) x = foldr (\c acc -> c + x * acc) 0 cs

-- | Multiply by a scalar.
scalePoly :: (Eq k, Num k) => k -> Poly k -> Poly k
scalePoly 0 _ = zeroPoly
scalePoly s (Poly cs) = mkPoly (map (* s) cs)

addPoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
addPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault 0 (+) as bs)

subPoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
subPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault 0 (-) as bs)

zipWithDefault :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipWithDefault _   _ []     []     = []
zipWithDefault def f (a:as) []     = f a def : zipWithDefault def f as []
zipWithDefault def f []     (b:bs) = f def b : zipWithDefault def f [] bs
zipWithDefault def f (a:as) (b:bs) = f a b   : zipWithDefault def f as bs

-- | Polynomial multiplication via schoolbook algorithm.
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

-- | Polynomial division with remainder: @divModPoly f g = (q, r)@
-- such that @f = g*q + r@ and @degree r < degree g@.
--
-- Requires the leading coefficient of @g@ to be invertible (i.e., a field).
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

-- | GCD of two polynomials via the Euclidean algorithm, made monic.
gcdPoly :: (Eq k, Fractional k) => Poly k -> Poly k -> Poly k
gcdPoly a (Poly []) = monicPoly a
gcdPoly a b         = gcdPoly b (snd $ divModPoly a b)

-- | Make a polynomial monic (leading coefficient 1).
monicPoly :: (Eq k, Fractional k) => Poly k -> Poly k
monicPoly (Poly []) = zeroPoly
monicPoly p@(Poly cs) =
  case leadCoeff p of
    Nothing -> zeroPoly
    Just lc -> Poly (map (/ lc) cs)

-- | Formal derivative.
diffPoly :: (Eq k, Num k) => Poly k -> Poly k
diffPoly (Poly [])     = zeroPoly
diffPoly (Poly (_:cs)) = mkPoly $ zipWith (\i c -> fromIntegral i * c) [1 :: Int ..] cs

-- | Compose two polynomials: @composePoly f g@ = @f(g(x))@.
composePoly :: (Eq k, Num k) => Poly k -> Poly k -> Poly k
composePoly (Poly []) _ = zeroPoly
composePoly (Poly cs) g = foldr (\c acc -> addPoly (constPoly c) (mulPoly g acc)) zeroPoly cs

-- | Square-free factorisation: returns a list of (factor, multiplicity) pairs.
-- Uses Yun's algorithm. Requires characteristic 0.
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
