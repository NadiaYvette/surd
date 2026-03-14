-- | Dynamically-nested algebraic extension tower.
--
-- Unlike 'ExtElem' which uses Haskell's type system for tower nesting
-- (requiring static depth), 'TowerElem' uses a recursive data type
-- that supports towers of arbitrary runtime-determined depth.
--
-- This enables the Gauss period descent to build field towers
-- incrementally, one extension at a time, without knowing the
-- final depth at compile time.
--
-- Elements support full field arithmetic (Num, Fractional) and
-- can be converted to radical expressions for display.
module Surd.Field.DynTower
  ( TowerElem (..),
    TowerLevel (..),
    adjoinTowerRoot,
    towerToRadExpr,
    tIsZero,
    tLevel,
    promoteTo,
    levelId,
  )
where

import Surd.Types (RadExpr (..))

-- | Element of a dynamically-nested field tower.
--
-- @TRat r@ is an element of Q.
-- @TExt [a₀, a₁, ..., a_{d-1}] level@ represents a₀ + a₁·α + ... + a_{d-1}·α^{d-1}
-- where α is the generator of the extension described by @level@.
-- Coefficients are themselves 'TowerElem' values at the previous level.
data TowerElem
  = TRat !Rational
  | TExt ![TowerElem] !TowerLevel
  deriving (Show)

-- | One level of the field tower, describing a single simple radical
-- extension \(K(\alpha)\) where \(\alpha^n = r\) for some element
-- \(r\) of the base field \(K\).
data TowerLevel = TowerLevel
  { -- | Unique level identifier (assigned by caller)
    tlId :: !Int,
    -- | Extension degree (= @tlRootDeg@ for radical extensions)
    tlDegree :: !Int,
    -- | Root degree \(n\): the generator satisfies \(\alpha^n = r\)
    tlRootDeg :: !Int,
    -- | Radicand \(r\): the generator is \(\sqrt[n]{r}\)
    tlRadicand :: !TowerElem
  }
  deriving (Show)

instance Eq TowerLevel where
  a == b = tlId a == tlId b

-- ---------------------------------------------------------------------------
-- Zero checking and level inspection
-- ---------------------------------------------------------------------------

-- | Test whether a tower element is zero.
--
-- For rational elements, checks @r == 0@. For extension elements,
-- checks that all coefficients are recursively zero. This is needed
-- for coefficient trimming, equality testing, and zero-divisor checks
-- in the extended GCD inversion algorithm.
tIsZero :: TowerElem -> Bool
tIsZero (TRat r) = r == 0
tIsZero (TExt cs _) = all tIsZero cs

-- | Get the outermost extension level of a tower element, or
-- 'Nothing' for a rational element (which lives in the base field Q).
tLevel :: TowerElem -> Maybe TowerLevel
tLevel (TRat _) = Nothing
tLevel (TExt _ l) = Just l

-- ---------------------------------------------------------------------------
-- Eq instance
-- ---------------------------------------------------------------------------

instance Eq TowerElem where
  TRat a == TRat b = a == b
  TExt cs1 l1 == TExt cs2 l2
    | l1 == l2 = eqCoeffs cs1 cs2
    | otherwise = False
  TRat a == TExt cs _ = eqCoeffs [TRat a] cs
  TExt cs _ == TRat a = eqCoeffs cs [TRat a]

-- | Compare coefficient lists, treating missing trailing entries as zero.
eqCoeffs :: [TowerElem] -> [TowerElem] -> Bool
eqCoeffs [] [] = True
eqCoeffs [] bs = all tIsZero bs
eqCoeffs as [] = all tIsZero as
eqCoeffs (a : as) (b : bs) = a == b && eqCoeffs as bs

-- ---------------------------------------------------------------------------
-- Trimming
-- ---------------------------------------------------------------------------

-- | Remove trailing zero coefficients.
trimTE :: [TowerElem] -> [TowerElem]
trimTE = reverse . dropWhile tIsZero . reverse

-- ---------------------------------------------------------------------------
-- Num instance
-- ---------------------------------------------------------------------------

-- | Promote a tower element to a given level by embedding as the
-- constant coefficient in the higher extension.
--
-- If element @e@ lives in field @K@, then @promoteTo lvl e@ embeds it
-- as @e + 0*α + 0*α² + ...@ in @K(α)@ where @α@ is the generator
-- of @lvl@. Errors if the element already lives at a /higher/ level
-- than the target (demotion is not supported).
promoteTo :: TowerLevel -> TowerElem -> TowerElem
promoteTo lvl e@(TRat _) = TExt (e : replicate (tlDegree lvl - 1) (TRat 0)) lvl
promoteTo lvl e@(TExt _ l)
  | l == lvl = e
  | tlId l < tlId lvl = TExt (e : replicate (tlDegree lvl - 1) (TRat 0)) lvl
  | otherwise = error $ "promoteTo: cannot demote level " ++ show (tlId l) ++ " to " ++ show (tlId lvl)

-- | Get the outermost level ID, or @-1@ for 'TRat' (base field Q).
--
-- Level IDs are assigned by the caller of 'adjoinTowerRoot' and should
-- be globally unique within a tower construction. Higher IDs correspond
-- to later (outer) extensions.
levelId :: TowerElem -> Int
levelId (TRat _) = -1
levelId (TExt _ l) = tlId l

instance Num TowerElem where
  -- Addition
  TRat a + TRat b = TRat (a + b)
  TRat a + TExt bs l = TExt (addCoeffs [TRat a] bs) l
  TExt as l + TRat b = TExt (addCoeffs as [TRat b]) l
  TExt as l1 + TExt bs l2
    | l1 == l2 = mkTExt (addCoeffs as bs) l1
    | tlId l1 < tlId l2 = promoteTo l2 (TExt as l1) + TExt bs l2
    | otherwise = TExt as l1 + promoteTo l1 (TExt bs l2)

  -- Multiplication
  TRat a * TRat b = TRat (a * b)
  TRat a * TExt bs l = mkTExt (map (TRat a *) bs) l
  TExt as l * TRat b = mkTExt (map (* TRat b) as) l
  TExt as l1 * TExt bs l2
    | l1 == l2 = mkTExt (reduceCoeffs l1 (polyMulTE as bs)) l1
    | tlId l1 < tlId l2 = promoteTo l2 (TExt as l1) * TExt bs l2
    | otherwise = TExt as l1 * promoteTo l1 (TExt bs l2)

  negate (TRat a) = TRat (negate a)
  negate (TExt cs l) = TExt (map negate cs) l

  abs = error "TowerElem: abs not meaningful"
  signum = error "TowerElem: signum not meaningful"
  fromInteger n = TRat (fromInteger n)

-- ---------------------------------------------------------------------------
-- Fractional instance
-- ---------------------------------------------------------------------------

instance Fractional TowerElem where
  recip (TRat a) = TRat (recip a)
  recip (TExt cs l) = tInv cs l

  fromRational = TRat

-- | Smart constructor: if all coefficients are zero, produce TRat 0.
mkTExt :: [TowerElem] -> TowerLevel -> TowerElem
mkTExt cs l =
  let cs' = trimTE cs
   in case cs' of
        [] -> TRat 0
        [c] | isBaseLevel c -> c
        _ -> TExt (padTo (tlDegree l) cs') l
  where
    isBaseLevel (TRat _) = True
    isBaseLevel _ = False

-- | Pad a coefficient list to exactly n elements with zeros.
padTo :: Int -> [TowerElem] -> [TowerElem]
padTo n cs
  | length cs >= n = take n cs
  | otherwise = cs ++ replicate (n - length cs) (TRat 0)

-- ---------------------------------------------------------------------------
-- Coefficient-list arithmetic
-- ---------------------------------------------------------------------------

addCoeffs :: [TowerElem] -> [TowerElem] -> [TowerElem]
addCoeffs [] bs = bs
addCoeffs as [] = as
addCoeffs (a : as) (b : bs) = (a + b) : addCoeffs as bs

-- | Schoolbook polynomial multiplication on coefficient lists.
polyMulTE :: [TowerElem] -> [TowerElem] -> [TowerElem]
polyMulTE [] _ = []
polyMulTE _ [] = []
polyMulTE as bs =
  let rlen = length as + length bs - 1
      zeros = replicate rlen (TRat 0)
      terms =
        [ (i + j, a * b)
          | (i, a) <- zip [0 :: Int ..] as,
            (j, b) <- zip [0 :: Int ..] bs
        ]
   in foldl (\acc (idx, c) -> addAt idx c acc) zeros terms
  where
    addAt _ _ [] = []
    addAt 0 c (x : xs) = (x + c) : xs
    addAt i c (x : xs) = x : addAt (i - 1) c xs

-- | Reduce a coefficient list modulo α^n = r.
-- For a binomial minimal polynomial x^n - r, reduction is simple:
-- replace α^n with r, α^{n+1} with r·α, etc.
reduceCoeffs :: TowerLevel -> [TowerElem] -> [TowerElem]
reduceCoeffs l cs
  | length cs <= n = cs
  | otherwise =
      let (lo, hi) = splitAt n cs
          -- α^{n+k} = r · α^k
          shifted = map (* tlRadicand l) hi
          combined = addCoeffs lo shifted
       in reduceCoeffs l combined
  where
    n = tlDegree l

-- ---------------------------------------------------------------------------
-- Inversion via extended GCD
-- ---------------------------------------------------------------------------

-- | Invert an element in an extension field.
-- Uses extended GCD: find s such that s·elem ≡ 1 (mod minpoly).
tInv :: [TowerElem] -> TowerLevel -> TowerElem
tInv cs l
  | all tIsZero cs = error "TowerElem: division by zero"
  | otherwise =
      let minP = minPolyCoeffs l
          -- extGcd gives (g, s, t) with g = s·cs + t·minP
          (_g, s, _t) = polyExtGcd (trimTE cs) minP
          -- s is the inverse, reduce mod minpoly
          reduced = reduceCoeffs l s
       in -- Make monic: divide by leading coefficient of g
          mkTExt reduced l

-- | Build the minimal polynomial coefficient list for a tower level.
-- For α^n = r, the minimal polynomial is x^n - r = [-r, 0, ..., 0, 1].
minPolyCoeffs :: TowerLevel -> [TowerElem]
minPolyCoeffs l =
  negate (tlRadicand l) : replicate (tlDegree l - 1) (TRat 0) ++ [TRat 1]

-- | Extended GCD for coefficient lists (polynomials over TowerElem).
-- Returns (g, s, t) such that g = s·a + t·b.
polyExtGcd :: [TowerElem] -> [TowerElem] -> ([TowerElem], [TowerElem], [TowerElem])
polyExtGcd a b = go a b [TRat 1] [] [] [TRat 1]
  where
    go r0 r1 s0 s1 t0 t1
      | all tIsZero r1 =
          -- Make monic
          let lc = lastNonZero r0
              r0' = map (/ lc) r0
              s0' = map (/ lc) s0
              t0' = map (/ lc) t0
           in (trimTE r0', trimTE s0', trimTE t0')
      | otherwise =
          let (q, r) = polyDivMod r0 r1
              s2 = polySub s0 (polyMulTE q s1)
              t2 = polySub t0 (polyMulTE q t1)
           in go r1 r s1 s2 t1 t2

    lastNonZero xs = case trimTE xs of
      [] -> TRat 1
      ts -> last ts

-- | Polynomial division with remainder on coefficient lists.
polyDivMod :: [TowerElem] -> [TowerElem] -> ([TowerElem], [TowerElem])
polyDivMod _ [] = error "polyDivMod: division by zero"
polyDivMod f g
  | degF < degG = ([], f)
  | otherwise = go [] f
  where
    f' = trimTE f
    g' = trimTE g
    degF = length f' - 1
    degG = length g' - 1
    lcG = last g'

    go q r =
      let r' = trimTE r
          degR = length r' - 1
       in if degR < degG || all tIsZero r'
            then (trimTE q, r')
            else
              let lcR = last r'
                  c = lcR / lcG
                  d = degR - degG
                  -- c * x^d
                  term = replicate d (TRat 0) ++ [c]
                  r'' = polySub r' (polyMulTE term g')
               in go (addCoeffs q term) r''

-- | Polynomial subtraction on coefficient lists.
polySub :: [TowerElem] -> [TowerElem] -> [TowerElem]
polySub [] bs = map negate bs
polySub as [] = as
polySub (a : as) (b : bs) = (a - b) : polySub as bs

-- ---------------------------------------------------------------------------
-- Tower construction
-- ---------------------------------------------------------------------------

-- | Adjoin an \(n\)th root to the tower: create a new extension level
-- where the generator \(\alpha\) satisfies \(\alpha^n = r\).
--
-- Returns the new 'TowerLevel' and the generator element \(\alpha\),
-- represented as @[0, 1, 0, ..., 0]@ (the polynomial \(\alpha\)) in
-- the new extension. The level ID must be globally unique within the
-- tower being constructed (the caller is responsible for this).
adjoinTowerRoot ::
  -- | Level ID (unique)
  Int ->
  -- | Root degree n
  Int ->
  -- | Radicand r (element of current field)
  TowerElem ->
  (TowerLevel, TowerElem)
adjoinTowerRoot lvlId n r =
  let level =
        TowerLevel
          { tlId = lvlId,
            tlDegree = n,
            tlRootDeg = n,
            tlRadicand = r
          }
      -- The generator α, represented as [0, 1, 0, ..., 0]
      gen = TExt (TRat 0 : TRat 1 : replicate (n - 2) (TRat 0)) level
   in (level, gen)

-- ---------------------------------------------------------------------------
-- Conversion to RadExpr
-- ---------------------------------------------------------------------------

-- | Convert a tower element to a radical expression ('RadExpr').
--
-- Recursively expands each extension level's generator \(\alpha\) as
-- @'Root' n (towerToRadExpr r)@ where \(\alpha^n = r\), and
-- reconstructs the polynomial expression
-- \(c_0 + c_1 \alpha + \cdots + c_{d-1} \alpha^{d-1}\).
--
-- Warning: the resulting expression may be exponentially large for deep
-- towers, since each level's generator is expanded independently. For
-- display purposes, prefer the structured rendering in
-- "Surd.Field.DynTower.Display".
towerToRadExpr :: TowerElem -> RadExpr Rational
towerToRadExpr (TRat r) = Lit r
towerToRadExpr (TExt cs level) =
  let gen = Root (tlRootDeg level) (towerToRadExpr (tlRadicand level))
      terms =
        [ case i of
            0 -> towerToRadExpr c
            1 -> Mul (towerToRadExpr c) gen
            _ -> Mul (towerToRadExpr c) (Pow gen i)
          | (c, i) <- zip cs [0 :: Int ..],
            not (tIsZero c)
        ]
   in case terms of
        [] -> Lit 0
        [t] -> t
        _ -> foldl1 Add terms
