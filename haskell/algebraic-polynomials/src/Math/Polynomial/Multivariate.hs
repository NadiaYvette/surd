-- | Sparse multivariate polynomials over an arbitrary coefficient ring.
--
-- Representation: @Map Mono k@ where each monomial maps to its
-- non-zero coefficient.  Monomials are products of variables
-- raised to positive integer powers.
module Math.Polynomial.Multivariate
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
  -- * Monomial operations
  , monoOne
  , monoVar
  , monoMul
  , monoDegree
  , monoGcd
  , monoLcm
  , monoDivides
  , monoDiv
  -- * Monomial comparison
  , compareGrevlex
  -- * Leading term operations
  , leadTermGrlex
  , leadTermGrevlex
  -- * GCD
  , gcdMPoly
  , contentMPoly
  , primPartMPoly
  , exactDivMPoly
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Math.Polynomial.Univariate as U

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

-- | GCD of two monomials (componentwise minimum of exponents).
monoGcd :: Mono -> Mono -> Mono
monoGcd (Mono a) (Mono b) =
  Mono (Map.filter (/= 0) (Map.intersectionWith min a b))

-- | LCM of two monomials (componentwise maximum of exponents).
monoLcm :: Mono -> Mono -> Mono
monoLcm (Mono a) (Mono b) =
  Mono (Map.unionWith max a b)

-- | Does the first monomial divide the second?
monoDivides :: Mono -> Mono -> Bool
monoDivides (Mono a) (Mono b) =
  Map.isSubmapOfBy (<=) a b

-- | Divide monomials: @monoDiv a b@ = @a / b@.
-- Precondition: @b@ divides @a@.
monoDiv :: Mono -> Mono -> Mono
monoDiv (Mono a) (Mono b) =
  Mono (Map.filter (/= 0) (Map.unionWith (-) a (Map.intersection b a)))

-- | Leading term under graded lex order (total degree first, then lex).
-- Returns @Nothing@ for the zero polynomial.
leadTermGrlex :: MPoly k -> Maybe (Mono, k)
leadTermGrlex (MPoly m)
  | Map.null m = Nothing
  | otherwise  = Just $ foldl1 pick (Map.toList m)
  where
    pick best@(bm, _) cur@(cm, _) =
      let bd = monoDegree bm
          cd = monoDegree cm
      in if cd > bd || (cd == bd && cm > bm) then cur else best

-- | Leading term under graded reverse lex order (grevlex).
-- Total degree first; among same degree, reverse lex on variables
-- (last variable is cheapest).
-- Returns @Nothing@ for the zero polynomial.
leadTermGrevlex :: MPoly k -> Maybe (Mono, k)
leadTermGrevlex (MPoly m)
  | Map.null m = Nothing
  | otherwise  = Just $ foldl1 pick (Map.toList m)
  where
    pick best@(bm, _) cur@(cm, _) =
      case compareGrevlex cm bm of
        GT -> cur
        _  -> best

-- | Compare two monomials in grevlex order.
compareGrevlex :: Mono -> Mono -> Ordering
compareGrevlex a b =
  case compare (monoDegree a) (monoDegree b) of
    LT -> LT
    GT -> GT
    EQ -> compareRevlex a b

-- | Reverse lex comparison: compare variables from highest to lowest;
-- the monomial with a SMALLER exponent on the highest differing variable wins.
compareRevlex :: Mono -> Mono -> Ordering
compareRevlex (Mono a) (Mono b) =
  let allVars = Map.keys (Map.union a b)
      -- Walk from highest variable down
      go [] = EQ
      go (v:vs) =
        let ea = Map.findWithDefault 0 v a
            eb = Map.findWithDefault 0 v b
        in case compare ea eb of
             EQ -> go vs
             -- In revlex, SMALLER exponent on highest variable is GREATER
             LT -> GT
             GT -> LT
  in go (reverse allVars)

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

-- ---------------------------------------------------------------------------
-- Multivariate GCD over Q
-- ---------------------------------------------------------------------------

-- | GCD of two multivariate polynomials over 'Rational'.
--
-- Uses the recursive dense algorithm: pick a variable, view both polynomials
-- as univariate in that variable with 'MPoly' coefficients, and compute the
-- GCD via a pseudo-remainder sequence.  The base case (no variables) reduces
-- to rational GCD.
--
-- The result is made primitive with positive leading coefficient.
gcdMPoly :: MPoly Rational -> MPoly Rational -> MPoly Rational
gcdMPoly a b
  | isZero a  = monicMPoly b
  | isZero b  = monicMPoly a
  | otherwise =
    case pickVar a b of
      Nothing -> constPoly 1  -- both constants; GCD over Q is 1 (up to units)
      Just v  ->
        let ca  = contentMPoly v a
            cb  = contentMPoly v b
            cg  = gcdMPoly ca cb
            pa  = primPartMPoly v a
            pb  = primPartMPoly v b
            ua  = toUnivariate v pa
            ub  = toUnivariate v pb
            ug  = pseudoGcdPoly ua ub
            g0  = fromUnivariateM v ug
            g1  = primPartMPoly v g0
            g2  = mulPoly cg g1
        in monicMPoly g2

-- | Pick a variable to recurse on.
pickVar :: MPoly k -> MPoly k -> Maybe Var
pickVar a b =
  let vs = Set.union (variables a) (variables b)
  in if Set.null vs then Nothing else Just (Set.findMax vs)

-- | Make an 'MPoly Rational' "monic": divide by the leading coefficient
-- (coefficient of the largest monomial under 'Ord Mono').
monicMPoly :: MPoly Rational -> MPoly Rational
monicMPoly (MPoly m)
  | Map.null m = zeroPoly
  | otherwise  =
    let lc = snd (Map.findMax m)
    in if lc == 0 then zeroPoly
       else clean (fmap (/ lc) m)

-- | GCD of all coefficients when viewed as univariate in the given variable.
-- The "coefficients" are multivariate polynomials in the remaining variables.
contentMPoly :: Var -> MPoly Rational -> MPoly Rational
contentMPoly v p =
  let U.Poly cs = toUnivariate v p
  in case filter (not . isZero) cs of
       []       -> zeroPoly
       [c]      -> monicMPoly c
       (c:rest) -> foldl gcdMPoly c rest

-- | Primitive part: @p / content(p)@ with respect to the given variable.
primPartMPoly :: Var -> MPoly Rational -> MPoly Rational
primPartMPoly v p =
  let c = contentMPoly v p
  in if isZero c then zeroPoly
     else exactDivMPoly p c

-- | Exact division of multivariate polynomials (assumes divisibility).
-- For a constant divisor, just divides every coefficient.
-- Otherwise, converts to univariate and performs exact polynomial division.
exactDivMPoly :: MPoly Rational -> MPoly Rational -> MPoly Rational
exactDivMPoly a b
  | isZero b  = error "exactDivMPoly: division by zero"
  | isConstMPoly b =
    let c = constCoeffQ b
    in fmap (/ c) a
  | otherwise =
    case pickVar a b of
      Nothing ->
        let ca = constCoeffQ a
            cb = constCoeffQ b
        in constPoly (ca / cb)
      Just v  ->
        let ua = toUnivariate v a
            ub = toUnivariate v b
        in fromUnivariateM v (exactDivUPoly ua ub)

-- | Is this a constant polynomial (including zero)?
isConstMPoly :: MPoly k -> Bool
isConstMPoly (MPoly m) =
  Map.null m || (Map.size m == 1 && all (Map.null . unMono) (Map.keys m))

-- | Extract the constant coefficient.
constCoeffQ :: Num k => MPoly k -> k
constCoeffQ (MPoly m) = fromMaybe 0 (Map.lookup monoOne m)

-- | Convert univariate poly with MPoly coefficients back to MPoly.
fromUnivariateM :: (Eq k, Num k) => Var -> U.Poly (MPoly k) -> MPoly k
fromUnivariateM v (U.Poly cs) =
  foldl addPoly zeroPoly
    [ mulPoly c (varPow v i)
    | (i, c) <- zip [0..] cs
    , not (isZero c)
    ]
  where
    varPow _ 0 = onePoly
    varPow w n = MPoly (Map.singleton (monoVar w n) 1)

-- ---------------------------------------------------------------------------
-- Pseudo-division and GCD for Poly (MPoly Rational)
-- ---------------------------------------------------------------------------

-- | Pseudo-remainder of @f@ by @g@.
pseudoRemPoly :: U.Poly (MPoly Rational)
              -> U.Poly (MPoly Rational)
              -> U.Poly (MPoly Rational)
pseudoRemPoly f g
  | U.degree f < U.degree g = f
  | U.degree g < 0          = error "pseudoRemPoly: division by zero"
  | otherwise                = go f (U.degree f - U.degree g + 1)
  where
    lcG = case U.leadCoeff g of Just c -> c; Nothing -> error "impossible"
    dg  = U.degree g

    go r 0 = r
    go r e
      | U.degree r < dg = r
      | otherwise =
        let lcR = case U.leadCoeff r of Just c -> c; Nothing -> error "impossible"
            d   = U.degree r - dg
            -- lcG * r - lcR * x^d * g
            r'  = U.subPoly (U.scalePoly lcG r)
                            (U.mulPoly (U.mkPoly (replicate d zeroPoly ++ [lcR])) g)
        in go r' (e - 1)

-- | GCD via pseudo-remainder sequence for Poly (MPoly Rational).
-- After each pseudo-remainder, make primitive to control coefficient growth.
pseudoGcdPoly :: U.Poly (MPoly Rational)
              -> U.Poly (MPoly Rational)
              -> U.Poly (MPoly Rational)
pseudoGcdPoly a b
  | U.degree a < 0 = b
  | U.degree b < 0 = a
  | U.degree a < U.degree b = pseudoGcdPoly b a
  | otherwise = go a b
  where
    go f g
      | U.degree g < 0 = f
      | otherwise =
        let r = pseudoRemPoly f g
        in if U.degree r < 0
           then g
           else go g (primPartUPoly r)

-- | Make a Poly (MPoly Rational) primitive: divide out the GCD of its coefficients.
primPartUPoly :: U.Poly (MPoly Rational) -> U.Poly (MPoly Rational)
primPartUPoly (U.Poly []) = U.zeroPoly
primPartUPoly (U.Poly cs) =
  let nonzero = filter (not . isZero) cs
  in case nonzero of
       [] -> U.zeroPoly
       (c0:rest) ->
         let g = foldl gcdMPoly c0 rest
         in if isZero g then U.Poly cs
            else U.mkPoly (map (`exactDivMPoly` g) cs)

-- | Exact polynomial division (no remainder) for Poly (MPoly Rational).
exactDivUPoly :: U.Poly (MPoly Rational)
              -> U.Poly (MPoly Rational)
              -> U.Poly (MPoly Rational)
exactDivUPoly f g
  | U.degree g < 0 = error "exactDivUPoly: division by zero"
  | U.degree f < U.degree g = U.zeroPoly
  | U.degree f < 0 = U.zeroPoly
  | otherwise = go f
  where
    dg  = U.degree g
    lcG = case U.leadCoeff g of Just c -> c; Nothing -> error "impossible"

    go r
      | U.degree r < 0  = U.zeroPoly
      | U.degree r < dg = U.zeroPoly
      | otherwise =
        let dr  = U.degree r
            lcR = case U.leadCoeff r of Just lc -> lc; Nothing -> error "impossible"
            d   = dr - dg
            c   = exactDivMPoly lcR lcG
            term = U.mkPoly (replicate d zeroPoly ++ [c])
            r'  = U.subPoly r (U.mulPoly term g)
        in U.addPoly term (go r')
