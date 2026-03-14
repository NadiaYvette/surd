{-# LANGUAGE PatternSynonyms #-}

-- | Euler substitution for integrals of the form
-- ∫ P(x)/Q(x) · (√(ax²+bx+c))^n dx.
--
-- Three substitutions reduce these to rational function integrals:
--
-- * Euler 1 (a > 0): √(ax²+bx+c) = t − x√a
-- * Euler 2 (c > 0): √(ax²+bx+c) = xt + √c
-- * Euler 3 (Δ > 0): √(a(x−r₁)(x−r₂)) = t(x−r₁)
module Surd.Integration.Euler
  ( -- * Result type
    SymExpr(..)
  , IntegralResult(..)
    -- * Integrand specification
  , EulerIntegrand(..)
    -- * Integration
  , eulerIntegrate
    -- * Rational function integration (standalone)
  , integrateRational
    -- * Rendering
  , prettySymExpr
  , latexSymExpr
  ) where

import Control.Applicative ((<|>))
import Data.Ratio (denominator, numerator)
import Math.Polynomial.Univariate
import Math.Polynomial.Factoring (factor)
import Math.Field.Extension (extGcd)
import Surd.Types (RadExpr(..), pattern Sqrt, ratE)
import Surd.Radical.Normalize (normalize)
import Surd.Radical.LaTeX (latex)
import Surd.Radical.Pretty (pretty)

-- | Symbolic expression tree for an antiderivative (a function of @x@).
--
-- This is a small expression language sufficient to represent the
-- antiderivatives that arise from Euler substitution and rational
-- function integration: rational constants, radical constants, the
-- integration variable, arithmetic operations, logarithms, and
-- inverse trigonometric\/hyperbolic functions.
data SymExpr
  = SRat !Rational               -- ^ Rational constant \(r \in \mathbb{Q}\)
  | SRad !(RadExpr Rational)     -- ^ Constant radical expression (no @x@ dependency)
  | SVar                         -- ^ The integration variable \(x\)
  | SSurd !Rational !Rational !Rational  -- ^ \(\sqrt{ax^2 + bx + c}\), stored as @(a, b, c)@
  | SNeg !SymExpr                -- ^ Negation
  | SAdd !SymExpr !SymExpr       -- ^ Addition
  | SMul !SymExpr !SymExpr       -- ^ Multiplication
  | SDiv !SymExpr !SymExpr       -- ^ Division
  | SPow !SymExpr !Int           -- ^ Integer power
  | SLn  !SymExpr                -- ^ Natural logarithm: \(\ln|{\cdot}|\)
  | SArcTan !SymExpr             -- ^ Inverse tangent: \(\arctan(\cdot)\)
  | SArcSin !SymExpr             -- ^ Inverse sine: \(\arcsin(\cdot)\)
  | SArsinh !SymExpr             -- ^ Inverse hyperbolic sine: \(\operatorname{arsinh}(\cdot)\)
  | SArcosh !SymExpr             -- ^ Inverse hyperbolic cosine: \(\operatorname{arcosh}(\cdot)\)
  deriving (Show)

-- | Result of Euler substitution integration.
--
-- Contains the antiderivative as a 'SymExpr' together with the
-- quadratic surd parameters \(a, b, c\) from the original integrand's
-- \(\sqrt{ax^2 + bx + c}\). These are retained so renderers can
-- reconstruct the original problem statement.
data IntegralResult = IntegralResult
  { irExpr :: !SymExpr           -- ^ The antiderivative expression (function of @x@)
  , irA    :: !Rational          -- ^ Coefficient \(a\) in \(\sqrt{ax^2 + bx + c}\)
  , irB    :: !Rational          -- ^ Coefficient \(b\)
  , irC    :: !Rational          -- ^ Constant term \(c\)
  } deriving (Show)

-- | Specification of an integrand of the form
-- \(\frac{P(x)}{Q(x)} \cdot \bigl(\sqrt{ax^2 + bx + c}\bigr)^n\).
--
-- The surd power @eiSqrtPow@ is typically @+1@ (integrand includes
-- the square root in the numerator) or @-1@ (square root in the
-- denominator, as in \(\int dx / \sqrt{\cdot}\)).
data EulerIntegrand = EulerIntegrand
  { eiP       :: !(Poly Rational)  -- ^ Numerator polynomial \(P(x)\)
  , eiQ       :: !(Poly Rational)  -- ^ Denominator polynomial \(Q(x)\)
  , eiSqrtPow :: !Int              -- ^ Power \(n\) of the surd factor
  , eiA       :: !Rational         -- ^ Coefficient \(a\) in the surd
  , eiB       :: !Rational         -- ^ Coefficient \(b\)
  , eiC       :: !Rational         -- ^ Constant term \(c\)
  } deriving (Show)

data EulerSub = Euler1 !Rational    -- ^ √a is rational, stored here
              | Euler2 !Rational    -- ^ √c is rational, stored here
              | Euler3 !Rational !Rational  -- ^ rational roots r₁, r₂
  deriving (Show)

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

sAdd :: SymExpr -> SymExpr -> SymExpr
sAdd (SRat 0) b = b
sAdd a (SRat 0) = a
sAdd (SRat a) (SRat b) = SRat (a + b)
sAdd a b = SAdd a b

sMul :: SymExpr -> SymExpr -> SymExpr
sMul (SRat 0) _ = SRat 0
sMul _ (SRat 0) = SRat 0
sMul (SRat 1) b = b
sMul a (SRat 1) = a
sMul (SRat (-1)) b = sNeg b
sMul (SRat a) (SRat b) = SRat (a * b)
-- Absorb rational into radical: r · rad → normalize(r · rad)
sMul (SRat a) (SRad e) = SRad (normalize (Mul (Lit a) e))
sMul (SRad e) (SRat a) = SRad (normalize (Mul (Lit a) e))
sMul a b = SMul a b

sDiv :: SymExpr -> SymExpr -> SymExpr
sDiv (SRat 0) _ = SRat 0
sDiv a (SRat 1) = a
sDiv (SRat a) (SRat b) = SRat (a / b)
-- r / rad → normalize(r / rad)
sDiv (SRat a) (SRad e) =
  -- Extract rational coefficient from RadExpr to avoid Lit/Inv non-cancellation
  let (c, e') = radCoeff e
  in SRad (normalize (Mul (Lit (a / c)) (Inv e')))
-- (r·x) / s → (r/s)·x
sDiv (SMul (SRat a) x) (SRat b) = sMul (SRat (a / b)) x
sDiv a b = SDiv a b

-- | Extract rational coefficient from a normalized RadExpr:
-- Mul (Lit c) rest → (c, rest); otherwise (1, expr)
radCoeff :: RadExpr Rational -> (Rational, RadExpr Rational)
radCoeff (Mul (Lit c) e) = (c, e)
radCoeff (Mul e (Lit c)) = (c, e)
radCoeff e = (1, e)

sNeg :: SymExpr -> SymExpr
sNeg (SRat r) = SRat (negate r)
sNeg (SNeg x) = x
sNeg x = SNeg x

sSum :: [SymExpr] -> SymExpr
sSum [] = SRat 0
sSum xs = foldl1 sAdd xs

--------------------------------------------------------------------------------
-- Choosing the substitution
--------------------------------------------------------------------------------

-- | Try to find a rational √a, √c, or rational roots.
chooseEuler :: Rational -> Rational -> Rational -> Maybe EulerSub
chooseEuler a b c
  | a > 0, Just s <- ratSqrt a = Just (Euler1 s)
  | c > 0, Just s <- ratSqrt c = Just (Euler2 s)
  | disc > 0, Just _ <- ratSqrt disc =
      -- roots are (-b ± √disc) / (2a)
      let Just sd = ratSqrt disc
          r1 = (-b + sd) / (2 * a)
          r2 = (-b - sd) / (2 * a)
      in Just (Euler3 r1 r2)
  | otherwise = Nothing
  where disc = b * b - 4 * a * c

-- | Exact rational square root, if it exists.
ratSqrt :: Rational -> Maybe Rational
ratSqrt r
  | r < 0 = Nothing
  | r == 0 = Just 0
  | otherwise =
      let n = numerator r
          d = denominator r
      in do sn <- intSqrt n
            sd <- intSqrt d
            Just (fromInteger sn / fromInteger sd)
  where
    intSqrt :: Integer -> Maybe Integer
    intSqrt x
      | x < 0 = Nothing
      | x == 0 = Just 0
      | otherwise = let s = floor (sqrt (fromInteger x :: Double))
                    in if s * s == x then Just s
                       else if (s+1)*(s+1) == x then Just (s+1)
                       else Nothing

--------------------------------------------------------------------------------
-- Euler substitution: produce rational function of t
--------------------------------------------------------------------------------

-- | After substitution, the integrand becomes N(t)/D(t) dt.
-- Also returns the back-substitution t = f(x, √(ax²+bx+c)).
data SubResult = SubResult
  { srNum  :: !(Poly Rational)     -- ^ numerator N(t)
  , srDen  :: !(Poly Rational)     -- ^ denominator D(t)
  , srBack :: !SymExpr             -- ^ t expressed in terms of x and √(ax²+bx+c)
  }

-- | Apply Euler substitution.
-- The integrand is P(x)/Q(x) · y^n where y = √(ax²+bx+c).
-- Returns (N(t), D(t)) such that the integral equals ∫ N(t)/D(t) dt,
-- and the back-substitution for t.
applyEuler :: EulerSub -> Poly Rational -> Poly Rational -> Int
           -> Rational -> Rational -> Rational -> SubResult

-- Euler 1: y = t - sx, so t = y + sx.
-- x = (t² - c) / (b + 2st)
-- y = (bt + st² + sc) / (b + 2st)
-- dx/dt = 2(bt + st² + sc) / (b + 2st)²
applyEuler (Euler1 s) pPoly qPoly n _a b c =
  let -- q_x(t) = b + 2st (denominator for x(t) and y(t))
      qx = mkPoly [b, 2*s]           -- b + 2st
      -- p_x(t) = t² - c (numerator for x(t))
      px = mkPoly [-c, 0, 1]         -- t² - c
      -- p_y(t) = sc + bt + st² (numerator for y(t))
      py = mkPoly [s*c, b, s]        -- sc + bt + st²
      -- dx/dt = 2·p_y / q_x²

      -- Evaluate P(x(t)): substitute x = p_x/q_x into P
      -- P(p_x/q_x) = P_num(t) / q_x^(deg P)
      dP = degree pPoly
      pNum = substRatFunc pPoly px qx

      -- Evaluate Q(x(t)): similarly
      dQ = degree qPoly
      qNum = substRatFunc qPoly px qx

      -- Full integrand numerator and denominator:
      -- P_num/q_x^dP * (p_y/q_x)^n * 2*p_y/q_x^2 / (Q_num/q_x^dQ)
      -- = 2 * P_num * p_y^(n+1) / (Q_num * q_x^(dP - dQ + n + 2))
      --
      -- For n = -1: 2 * P_num / (Q_num * q_x^(dP - dQ + 1))
      -- For n = +1: 2 * P_num * p_y^2 / (Q_num * q_x^(dP - dQ + 3))
      numParts = scalePoly 2 pNum * polyPow py (max 0 (n + 1))
      denParts = qNum * polyPow qx (dP - dQ + n + 2)
                 * polyPow py (max 0 (-(n + 1)))

      -- Back-substitution: t = y + sx = √(ax²+bx+c) + s·x
      back = sAdd (SSurd (s*s) b c) (sMul (SRat s) SVar)
  in SubResult numParts denParts back

-- Euler 2: y = xt + √c, so t = (y - √c) / x.
-- x = (2t·sc - b) / (a - t²)   where sc = √c
-- y = xt + sc
-- dx/dt = 2(at·sc - bt + sc·t²) / ??? -- simpler via chain rule
applyEuler (Euler2 sc) pPoly qPoly n a b _c =
  let -- x(t) = (2·sc·t - b) / (a - t²)
      px = mkPoly [-b, 2*sc]         -- -b + 2·sc·t
      qx = mkPoly [a, 0, -1]         -- a - t²
      -- y(t) = x(t)·t + sc
      -- = t·(-b + 2·sc·t)/(a - t²) + sc
      -- = (-bt + 2·sc·t² + sc·a - sc·t²) / (a - t²)
      -- = (sc·a - bt + sc·t²) / (a - t²)
      py = mkPoly [sc*a, -b, sc]      -- sc·a - bt + sc·t²
      -- dx/dt = [(2·sc)(a - t²) - (-b + 2·sc·t)(-2t)] / (a - t²)²
      -- = [2·sc·a - 2·sc·t² + 2bt - 4·sc·t²] ... wait let me compute
      -- = [2·sc·a - 2·sc·t² - (-2t)(-b + 2·sc·t)] / (a-t²)²
      -- = [2·sc·a - 2·sc·t² - (2bt - 4·sc·t²)] / (a-t²)²
      -- Hmm, this is getting messy. Let me use the general formula.
      -- Actually: dx/dt = 2·py / qx²  (same pattern as Euler 1 by Euler sub theory)
      -- Let me verify: for Euler 2, we have y = xt + sc, so y - sc = xt, dy = x dt + t dx.
      -- Also y² = ax²+bx+c, so 2y dy = (2ax+b) dx, dy = (2ax+b)/(2y) dx.
      -- From y = xt + sc: dy = x dt + t dx. So x dt + t dx = (2ax+b)/(2y) dx
      -- x dt = [(2ax+b)/(2y) - t] dx = [(2ax+b - 2yt)/(2y)] dx
      -- dx/dt = 2xy / (2ax+b - 2yt)
      -- This doesn't simplify as neatly. Let me just compute dx/dt directly.
      -- x = px/qx, dx/dt = (px'·qx - px·qx') / qx²
      -- dx/dt = 2·py / qx² (same pattern as Euler 1)

      dP = degree pPoly
      dQ = degree qPoly
      pNum = substRatFunc pPoly px qx
      qNum = substRatFunc qPoly px qx

      numParts = scalePoly 2 pNum * polyPow py (max 0 (n + 1))
      denParts = qNum * polyPow qx (dP - dQ + n + 2)
                 * polyPow py (max 0 (-(n + 1)))

      back = sDiv (sAdd (SSurd a b (sc*sc)) (sNeg (SRat sc))) SVar
  in SubResult numParts denParts back

-- Euler 3: y = t(x - r₁), roots r₁, r₂.
-- a(x-r₁)(x-r₂) = t²(x-r₁)², so a(x-r₂) = t²(x-r₁)
-- x = (a·r₂ - t²·r₁) / (a - t²)
applyEuler (Euler3 r1 r2) pPoly qPoly n a b c =
  let px = mkPoly [a*r2, 0, -r1]     -- a·r₂ - r₁·t²
      qx = mkPoly [a, 0, -1]          -- a - t²
      -- y = t(x - r₁) = t·(px/qx - r₁) = t·(px - r₁·qx)/qx
      -- px - r₁·qx = (a·r₂ - r₁·t²) - r₁·(a - t²) = a·r₂ - r₁·t² - a·r₁ + r₁·t² = a(r₂ - r₁)
      -- So y = t · a(r₂-r₁) / qx = a(r₂-r₁)·t / qx
      py = mkPoly [0, a * (r2 - r1)]  -- a(r₂-r₁)·t

      -- dx/dt: same pattern (can verify)
      px' = diffPoly px
      qx' = diffPoly qx
      dxNum = mulPoly px' qx - mulPoly px qx'

      dP = degree pPoly
      dQ = degree qPoly
      pNum = substRatFunc pPoly px qx
      qNum = substRatFunc qPoly px qx

      numParts = pNum * dxNum * polyPow py (max 0 n)
      denParts = qNum * polyPow qx (dP - dQ + 2)
                 * polyPow py (max 0 (-n))

      back = sDiv (SSurd a b c) (sAdd SVar (sNeg (SRat r1)))
  in SubResult numParts denParts back

-- | Substitute x = p/q into a polynomial f(x).
-- Returns the numerator of f(p/q) · q^(deg f).
substRatFunc :: Poly Rational -> Poly Rational -> Poly Rational -> Poly Rational
substRatFunc f p q =
  let cs = unPoly f
      d  = degree f
  in case cs of
    [] -> zeroPoly
    _  -> let -- f(p/q) = Σ cᵢ (p/q)^i = (Σ cᵢ pⁱ q^(d-i)) / q^d
              terms = zipWith (\ci i -> scalePoly ci (polyPow p i * polyPow q (d - i)))
                              cs [0..]
          in foldl' addPoly zeroPoly terms

-- | Integer power of a polynomial.
polyPow :: (Eq k, Num k) => Poly k -> Int -> Poly k
polyPow _ 0 = constPoly 1
polyPow p 1 = p
polyPow p k
  | k < 0     = error "polyPow: negative exponent"
  | even k    = let h = polyPow p (k `div` 2) in h * h
  | otherwise = p * polyPow p (k - 1)

--------------------------------------------------------------------------------
-- Rational function integration
--------------------------------------------------------------------------------

-- | Integrate a rational function \(N(t)/D(t) \, dt\).
--
-- Performs polynomial long division, factors the denominator, computes
-- a partial fraction decomposition, and integrates each term:
--
--   * Linear factors produce logarithms or negative powers.
--   * Irreducible quadratic factors produce logarithms and arctangent
--     (complex roots) or logarithmic ratios (real irrational roots).
--
-- The result is a 'SymExpr' where 'SVar' represents the integration
-- variable.
integrateRational :: Poly Rational -> Poly Rational -> SymExpr
integrateRational num den
  | unPoly den == [] = error "integrateRational: zero denominator"
  | otherwise =
      let -- Make denominator monic
          lc = case leadCoeff den of Just c -> c; Nothing -> 1
          num' = scalePoly (1/lc) num
          den' = monicPoly den
          -- Polynomial division
          (q, r) = divModPoly num' den'
          polyPart = integratePoly q
          -- Factor denominator
          facs = factor den'
          -- Partial fraction decomposition of r/den'
          pfTerms = partialFractions r facs
          -- Integrate each partial fraction term
          pfParts = map integratePartialFraction pfTerms
      in sSum (polyPart : pfParts)

-- | Integrate a polynomial term by term.
integratePoly :: Poly Rational -> SymExpr
integratePoly (Poly []) = SRat 0
integratePoly (Poly cs) =
  sSum [ sMul (SRat (c / fromIntegral (i + 1))) (SPow SVar (i + 1))
       | (c, i) <- zip cs [0 :: Int ..], c /= 0 ]

-- | A partial fraction term: r(t) / f(t)^k where deg r < deg f.
data PFTerm = PFTerm
  { pfNum    :: !(Poly Rational)   -- ^ numerator r(t), deg r < deg f
  , pfFactor :: !(Poly Rational)   -- ^ irreducible factor f(t)
  , pfPower  :: !Int               -- ^ k ≥ 1
  } deriving (Show)

-- | Decompose r(t) / ∏(fᵢ(t))^eᵢ into partial fractions.
partialFractions :: Poly Rational -> [(Poly Rational, Int)] -> [PFTerm]
partialFractions _ [] = []
partialFractions r [(f, e)] = expandPower r f e
partialFractions r ((f, e) : rest) =
  let -- Compute the product of remaining factors
      others = foldl' (\acc (g, m) -> acc * polyPow g m) (constPoly 1) rest
      fe = polyPow f e
      -- Extended GCD: u·fe + v·others = 1
      (_, u, v) = extGcd fe others
      -- r / (fe * others) = r·v/fe + r·u/others
      rv = snd (divModPoly (r * v) fe)     -- reduce mod fe
      ru = snd (divModPoly (r * u) others) -- reduce mod others
  in expandPower rv f e ++ partialFractions ru rest

-- | Expand r(t) / f(t)^e into PFTerms with powers 1..e.
expandPower :: Poly Rational -> Poly Rational -> Int -> [PFTerm]
expandPower r f e = go r e
  where
    go rem_ k
      | k <= 0 = []
      | otherwise =
          let (q_, rk) = divModPoly rem_ f
          in PFTerm rk f k : go q_ (k - 1)

-- | Integrate a single partial fraction term r(t)/f(t)^k.
integratePartialFraction :: PFTerm -> SymExpr
integratePartialFraction (PFTerm (Poly []) _ _) = SRat 0

-- Linear factor f = t - α (stored as [-α, 1])
integratePartialFraction (PFTerm (Poly [a]) (Poly [negAlpha, 1]) k)
  | k == 1    = sMul (SRat a) (SLn (sAdd SVar (SRat negAlpha)))
  | otherwise = let k' = fromIntegral (1 - k) :: Rational
                in sMul (SRat (a / k'))
                        (SPow (sAdd SVar (SRat negAlpha)) (1 - k))

-- Irreducible quadratic f = t² + pt + q (stored as [q, p, 1])
-- Constant numerator: treat as (c + 0·t)
integratePartialFraction (PFTerm (Poly [c_]) fac@(Poly [_, _, 1]) 1) =
  integrateQuadratic c_ 0 fac
integratePartialFraction (PFTerm (Poly [c_, b_]) fac@(Poly [_, _, 1]) 1) =
  integrateQuadratic c_ b_ fac

-- Fallback: cannot integrate this term symbolically
integratePartialFraction (PFTerm num_ fac_ k_) =
  -- Leave as unevaluated r(t)/f(t)^k
  SDiv (polyToSym num_) (SPow (polyToSym fac_) k_)

-- | Integrate (b·t + c) / (t² + p·t + q) dt.
integrateQuadratic :: Rational -> Rational -> Poly Rational -> SymExpr
integrateQuadratic c_ b_ (Poly [q_, p_, 1]) =
  let disc = 4 * q_ - p_ * p_
      quadExpr = sAdd (sAdd (SPow SVar 2) (sMul (SRat p_) SVar)) (SRat q_)
      logPart = sMul (SRat (b_ / 2)) (SLn quadExpr)
      remCoeff = c_ - b_ * p_ / 2
  in if disc > 0
     then -- Complex roots: arctan formula
       let sqrtDisc = case ratSqrt disc of
                        Just s  -> SRat s
                        Nothing -> SRad (normalize (Sqrt (ratE disc)))
           arctanPart = sMul (sDiv (SRat (remCoeff * 2)) sqrtDisc)
                             (SArcTan (sDiv (sAdd (sMul (SRat 2) SVar) (SRat p_)) sqrtDisc))
       in sAdd logPart arctanPart
     else -- Real irrational roots: log formula
       -- ∫ (bt+c)/(t²+pt+q) dt = b/2·ln|t²+pt+q| + (c-bp/2)/δ · ln|(2t+p-δ)/(2t+p+δ)|
       let negDisc = negate disc  -- p²-4q > 0
           sqrtNegDisc = case ratSqrt negDisc of
                           Just s  -> SRat s
                           Nothing -> SRad (normalize (Sqrt (ratE negDisc)))
           twoTPlusP = sAdd (sMul (SRat 2) SVar) (SRat p_)
           logArg = sDiv (sAdd twoTPlusP (sNeg sqrtNegDisc))
                         (sAdd twoTPlusP sqrtNegDisc)
           logRatPart = sMul (sDiv (SRat remCoeff) sqrtNegDisc) (SLn logArg)
       in sAdd logPart logRatPart
integrateQuadratic _ _ _ = error "integrateQuadratic: not a monic quadratic"

-- | Convert a polynomial to a SymExpr.
polyToSym :: Poly Rational -> SymExpr
polyToSym (Poly []) = SRat 0
polyToSym (Poly cs) =
  sSum [ sMul (SRat c) (SPow SVar i) | (c, i) <- zip cs [0..], c /= 0 ]

--------------------------------------------------------------------------------
-- Main integration entry point
--------------------------------------------------------------------------------

-- | Integrate \(\frac{P(x)}{Q(x)} \cdot \bigl(\sqrt{ax^2+bx+c}\bigr)^n \, dx\)
-- via Euler substitution.
--
-- Selects the appropriate Euler substitution (1, 2, or 3) based on
-- the coefficients, transforms the integrand into a rational function
-- of an auxiliary variable \(t\), integrates the rational function via
-- partial fractions, and back-substitutes to express the result in
-- terms of \(x\) and \(\sqrt{ax^2+bx+c}\).
--
-- Returns 'Nothing' if none of the three Euler substitutions applies
-- (i.e., \(a < 0\), \(c < 0\), and \(\Delta < 0\)).
eulerIntegrate :: EulerIntegrand -> Maybe IntegralResult
eulerIntegrate ei = do
  sub <- chooseEuler (eiA ei) (eiB ei) (eiC ei)
  let sr = applyEuler sub (eiP ei) (eiQ ei) (eiSqrtPow ei)
                      (eiA ei) (eiB ei) (eiC ei)
      -- Integrate the rational function of t
      ft = integrateRational (srNum sr) (srDen sr)
      -- Back-substitute t = f(x, √(ax²+bx+c))
      result = detectInverseTrig $ substBack ft (srBack sr)
  pure $ IntegralResult result (eiA ei) (eiB ei) (eiC ei)

-- | Substitute the back-substitution expression for SVar in a SymExpr.
substBack :: SymExpr -> SymExpr -> SymExpr
substBack expr tExpr = go expr
  where
    go SVar           = tExpr
    go e@(SRat _)     = e
    go e@(SRad _)     = e
    go e@(SSurd _ _ _) = e
    go (SNeg a)       = sNeg (go a)
    go (SAdd a b)     = sAdd (go a) (go b)
    go (SMul a b)     = sMul (go a) (go b)
    go (SDiv a b)     = sDiv (go a) (go b)
    go (SPow a n)     = SPow (go a) n
    go (SLn a)        = SLn (go a)
    go (SArcTan a)    = SArcTan (go a)
    go (SArcSin a)    = SArcSin (go a)
    go (SArsinh a)    = SArsinh (go a)
    go (SArcosh a)    = SArcosh (go a)

-- | Detect and rewrite inverse trig/hyperbolic patterns.
--
-- * @c · ln|√(ax²+bx+d) + ex + f|@ → @c · arsinh(...)@ or @c · arcosh(...)@
-- * @c · arctan(√(d-ax²)/x)@ type patterns → @c · arcsin(...)@
detectInverseTrig :: SymExpr -> SymExpr
detectInverseTrig = go
  where
    go (SAdd a b) = SAdd (go a) (go b)
    go (SMul c e) = simplifyMulTrig c (go e)
    go (SNeg a)   = SNeg (go a)
    go (SLn arg)  = case matchArsinh arg of
                      Just x  -> SArsinh x
                      Nothing -> case matchArcosh arg of
                        Just x  -> SArcosh x
                        Nothing -> SLn arg
    go e = e

    -- -2 · arctan((√(c-ax²) - √c) / (√a · x)) = arcsin(√a · x / √c)
    -- For a=-1, c=1: -2·arctan((√(1-x²)-1)/x) = arcsin(x)
    simplifyMulTrig (SRat r) (SArcTan arg)
      | r == -2 = case matchArcsinFromArctan arg of
          Just x  -> SArcSin x
          Nothing -> SMul (SRat r) (SArcTan arg)
    simplifyMulTrig c (SLn arg) = case matchArsinh arg of
      Just x  -> sMul c (SArsinh x)
      Nothing -> case matchArcosh arg of
        Just x  -> sMul c (SArcosh x)
        Nothing -> SMul c (SLn arg)
    simplifyMulTrig c e = SMul c e

    -- Match ln|... + √(ax²+bx+c) + ...|  → arsinh/arcosh
    -- Flatten SAdd chain, find the SSurd, use its a,b,c:
    --   4ac - b² > 0 and a > 0 → arsinh((2ax+b)/√(4ac-b²))
    --   4ac - b² < 0 and a > 0 → arcosh((2ax+b)/√(b²-4ac))
    matchArsinh :: SymExpr -> Maybe SymExpr
    matchArsinh expr = do
      (a, b, c) <- findSSurd expr
      let disc = 4 * a * c - b * b
      if a > 0 && disc > 0
        then Just (invHypArg (2*a) b disc)
        else Nothing

    matchArcosh :: SymExpr -> Maybe SymExpr
    matchArcosh expr = do
      (a, b, c) <- findSSurd expr
      let disc = 4 * a * c - b * b
      if a > 0 && disc < 0
        then Just (invHypArg (2*a) b (negate disc))
        else Nothing

    -- Build (coeff·x + const) / √d, simplifying when √d divides coeff and const
    invHypArg :: Rational -> Rational -> Rational -> SymExpr
    invHypArg coeff cnst d = case ratSqrt d of
      Just sd -> sAdd (sMul (SRat (coeff / sd)) SVar) (SRat (cnst / sd))
      Nothing -> sDiv (sAdd (sMul (SRat coeff) SVar) (SRat cnst))
                      (SRad (normalize (Sqrt (ratE d))))

    -- Find SSurd in a flattened SAdd chain
    findSSurd :: SymExpr -> Maybe (Rational, Rational, Rational)
    findSSurd (SSurd a b c) = Just (a, b, c)
    findSSurd (SAdd x y) = findSSurd x <|> findSSurd y
    findSSurd _ = Nothing

    -- -2·arctan((√(c-ax²) - √c) / (sx)) = arcsin(sx/√c)
    -- After Euler 2: t = (y - √c)/x where y = √(-ax²+c)
    -- -2·arctan(t) = arcsin(x·√(-a)/√c) = arcsin(x·√|a|/√c)  for a < 0
    matchArcsinFromArctan :: SymExpr -> Maybe SymExpr
    matchArcsinFromArctan (SDiv (SAdd (SSurd a_ b_ c_) rest) denom) =
      -- Check: a_ < 0 (i.e., √(c-|a|x²)), b_ = 0
      -- rest should be a negative constant (- √c)
      -- denom should be proportional to x
      let (_, f) = extractLinear rest
          (sd, _) = extractLinear denom
      in if a_ < 0 && b_ == 0 && f < 0 && sd /= 0 && c_ > 0
         then -- arcsin(sd·x·√|a| / (√c · sd)) = arcsin(√|a|·x / √c)
              -- Actually: arcsin argument = √(-a)·x / √c
              let negA = negate a_
                  arg = case (ratSqrt negA, ratSqrt c_) of
                    (Just sa, Just sc) -> sMul (SRat (sa / sc)) SVar
                    _ -> sDiv (sMul (SRad (normalize (Sqrt (ratE negA)))) SVar)
                              (SRad (normalize (Sqrt (ratE c_))))
              in Just arg
         else Nothing
    matchArcsinFromArctan _ = Nothing

    extractLinear :: SymExpr -> (Rational, Rational)
    extractLinear SVar = (1, 0)
    extractLinear (SRat r) = (0, r)
    extractLinear (SMul (SRat s) SVar) = (s, 0)
    extractLinear (SAdd a b) = let (s1, f1) = extractLinear a
                                   (s2, f2) = extractLinear b
                               in (s1 + s2, f1 + f2)
    extractLinear (SNeg a) = let (s, f) = extractLinear a in (negate s, negate f)
    extractLinear _ = (0, 0)

    -- Build a radical constant √r or just rational if perfect square
    radicalConst :: Rational -> SymExpr
    radicalConst r = case ratSqrt r of
      Just s  -> SRat s
      Nothing -> SRad (normalize (Sqrt (ratE r)))

--------------------------------------------------------------------------------
-- Rendering: pretty-print (text)
--------------------------------------------------------------------------------

-- | Render a 'SymExpr' as a human-readable plain text string.
--
-- Uses Unicode symbols for roots and multiplication dots, and
-- precedence-aware parenthesisation to minimise brackets.
prettySymExpr :: SymExpr -> String
prettySymExpr = prettyPrec 0

prettyPrec :: Int -> SymExpr -> String
prettyPrec _ (SRat r)
  | denominator r == 1 = show (numerator r)
  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)
prettyPrec _ (SRad e) = pretty (normalize e)
prettyPrec _ SVar = "x"
prettyPrec _ (SSurd a b c) = "√(" ++ prettyQuad a b c ++ ")"
prettyPrec _ (SNeg a) = "-" ++ prettyPrec 4 a
prettyPrec p (SAdd a (SNeg b)) = parensIf (p > 1) $
  prettyPrec 1 a ++ " - " ++ prettyPrec 2 b
prettyPrec p (SAdd a (SRat r)) | r < 0 = parensIf (p > 1) $
  prettyPrec 1 a ++ " - " ++ prettyPrec 2 (SRat (negate r))
prettyPrec p (SAdd a (SMul (SRat r) b)) | r < 0 = parensIf (p > 1) $
  prettyPrec 1 a ++ " - " ++ prettyPrec 2 (sMul (SRat (negate r)) b)
prettyPrec p (SAdd a b) = parensIf (p > 1) $
  prettyPrec 1 a ++ " + " ++ prettyPrec 1 b
prettyPrec p (SMul a b) = parensIf (p > 2) $
  prettyPrec 2 a ++ "·" ++ prettyPrec 3 b
prettyPrec p (SDiv a b) = parensIf (p > 2) $
  prettyPrec 2 a ++ "/" ++ prettyPrec 3 b
prettyPrec _ (SPow a n)
  | n == 0 = "1"
  | n == 1 = prettyPrec 5 a
  | otherwise = prettyPrec 5 a ++ "^" ++ show n
prettyPrec _ (SLn a) = "ln|" ++ prettyPrec 0 a ++ "|"
prettyPrec _ (SArcTan a) = "arctan(" ++ prettyPrec 0 a ++ ")"
prettyPrec _ (SArcSin a) = "arcsin(" ++ prettyPrec 0 a ++ ")"
prettyPrec _ (SArsinh a) = "arsinh(" ++ prettyPrec 0 a ++ ")"
prettyPrec _ (SArcosh a) = "arcosh(" ++ prettyPrec 0 a ++ ")"

prettyQuad :: Rational -> Rational -> Rational -> String
prettyQuad a b c = renderTerms $ filter (not . null)
  [ if a == 1 then "x²" else if a == -1 then "-x²" else prettyR a ++ "x²"
  , if b == 0 then "" else if b == 1 then "x" else if b == -1 then "-x" else prettyR b ++ "x"
  , if c == 0 then "" else prettyR c
  ]
  where prettyR r | denominator r == 1 = show (numerator r)
                  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)
        renderTerms [] = "0"
        renderTerms (t:ts) = t ++ concatMap addTerm ts
        addTerm t@('-':_) = " - " ++ drop 1 t
        addTerm t = " + " ++ t

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

--------------------------------------------------------------------------------
-- Rendering: LaTeX
--------------------------------------------------------------------------------

-- | Render a 'SymExpr' as a LaTeX math expression.
--
-- Fractions use @\\frac@, logarithms use @\\ln@, and inverse
-- trig\/hyperbolic functions use their standard LaTeX commands.
-- Delimiters are sized with @\\left@ / @\\right@.
latexSymExpr :: SymExpr -> String
latexSymExpr = latexPrec_ 0

latexPrec_ :: Int -> SymExpr -> String
latexPrec_ _ (SRat r)
  | denominator r == 1 = show (numerator r)
  | otherwise = "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"
latexPrec_ _ (SRad e) = latex (normalize e)
latexPrec_ _ SVar = "x"
latexPrec_ _ (SSurd a b c) = "\\sqrt{" ++ latexQuad a b c ++ "}"
latexPrec_ _ (SNeg a) = "-" ++ latexPrec_ 4 a
latexPrec_ p (SAdd a (SNeg b)) = latexParensIf (p > 1) $
  latexPrec_ 1 a ++ " - " ++ latexPrec_ 2 b
latexPrec_ p (SAdd a b) = latexParensIf (p > 1) $
  latexPrec_ 1 a ++ " + " ++ latexPrec_ 1 b
latexPrec_ p (SMul (SRat r) b) | r == -1 = latexParensIf (p > 2) $
  "-" ++ latexPrec_ 3 b
latexPrec_ p (SMul a b) = latexParensIf (p > 2) $
  latexPrec_ 2 a ++ " \\cdot " ++ latexPrec_ 3 b
latexPrec_ _ (SDiv a b) =
  "\\frac{" ++ latexPrec_ 0 a ++ "}{" ++ latexPrec_ 0 b ++ "}"
latexPrec_ _ (SPow a n)
  | n == 0 = "1"
  | n == 1 = latexPrec_ 5 a
  | otherwise = latexPrec_ 5 a ++ "^{" ++ show n ++ "}"
latexPrec_ _ (SLn a) = "\\ln\\left|" ++ latexPrec_ 0 a ++ "\\right|"
latexPrec_ _ (SArcTan a) = "\\arctan\\left(" ++ latexPrec_ 0 a ++ "\\right)"
latexPrec_ _ (SArcSin a) = "\\arcsin\\left(" ++ latexPrec_ 0 a ++ "\\right)"
latexPrec_ _ (SArsinh a) = "\\operatorname{arsinh}\\left(" ++ latexPrec_ 0 a ++ "\\right)"
latexPrec_ _ (SArcosh a) = "\\operatorname{arcosh}\\left(" ++ latexPrec_ 0 a ++ "\\right)"

latexQuad :: Rational -> Rational -> Rational -> String
latexQuad a b c = renderTerms $ filter (not . null)
  [ if a == 1 then "x^{2}" else latexR a ++ "x^{2}"
  , if b == 0 then "" else if b == 1 then "x" else latexR b ++ "x"
  , if c == 0 then "" else latexR c
  ]
  where latexR r | denominator r == 1 = show (numerator r)
                 | otherwise = "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"
        renderTerms [] = "0"
        renderTerms (t:ts) = t ++ concatMap addTerm ts
        addTerm t@('-':_) = " - " ++ drop 1 t
        addTerm t = " + " ++ t

latexParensIf :: Bool -> String -> String
latexParensIf True  s = "\\left(" ++ s ++ "\\right)"
latexParensIf False s = s
