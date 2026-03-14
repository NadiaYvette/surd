-- | Display tower elements as structured field extension towers.
--
-- Instead of expanding tower elements into radical trees (which explode
-- exponentially for solvable quintics etc.), this module renders the
-- tower structure directly: each level is shown as a field extension
-- with its minimal polynomial, and elements are polynomials in the
-- generator of each level.
--
-- This follows Gauss's own presentation style from the Disquisitiones
-- Arithmeticae: name the intermediate period sums and show the equations
-- they satisfy, rather than displaying fully-expanded radical formulas.
module Surd.Field.DynTower.Display
  ( -- * LaTeX rendering
    latexTower,
    latexTowerElem,

    -- * Text rendering
    prettyTower,
    prettyTowerElem,

    -- * Tower structure extraction
    TowerDisplay (..),
    ExtensionStep (..),
    extractTower,
  )
where

import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Ratio (denominator, numerator)
import Surd.Field.DynTower (TowerElem (..), TowerLevel (..), tIsZero)

-- ---------------------------------------------------------------------------
-- Tower structure extraction
-- ---------------------------------------------------------------------------

-- | A single extension step in the tower, associating a human-readable
-- name with the generator of that extension level.
--
-- For example, in the tower \(\mathbb{Q} \subset \mathbb{Q}(\alpha)
-- \subset \mathbb{Q}(\alpha)(\beta)\), there would be two steps:
-- one for \(\alpha\) (e.g., a square root) and one for \(\beta\).
data ExtensionStep = ExtensionStep
  { -- | The underlying tower level
    esLevel :: !TowerLevel,
    -- | Human-readable name for the generator (e.g., "α", "β", "γ")
    esName :: !String,
    -- | LaTeX name for the generator (e.g., "\\alpha", "\\beta")
    esLatexName :: !String,
    -- | Degree of the extension (\(\alpha^n = r\))
    esDegree :: !Int,
    -- | Radicand: the element \(r\) such that the generator satisfies \(\alpha^n = r\)
    esRadicand :: !TowerElem
  }
  deriving (Show)

-- | Complete tower display data: the sequence of extension steps plus
-- the element to be rendered and a label string.
--
-- Produced by 'extractTower' and consumed by 'latexTower' or 'prettyTower'.
data TowerDisplay = TowerDisplay
  { -- | Extension steps, ordered from bottom (closest to \(\mathbb{Q}\)) to top
    tdSteps :: ![ExtensionStep],
    -- | The element being displayed, expressed in terms of the tower generators
    tdElement :: !TowerElem,
    -- | Label for the element (e.g., @\"cos(2π\/11)\"@)
    tdLabel :: !String
  }
  deriving (Show)

-- | Extract the tower structure from a 'TowerElem'.
--
-- Traverses the element recursively to collect all distinct tower levels
-- referenced (including those in radicands of other levels). Levels with
-- the same degree and structurally equivalent radicand are merged (e.g.,
-- two independent \(\sqrt{-3}\) extensions get a single name). The
-- resulting steps are ordered from bottom (closest to \(\mathbb{Q}\))
-- to top.
--
-- The @label@ argument is used as a display label for the element
-- (e.g., @\"cos(2π\/11)\"@).
extractTower :: String -> TowerElem -> TowerDisplay
extractTower label e =
  let levels = collectLevels e
      sorted = sortBy (comparing tlId) (Map.elems levels)
      -- Group levels by (degree, radicand) and assign canonical names.
      -- Within each group, the first (lowest ID) level is canonical.
      (steps, _) = mergeEquivLevels sorted
   in TowerDisplay
        { tdSteps = steps,
          tdElement = e,
          tdLabel = label
        }

-- | Merge tower levels that have the same degree and radicand.
-- Returns deduplicated steps and a map from each level ID to its
-- canonical ExtensionStep (so the renderer uses the right name).
mergeEquivLevels :: [TowerLevel] -> ([ExtensionStep], Map Int ExtensionStep)
mergeEquivLevels lvls =
  let -- Group by structural equivalence of (degree, radicand).
      -- Two levels are equivalent if they have the same degree and
      -- their radicands are structurally equal (TowerElem Eq).
      go [] _ = ([], Map.empty)
      go (l : ls) nextName =
        -- Find all levels equivalent to l that haven't been assigned yet
        let -- Partition ls into equivalent and non-equivalent
            (moreEquivs, nonEquivs) = foldr classifyLevel ([], []) ls
            classifyLevel l' (eq, ne)
              | tlDegree l == tlDegree l' && radicandEquiv (tlRadicand l) (tlRadicand l') =
                  (l' : eq, ne)
              | otherwise = (eq, l' : ne)
            step =
              ExtensionStep
                { esLevel = l,
                  esName = greekName nextName,
                  esLatexName = greekLatexName nextName,
                  esDegree = tlDegree l,
                  esRadicand = tlRadicand l
                }
            -- Map all equivalent level IDs to this step
            idEntries = [(tlId l, step)] ++ [(tlId l', step) | l' <- moreEquivs]
            (restSteps, restMap) = go nonEquivs (nextName + 1)
         in (step : restSteps, Map.union (Map.fromList idEntries) restMap)
   in go lvls 1

-- | Check if two radicands are semantically equivalent.
-- Normalizes both to a canonical form (stripping tower-level wrappers
-- and comparing by underlying coefficients and level structure) to
-- detect equivalent radicands expressed through different parent levels.
radicandEquiv :: TowerElem -> TowerElem -> Bool
radicandEquiv a b = canonicalize a == canonicalize b

-- | Canonical form for comparison: strip tower level wrappers,
-- keeping only the essential structure (rationals, coefficient lists,
-- and root degrees/radicands at each level).
data CanonElem
  = CRat !Rational
  | CExt ![CanonElem] !Int !CanonElem -- coeffs, rootDeg, radicand
  deriving (Eq)

canonicalize :: TowerElem -> CanonElem
canonicalize (TRat r) = CRat r
-- Constant polynomial (just the first coefficient, rest zero):
-- collapse to the coefficient's canonical form
canonicalize (TExt (c : cs) _)
  | all tIsZero cs = canonicalize c
canonicalize (TExt cs lvl) =
  CExt
    (map canonicalize cs)
    (tlRootDeg lvl)
    (canonicalize (tlRadicand lvl))

-- | Collect all distinct TowerLevels referenced by a TowerElem.
collectLevels :: TowerElem -> Map Int TowerLevel
collectLevels (TRat _) = Map.empty
collectLevels (TExt cs lvl) =
  let fromCoeffs = Map.unions (map collectLevels cs)
      fromRadicand = collectLevels (tlRadicand lvl)
   in Map.insert (tlId lvl) lvl (Map.union fromCoeffs fromRadicand)

-- | Generate Greek letter names: α, β, γ, δ, ε, ...
greekName :: Int -> String
greekName i = greekLetters !! (i - 1)
  where
    greekLetters =
      ["α", "β", "γ", "δ", "ε", "ζ_t", "η_t", "θ_t"]
        ++ ["α" ++ show n | n <- [2 :: Int ..]]

-- | Generate Greek letter LaTeX names.
greekLatexName :: Int -> String
greekLatexName i = greekLetters !! (i - 1)
  where
    greekLetters =
      [ "\\alpha",
        "\\beta",
        "\\gamma",
        "\\delta",
        "\\varepsilon",
        "\\zeta_t",
        "\\eta_t",
        "\\theta_t"
      ]
        ++ ["\\alpha_{" ++ show n ++ "}" | n <- [2 :: Int ..]]

-- ---------------------------------------------------------------------------
-- Name lookup
-- ---------------------------------------------------------------------------

type NameMap = Map Int ExtensionStep

buildNameMap :: TowerDisplay -> NameMap
buildNameMap td =
  let -- The mergeEquivLevels already built a complete ID→step map,
      -- but we stored only the deduplicated steps. Rebuild from the element.
      levels = collectLevels (tdElement td)
      steps = tdSteps td
      -- For each level ID, find the step with matching (degree, radicand)
      findStep lvl =
        case filter (\s -> esDegree s == tlDegree lvl && radicandEquiv (esRadicand s) (tlRadicand lvl)) steps of
          (s : _) -> s
          [] -> -- Fallback: find by exact level ID match
            case filter (\s -> tlId (esLevel s) == tlId lvl) steps of
              (s : _) -> s
              [] -> error $ "buildNameMap: no step for level " ++ show (tlId lvl)
   in Map.fromList [(lid, findStep lvl) | (lid, lvl) <- Map.toList levels]

-- ---------------------------------------------------------------------------
-- LaTeX rendering
-- ---------------------------------------------------------------------------

-- | Render a complete tower display as LaTeX, showing both the tower
-- of field extensions and the final element expression.
--
-- Each extension step is shown as \(\alpha^n = r\) with a bracketed
-- notation \([\alpha = \sqrt[n]{r}]\). The output is wrapped in an
-- @alignat*@ environment. Long radicands are automatically split
-- across multiple alignment rows for readability.
latexTower :: TowerDisplay -> String
latexTower td =
  let nm = buildNameMap td
      steps = concatMap (latexStep nm) (tdSteps td)
      elemStr = latexTE nm (tdElement td)
      elemLines = latexElemLines (tdLabel td) elemStr
   in unlines $
        [ "\\begin{alignat*}{2}" ]
          ++ [ "& & \\textbf{Tower of extensions:} \\\\" | not (null (tdSteps td)) ]
          ++ steps
          ++ elemLines
          ++ [ "\\end{alignat*}" ]

-- | Render a single extension step in LaTeX.
-- Short radicands (≤80 chars) are shown on one line with an inline bracket.
-- Long radicands put the bracket on a separate alignment row, giving it
-- the full text width rather than squeezing it inside a sub-environment.
-- Returns a list of alignment rows.
latexStep :: NameMap -> ExtensionStep -> [String]
latexStep nm step =
  let name = esLatexName step
      n = esDegree step
      rad = latexTE nm (esRadicand step)
      lhs = name ++ "^{" ++ show n ++ "}"
      bracketInline = "\\bigl[" ++ name ++ " = " ++ rootExpr n rad ++ "\\bigr]"
      bracketRow = "\\bigl[" ++ name ++ " &= &" ++ rootExpr n rad ++ "\\bigr]"
   in if length rad <= 80
        then [lhs ++ " &= &" ++ rad ++ " \\qquad " ++ bracketInline ++ " \\\\"]
        else
          [ lhs ++ " &= &" ++ rad ++ " \\\\",
            bracketRow ++ " \\\\"
          ]
  where
    rootExpr 2 r = "\\sqrt{" ++ r ++ "}"
    rootExpr 3 r = "\\sqrt[3]{" ++ r ++ "}"
    rootExpr deg r = "\\sqrt[" ++ show deg ++ "]{" ++ r ++ "}"

-- | Render the final element line(s).  Short expressions go on one row;
-- long ones use @split@ to break at top-level @+@/@-@ boundaries.
latexElemLines :: String -> String -> [String]
latexElemLines label expr
  | length expr <= 120 = [label ++ " &= &" ++ expr]
  | otherwise =
      let terms = splitAtTerms expr
       in case terms of
            [] -> [label ++ " &= &" ++ expr]
            [_] -> [label ++ " &= &" ++ expr]
            (first : rest) ->
              [ label ++ " &= \\begin{split} &" ++ first ++ " \\\\"
              ]
                ++ map (\t -> "  &" ++ t ++ " \\\\") (init rest)
                ++ [ "  &" ++ last rest,
                     "  \\end{split}"
                   ]

-- | Split a rendered LaTeX expression at top-level @+@ or @-@ boundaries
-- (brace/delimiter depth 0), aiming for lines of roughly 80–100 characters.
splitAtTerms :: String -> [String]
splitAtTerms = finish . go (0 :: Int) (0 :: Int) (0 :: Int) ""
  where
    go _ _ _ acc [] = [reverse acc]
    go bd dd col acc ('{' : cs) = go (bd + 1) dd (col + 1) ('{' : acc) cs
    go bd dd col acc ('}' : cs) = go (max 0 (bd - 1)) dd (col + 1) ('}' : acc) cs
    go bd dd col acc ('\\' : cs)
      | Just rest <- stripPfx "left" cs =
          go bd (dd + 1) (col + 5) ('t' : 'f' : 'e' : 'l' : '\\' : acc) rest
      | Just rest <- stripPfx "right" cs =
          go bd (max 0 (dd - 1)) (col + 6) ('t' : 'h' : 'g' : 'i' : 'r' : '\\' : acc) rest
      | otherwise = go bd dd (col + 1) ('\\' : acc) cs
    go bd dd col acc (c : cs)
      | bd == 0,
        dd == 0,
        col >= 80,
        c == ' ',
        Just (op, rest) <- matchOp cs =
          reverse acc : go 0 0 (length op) (reverse op) rest
      | otherwise = go bd dd (col + 1) (c : acc) cs

    matchOp ('+' : ' ' : rest) = Just ("+ ", rest)
    matchOp ('-' : ' ' : rest) = Just ("- ", rest)
    matchOp _ = Nothing

    stripPfx [] s = Just s
    stripPfx (p : ps) (x : xs) | p == x = stripPfx ps xs
    stripPfx _ _ = Nothing

    finish = filter (not . null)

-- | Render a single 'TowerElem' as a LaTeX expression, using the
-- named generators from the given 'TowerDisplay'. The element is
-- expressed as a polynomial in the generators (e.g.,
-- @\\alpha^2 + 3\\beta@).
latexTowerElem :: TowerDisplay -> TowerElem -> String
latexTowerElem td = latexTE (buildNameMap td)

latexTE :: NameMap -> TowerElem -> String
latexTE _ (TRat r) = latexRat r
latexTE nm (TExt cs lvl) =
  case Map.lookup (tlId lvl) nm of
    Nothing -> "?_{" ++ show (tlId lvl) ++ "}"
    Just step ->
      let name = esLatexName step
          terms = collectTerms name nm cs
       in case terms of
            [] -> "0"
            _ -> renderLatexTerms terms

-- | Collect non-zero terms from coefficient list as (sign, rendered) pairs.
collectTerms :: String -> NameMap -> [TowerElem] -> [(Bool, String)]
collectTerms name nm cs =
  [ (positive, rendered)
    | (c, i) <- zip cs [0 :: Int ..],
      not (tIsZero c),
      let (positive, rendered) = renderCoeffTerm name nm c i
  ]

-- | Render a single term: coefficient * generator^power.
renderCoeffTerm :: String -> NameMap -> TowerElem -> Int -> (Bool, String)
renderCoeffTerm _name nm c 0 =
  let s = latexTE nm c
   in case s of
        ('-' : rest) -> (False, rest)
        _ -> (True, s)
renderCoeffTerm name nm c i =
  let genPart = if i == 1 then name else name ++ "^{" ++ show i ++ "}"
   in case c of
        TRat 1 -> (True, genPart)
        TRat (-1) -> (False, genPart)
        TRat r
          | r > 0 -> (True, latexRat r ++ " " ++ genPart)
          | otherwise -> (False, latexRat (negate r) ++ " " ++ genPart)
        _ ->
          let s = latexTE nm c
           in case s of
                ('-' : rest) -> (False, wrapIfCompound rest ++ " " ++ genPart)
                _ -> (True, wrapIfCompound s ++ " " ++ genPart)
  where
    wrapIfCompound s
      | any (`elem` ("+-" :: String)) (drop 1 s) =
          "\\left(" ++ s ++ "\\right)" ++ " \\cdot"
      | otherwise = s

-- | Render a list of signed terms with + and - separators.
renderLatexTerms :: [(Bool, String)] -> String
renderLatexTerms [] = "0"
renderLatexTerms ((s, t) : rest) =
  let hd = if s then t else "-" ++ t
   in hd ++ concatMap renderRest rest
  where
    renderRest (True, e) = " + " ++ e
    renderRest (False, e) = " - " ++ e

latexRat :: Rational -> String
latexRat r
  | d == 1 = show n'
  | n' < 0 = "-\\frac{" ++ show (abs n') ++ "}{" ++ show d ++ "}"
  | otherwise = "\\frac{" ++ show n' ++ "}{" ++ show d ++ "}"
  where
    n' = numerator r
    d = denominator r

-- ---------------------------------------------------------------------------
-- Text rendering
-- ---------------------------------------------------------------------------

-- | Render a complete tower display as plain text, showing the tower
-- of extensions and the final element expression.
--
-- Each extension step is shown with Unicode root symbols (e.g.,
-- @α^2 = -3    [α = √(-3)]@). The final element is expressed as
-- a polynomial in the named generators.
prettyTower :: TowerDisplay -> String
prettyTower td =
  let nm = buildNameMap td
      steps = map (prettyStep nm) (tdSteps td)
      elemStr = prettyTE nm (tdElement td)
   in unlines $
        [ "Tower of extensions:" | not (null (tdSteps td)) ]
          ++ steps
          ++ ["", tdLabel td ++ " = " ++ elemStr]

-- | Render a single extension step in text.
prettyStep :: NameMap -> ExtensionStep -> String
prettyStep nm step =
  let name = esName step
      n = esDegree step
      rad = prettyTE nm (esRadicand step)
   in "  " ++ name ++ "^" ++ show n ++ " = " ++ rad
        ++ "    [" ++ name ++ " = " ++ rootStr n ++ "(" ++ rad ++ ")]"
  where
    rootStr 2 = "√"
    rootStr 3 = "∛"
    rootStr deg = show deg ++ "√"

-- | Render a single 'TowerElem' as a plain text expression, using the
-- named generators from the given 'TowerDisplay'.
prettyTowerElem :: TowerDisplay -> TowerElem -> String
prettyTowerElem td = prettyTE (buildNameMap td)

prettyTE :: NameMap -> TowerElem -> String
prettyTE _ (TRat r) = prettyRat r
prettyTE nm (TExt cs lvl) =
  case Map.lookup (tlId lvl) nm of
    Nothing -> "?_" ++ show (tlId lvl)
    Just step ->
      let name = esName step
          terms = collectTextTerms name nm cs
       in case terms of
            [] -> "0"
            _ -> renderTextTerms terms

collectTextTerms :: String -> NameMap -> [TowerElem] -> [(Bool, String)]
collectTextTerms name nm cs =
  [ (positive, rendered)
    | (c, i) <- zip cs [0 :: Int ..],
      not (tIsZero c),
      let (positive, rendered) = renderTextCoeffTerm name nm c i
  ]

renderTextCoeffTerm :: String -> NameMap -> TowerElem -> Int -> (Bool, String)
renderTextCoeffTerm _name nm c 0 =
  let s = prettyTE nm c
   in case s of
        ('-' : rest) -> (False, rest)
        _ -> (True, s)
renderTextCoeffTerm name nm c i =
  let genPart = if i == 1 then name else name ++ "^" ++ show i
   in case c of
        TRat 1 -> (True, genPart)
        TRat (-1) -> (False, genPart)
        TRat r
          | r > 0 -> (True, prettyRat r ++ "·" ++ genPart)
          | otherwise -> (False, prettyRat (negate r) ++ "·" ++ genPart)
        _ ->
          let s = prettyTE nm c
           in case s of
                ('-' : rest) -> (False, "(" ++ rest ++ ")·" ++ genPart)
                _ ->
                  if any (`elem` ("+-" :: String)) (drop 1 s)
                    then (True, "(" ++ s ++ ")·" ++ genPart)
                    else (True, s ++ "·" ++ genPart)

renderTextTerms :: [(Bool, String)] -> String
renderTextTerms [] = "0"
renderTextTerms ((s, t) : rest) =
  let hd = if s then t else "-" ++ t
   in hd ++ concatMap renderRest rest
  where
    renderRest (True, e) = " + " ++ e
    renderRest (False, e) = " - " ++ e

prettyRat :: Rational -> String
prettyRat r
  | d == 1 = show n'
  | n' < 0 = "(-" ++ show (abs n') ++ "/" ++ show d ++ ")"
  | otherwise = "(" ++ show n' ++ "/" ++ show d ++ ")"
  where
    n' = numerator r
    d = denominator r
