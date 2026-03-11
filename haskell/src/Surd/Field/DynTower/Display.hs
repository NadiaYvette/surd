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

import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Ratio (denominator, numerator)
import Surd.Field.DynTower (TowerElem (..), TowerLevel (..), tIsZero)

-- ---------------------------------------------------------------------------
-- Tower structure extraction
-- ---------------------------------------------------------------------------

-- | A single extension step in the tower.
data ExtensionStep = ExtensionStep
  { -- | The tower level
    esLevel :: !TowerLevel,
    -- | Human-readable name for the generator (α₁, α₂, ...)
    esName :: !String,
    -- | LaTeX name for the generator
    esLatexName :: !String,
    -- | Degree of the extension
    esDegree :: !Int,
    -- | Minimal polynomial description: αⁿ = radicand
    esRadicand :: !TowerElem
  }
  deriving (Show)

-- | Complete tower display data.
data TowerDisplay = TowerDisplay
  { -- | Extension steps, from bottom (closest to Q) to top
    tdSteps :: ![ExtensionStep],
    -- | The element being displayed, expressed in terms of the tower
    tdElement :: !TowerElem,
    -- | Label for the element (e.g. "cos(2π/11)")
    tdLabel :: !String
  }
  deriving (Show)

-- | Extract the tower structure from a TowerElem.
-- Collects all distinct tower levels referenced (transitively)
-- and orders them from bottom to top.
extractTower :: String -> TowerElem -> TowerDisplay
extractTower label e =
  let levels = collectLevels e
      sorted = sortBy (comparing tlId) (Map.elems levels)
      steps = zipWith mkStep [1 ..] sorted
   in TowerDisplay
        { tdSteps = steps,
          tdElement = e,
          tdLabel = label
        }
  where
    mkStep i lvl =
      ExtensionStep
        { esLevel = lvl,
          esName = greekName i,
          esLatexName = greekLatexName i,
          esDegree = tlDegree lvl,
          esRadicand = tlRadicand lvl
        }

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

buildNameMap :: [ExtensionStep] -> NameMap
buildNameMap steps = Map.fromList [(tlId (esLevel s), s) | s <- steps]

-- ---------------------------------------------------------------------------
-- LaTeX rendering
-- ---------------------------------------------------------------------------

-- | Render a tower element as a complete LaTeX display.
-- Shows the tower of field extensions and the element's expression.
latexTower :: TowerDisplay -> String
latexTower td =
  let nm = buildNameMap (tdSteps td)
      steps = map (latexStep nm) (tdSteps td)
      elemStr = latexTE nm (tdElement td)
   in unlines $
        [ "\\begin{align*}" ]
          ++ [ "& \\textbf{Tower of extensions:} \\\\" | not (null (tdSteps td)) ]
          ++ steps
          ++ [ "& " ++ tdLabel td ++ " = " ++ elemStr,
               "\\end{align*}"
             ]

-- | Render a single extension step in LaTeX.
latexStep :: NameMap -> ExtensionStep -> String
latexStep nm step =
  let name = esLatexName step
      n = esDegree step
      rad = latexTE nm (esRadicand step)
   in "& " ++ name ++ "^{" ++ show n ++ "} = " ++ rad
        ++ " \\qquad "
        ++ "\\bigl["
        ++ name ++ " = "
        ++ rootExpr name n rad
        ++ "\\bigr]"
        ++ " \\\\"
  where
    rootExpr _ 2 r = "\\sqrt{" ++ r ++ "}"
    rootExpr _ 3 r = "\\sqrt[3]{" ++ r ++ "}"
    rootExpr _ deg r = "\\sqrt[" ++ show deg ++ "]{" ++ r ++ "}"

-- | Render a TowerElem as LaTeX, using named generators.
latexTowerElem :: TowerDisplay -> TowerElem -> String
latexTowerElem td = latexTE (buildNameMap (tdSteps td))

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

-- | Render a tower element as a complete text display.
prettyTower :: TowerDisplay -> String
prettyTower td =
  let nm = buildNameMap (tdSteps td)
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

-- | Render a TowerElem as text, using named generators.
prettyTowerElem :: TowerDisplay -> TowerElem -> String
prettyTowerElem td = prettyTE (buildNameMap (tdSteps td))

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
