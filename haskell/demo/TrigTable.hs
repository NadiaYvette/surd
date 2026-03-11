-- | Generate LaTeX tables of exact trigonometric values.
--
-- Usage:
--   surd-trig-table [OPTIONS] SPEC [SPEC ...]
--
-- Each SPEC is one of:
--   N          — tabulate cos and sin at kπ/N for k = 0, 1, ..., N
--   p/q        — single angle pπ/q
--   N..M       — tabulate at kπ/q for each q in N..M, k = 0..q
--
-- Options:
--   --format=latex     LaTeX longtable (default)
--   --format=text      plain text (Unicode)
--   --standalone       emit a complete LaTeX document (default: fragment)
--   --no-standalone    emit only the table environment
--   --cos-only         omit sine column
--   --sin-only         omit cosine column
--   --tan              include tangent column
--   --force-radical    render formulas without simplification limits
--
-- Examples:
--   surd-trig-table 12
--   surd-trig-table --standalone 8 12
--   surd-trig-table 1/7 2/7 3/7
--   surd-trig-table --format=text 5..12
module Main (main) where

import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf)
import Surd.Radical.LaTeX (latex, latexDAG)
import Surd.Radical.Normalize (normalize)
import Surd.Radical.Pretty (pretty)
import Surd.Trig (TrigResult (..), cosExact, simplifyTrigResult, sinExact)
import Surd.Types (RadExpr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- --------------------------------------------------------------------------
-- Configuration
-- --------------------------------------------------------------------------

data Format = LaTeX | Text deriving (Eq)

data Cols = CosAndSin | CosOnly | SinOnly deriving (Eq)

data Config = Config
  { cfgFormat :: Format,
    cfgStandalone :: Bool,
    cfgCols :: Cols,
    cfgTan :: Bool,
    cfgForceRadical :: Bool
  }

defaultConfig :: Config
defaultConfig = Config LaTeX True CosAndSin False False

-- --------------------------------------------------------------------------
-- Angle specification
-- --------------------------------------------------------------------------

data Spec
  = -- | p/q
    Single Integer Integer
  | -- | all k/n for k = 0..n
    Denom Integer
  | -- | all k/q for q in lo..hi, k = 0..q
    Range Integer Integer

-- | An angle to tabulate: (p, q) meaning pπ/q.
type Angle = (Integer, Integer)

specAngles :: Spec -> [Angle]
specAngles (Single p q) = [(p, q)]
specAngles (Denom n) = [(k, n) | k <- [0 .. n]]
specAngles (Range lo hi) =
  [(k, q) | q <- [lo .. hi], k <- [0 .. q]]

-- --------------------------------------------------------------------------
-- Parsing
-- --------------------------------------------------------------------------

parseArgs :: [String] -> Either String (Config, [Spec])
parseArgs args =
  let (opts, rest) = span ("--" `isPrefixOf`) args
   in do
        cfg <- foldl (\c o -> c >>= applyOpt o) (Right defaultConfig) opts
        specs <- mapM parseSpec rest
        if null specs
          then Left "no angle specifications given"
          else Right (cfg, specs)

applyOpt :: String -> Config -> Either String Config
applyOpt "--format=latex" c = Right c {cfgFormat = LaTeX}
applyOpt "--format=text" c = Right c {cfgFormat = Text}
applyOpt "--standalone" c = Right c {cfgStandalone = True}
applyOpt "--no-standalone" c = Right c {cfgStandalone = False}
applyOpt "--cos-only" c = Right c {cfgCols = CosOnly}
applyOpt "--sin-only" c = Right c {cfgCols = SinOnly}
applyOpt "--tan" c = Right c {cfgTan = True}
applyOpt "--force-radical" c = Right c {cfgForceRadical = True}
applyOpt o _ = Left $ "unknown option: " ++ o

parseSpec :: String -> Either String Spec
parseSpec s
  | (a, '.' : '.' : b) <- break (== '.') s,
    all isDigit a,
    not (null a),
    all isDigit b,
    not (null b) =
      Right (Range (read a) (read b))
  | (p, '/' : q) <- break (== '/') s,
    validInt p,
    all isDigit q,
    not (null q) =
      Right (Single (readSigned p) (read q))
  | all isDigit s, not (null s) = Right (Denom (read s))
  | otherwise = Left $ "cannot parse angle spec: " ++ show s
  where
    validInt ('-' : ds) = not (null ds) && all isDigit ds
    validInt ds = not (null ds) && all isDigit ds
    readSigned ('-' : ds) = negate (read ds)
    readSigned ds = read ds

-- --------------------------------------------------------------------------
-- Rendering
-- --------------------------------------------------------------------------

renderAngle :: Format -> Angle -> String
renderAngle LaTeX (p, q) = latexFrac p q
renderAngle Text (p, q) = textFrac p q

latexFrac :: Integer -> Integer -> String
latexFrac 0 _ = "0"
latexFrac p 1 = showCoeffPi p
latexFrac p q =
  let (sn, np) = signAndCoeff p
   in sn ++ "\\frac{" ++ np ++ "\\pi}{" ++ show q ++ "}"
  where
    signAndCoeff n
      | n < 0 && abs n == 1 = ("-", "")
      | n < 0 = ("-", show (abs n))
      | n == 1 = ("", "")
      | otherwise = ("", show n)

showCoeffPi :: Integer -> String
showCoeffPi 1 = "\\pi"
showCoeffPi (-1) = "-\\pi"
showCoeffPi n = show n ++ "\\pi"

textFrac :: Integer -> Integer -> String
textFrac 0 _ = "0"
textFrac p 1
  | p == 1 = "π"
  | p == -1 = "-π"
  | otherwise = show p ++ "π"
textFrac 1 q = "π/" ++ show q
textFrac (-1) q = "-π/" ++ show q
textFrac p q = show p ++ "π/" ++ show q

-- | A rendered result may carry DAG definitions for large expressions.
data RenderedResult = RenderedResult
  { -- | DAG variable definitions (name, body)
    rrDefs :: [(String, String)],
    -- | The expression (may reference DAG variables)
    rrExpr :: String
  }

renderResult :: Config -> TrigResult -> RenderedResult
renderResult cfg (Radical e)
  | cfgFormat cfg == LaTeX && cfgForceRadical cfg =
      RenderedResult [] (breakTexLines 500 (latex e))
  | cfgFormat cfg == LaTeX =
      let (defs, expr) = latexDAG e
       in RenderedResult defs expr
  | otherwise = RenderedResult [] (pretty e)
renderResult cfg (MinPoly p)
  | cfgFormat cfg == LaTeX = RenderedResult [] ("\\text{minpoly: }" ++ show p)
  | otherwise = RenderedResult [] ("minpoly: " ++ show p)

-- --------------------------------------------------------------------------
-- Table generation
-- --------------------------------------------------------------------------

data Row = Row
  { rowAngle :: String,
    rowCos :: Maybe RenderedResult,
    rowSin :: Maybe RenderedResult,
    rowTan :: Maybe RenderedResult
  }

-- | Collect all DAG definitions from a row.
rowDefs :: Row -> [(String, String)]
rowDefs r = concatMap (maybe [] rrDefs) [rowCos r, rowSin r, rowTan r]

computeRow :: Config -> Angle -> Row
computeRow cfg (p, q) =
  let simp = if cfgForceRadical cfg then id else simplifyTrigResult
      simpCos = simp (cosExact p q)
      simpSin = simp (sinExact p q)
   in Row
        { rowAngle = renderAngle (cfgFormat cfg) (p, q),
          rowCos =
            if cfgCols cfg /= SinOnly
              then Just (renderResult cfg simpCos)
              else Nothing,
          rowSin =
            if cfgCols cfg /= CosOnly
              then Just (renderResult cfg simpSin)
              else Nothing,
          rowTan =
            if cfgTan cfg
              then Just $ computeTan cfg simpCos simpSin
              else Nothing
        }

computeTan :: Config -> TrigResult -> TrigResult -> RenderedResult
computeTan cfg simpCos simpSin =
  case (simpSin, simpCos) of
    (Radical s, Radical c)
      | isZero c -> undefinedStr (cfgFormat cfg)
      | isZero s -> renderResult cfg (Radical (Lit 0))
      | otherwise -> renderResult cfg (simplifyTrigResult (Radical (normalize (Mul s (Inv c)))))
    _ -> undefinedStr (cfgFormat cfg)
  where
    isZero (Lit 0) = True
    isZero _ = False
    undefinedStr LaTeX = RenderedResult [] "\\text{undefined}"
    undefinedStr Text = RenderedResult [] "undefined"

-- --------------------------------------------------------------------------
-- LaTeX output
-- --------------------------------------------------------------------------

latexTable :: Config -> [Row] -> String
latexTable cfg rows =
  let cols = colSpec cfg
      hdr = header cfg
      body = unlines $ map (latexRow cfg) rows
      -- Collect and deduplicate DAG definitions from all rows
      allDefs = dedup (concatMap rowDefs rows)
      defsBlock
        | null allDefs = ""
        | otherwise =
            unlines
              [ "\\noindent\\textbf{Where:}",
                "\\begin{align*}",
                unlines [name ++ " &= " ++ defn ++ " \\\\" | (name, defn) <- init allDefs]
                  ++ let (name, defn) = last allDefs in name ++ " &= " ++ defn,
                "\\end{align*}",
                ""
              ]
      table =
        unlines
          [ "\\begin{longtable}{" ++ cols ++ "}",
            "\\toprule",
            hdr ++ " \\\\",
            "\\midrule",
            "\\endhead",
            body,
            "\\bottomrule",
            "\\end{longtable}"
          ]
   in if cfgStandalone cfg
        then latexPreamble ++ defsBlock ++ table ++ "\\end{document}\n"
        else defsBlock ++ table
  where
    -- Remove duplicate definitions (same name)
    dedup [] = []
    dedup ((n, d) : rest) = (n, d) : dedup (filter (\(n', _) -> n' /= n) rest)

colSpec :: Config -> String
colSpec cfg = 'c' : replicate (numCols cfg) 'l'

numCols :: Config -> Int
numCols cfg =
  (if cfgCols cfg /= SinOnly then 1 else 0)
    + (if cfgCols cfg /= CosOnly then 1 else 0)
    + (if cfgTan cfg then 1 else 0)

header :: Config -> String
header cfg =
  intercalate " & " $
    ["$\\theta$"]
      ++ ["$\\cos\\theta$" | cfgCols cfg /= SinOnly]
      ++ ["$\\sin\\theta$" | cfgCols cfg /= CosOnly]
      ++ ["$\\tan\\theta$" | cfgTan cfg]

latexRow :: Config -> Row -> String
latexRow _cfg r = intercalate " & " (map wrapMath fields) ++ " \\\\"
  where
    fields =
      [rowAngle r]
        ++ maybe [] (\rr -> [latexWrap (rrExpr rr)]) (rowCos r)
        ++ maybe [] (\rr -> [latexWrap (rrExpr rr)]) (rowSin r)
        ++ maybe [] (\rr -> [latexWrap (rrExpr rr)]) (rowTan r)
    wrapMath s = "$" ++ s ++ "$"

latexPreamble :: String
latexPreamble =
  unlines
    [ "\\documentclass[11pt]{article}",
      "\\usepackage[a4paper,margin=1in]{geometry}",
      "\\usepackage{amsmath,amssymb}",
      "\\usepackage{longtable}",
      "\\usepackage{booktabs}",
      "\\title{Exact Trigonometric Values}",
      "\\author{Generated by \\texttt{surd}}",
      "\\date{}",
      "\\begin{document}",
      "\\maketitle",
      ""
    ]

-- --------------------------------------------------------------------------
-- Text output
-- --------------------------------------------------------------------------

textTable :: Config -> [Row] -> String
textTable cfg rows =
  let hdr = textHeader cfg
      sep = map (const '-') hdr
      body = unlines $ map (textRow cfg) rows
   in unlines [hdr, sep] ++ body

textHeader :: Config -> String
textHeader cfg =
  padCols
    cfg
    ["θ"]
    (["cos θ" | cfgCols cfg /= SinOnly])
    (["sin θ" | cfgCols cfg /= CosOnly])
    (["tan θ" | cfgTan cfg])

textRow :: Config -> Row -> String
textRow _cfg r =
  intercalate "  │  " $
    [rowAngle r]
      ++ maybe [] (\rr -> [wrapExpr (rrExpr rr)]) (rowCos r)
      ++ maybe [] (\rr -> [wrapExpr (rrExpr rr)]) (rowSin r)
      ++ maybe [] (\rr -> [wrapExpr (rrExpr rr)]) (rowTan r)

padCols :: Config -> [String] -> [String] -> [String] -> [String] -> String
padCols _ a b c d = intercalate "  │  " (a ++ b ++ c ++ d)

-- | Wrap a long LaTeX math expression using aligned environment.
-- Breaks at top-level + or - (brace depth 0), placing continuation
-- terms on new lines.  Short expressions pass through unchanged.
latexWrap :: String -> String
latexWrap s
  | length s <= 120 = s
  | otherwise =
      let parts = splitLatexTerms s
       in case parts of
            [] -> s
            [_] -> s
            (first : rest) ->
              "\\begin{aligned} & "
                ++ first
                ++ concatMap (" \\\\\n& " ++) rest
                ++ "\\end{aligned}"

-- | Split a LaTeX expression into top-level terms at + and - boundaries.
-- Tracks both brace depth and \\left/\\right delimiter depth to avoid
-- splitting inside \\sqrt{...}, \\frac{...}{...}, or \\left(...)\\right).
splitLatexTerms :: String -> [String]
splitLatexTerms = finish . go (0 :: Int) (0 :: Int) (0 :: Int) ""
  where
    -- go braceDepth delimDepth col acc str
    go _ _ _ acc [] = [reverse acc]
    go bd dd col acc ('{' : cs) = go (bd + 1) dd (col + 1) ('{' : acc) cs
    go bd dd col acc ('}' : cs) = go (max 0 (bd - 1)) dd (col + 1) ('}' : acc) cs
    go bd dd col acc ('\\' : cs)
      | Just rest <- stripPrefix "left" cs =
          go bd (dd + 1) (col + 5) ('t' : 'f' : 'e' : 'l' : '\\' : acc) rest
      | Just rest <- stripPrefix "right" cs =
          go bd (max 0 (dd - 1)) (col + 6) ('t' : 'h' : 'g' : 'i' : 'r' : '\\' : acc) rest
      | otherwise = go bd dd (col + 1) ('\\' : acc) cs
    go bd dd col acc (c : cs)
      -- At depth 0 (both braces and delimiters), past column 60,
      -- split before " + " or " - "
      | bd == 0,
        dd == 0,
        col >= 60,
        c == ' ',
        Just (op, rest) <- matchOp cs =
          reverse acc : go 0 0 (length op) (reverse op) rest
      | otherwise = go bd dd (col + 1) (c : acc) cs

    matchOp ('+' : ' ' : rest) = Just ("+ ", rest)
    matchOp ('-' : ' ' : rest) = Just ("- ", rest)
    matchOp _ = Nothing

    finish = filter (not . null)

    stripPrefix [] s = Just s
    stripPrefix (p : ps) (c : cs) | p == c = stripPrefix ps cs
    stripPrefix _ _ = Nothing

-- | Wrap a long expression at ' + ' or ' - ' boundaries.
-- Only breaks at low nesting depth (≤1 paren deep) to avoid splitting
-- subexpressions like ∛(1 + 3·√3) across lines.
-- Continuation lines are indented by 4 spaces.
wrapExpr :: String -> String
wrapExpr s
  | length s <= 80 = s
  | otherwise = go (0 :: Int) (0 :: Int) s
  where
    -- go col depth str
    go _ _ [] = []
    go col d (c : cs)
      | c == '(' = c : go (col + 1) (d + 1) cs
      | c == ')' = c : go (col + 1) (max 0 (d - 1)) cs
      | col >= 80,
        d == 0,
        c == ' ',
        Just rest <- stripPlus cs =
          "\n    + " ++ go 6 d rest
      | col >= 80,
        d == 0,
        c == ' ',
        Just rest <- stripMinus cs =
          "\n    - " ++ go 6 d rest
      | otherwise = c : go (col + 1) d cs
    stripPlus ('+' : ' ' : rest) = Just rest
    stripPlus _ = Nothing
    stripMinus ('-' : ' ' : rest) = Just rest
    stripMinus _ = Nothing

-- | Insert @%\\n@ (TeX comment + newline) to keep source lines under
-- TeX's @buf_size@ limit.  Breaks only after @}@, @)@, or spaces to
-- avoid splitting TeX commands.  Has no effect on rendered output.
breakTexLines :: Int -> String -> String
breakTexLines maxLen = go 0
  where
    go _ [] = []
    go col (c : cs)
      | col >= maxLen, safe c = c : "%\n" ++ go 0 cs
      | otherwise = c : go (col + 1) cs
    safe '}' = True
    safe ')' = True
    safe ' ' = True
    safe _ = False

-- --------------------------------------------------------------------------
-- Main
-- --------------------------------------------------------------------------

usage :: String
usage =
  unlines
    [ "Usage: surd-trig-table [OPTIONS] SPEC [SPEC ...]",
      "",
      "SPEC is one of:",
      "  N          tabulate cos/sin at kπ/N for k = 0..N",
      "  p/q        single angle pπ/q",
      "  N..M       tabulate for each denominator q in N..M",
      "",
      "Options:",
      "  --format=latex     LaTeX longtable (default)",
      "  --format=text      plain text (Unicode)",
      "  --standalone       complete LaTeX document (default)",
      "  --no-standalone    table fragment only",
      "  --cos-only         omit sine column",
      "  --sin-only         omit cosine column",
      "  --tan              include tangent column",
      "  --force-radical    render radical formulas without simplification limits"
    ]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      hPutStrLn stderr ""
      hPutStrLn stderr usage
      exitFailure
    Right (cfg, specs) -> do
      let angles = concatMap specAngles specs
          rows = map (computeRow cfg) angles
          output = case cfgFormat cfg of
            LaTeX -> latexTable cfg rows
            Text -> textTable cfg rows
      putStr output
