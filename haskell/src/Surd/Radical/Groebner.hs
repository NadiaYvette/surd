{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Gröbner basis reduction for radical expressions.
--
-- Converts a 'NormExpr' (polynomial in radical atoms over Q) into an
-- 'MPoly Rational', computes a Gröbner basis of the ideal of defining
-- relations between the atoms, and reduces the expression modulo that ideal.
--
-- This can simplify expressions that NormalForm alone cannot, by exploiting
-- polynomial relations between radical atoms — e.g., when atoms are
-- algebraic conjugates (as in Gauss period expressions for higher-degree
-- cyclotomic fields).
--
-- Three reduction strategies are provided:
--
--   1. __Denominator clearing__ ('reduceCleared'): multiply through by atom
--      powers to eliminate negative exponents, then reduce the polynomial.
--      Simple but may inflate the expression.
--
--   2. __Inverse variables__ ('reduceInverse'): introduce auxiliary variables
--      @a_inv@ with @a · a_inv = 1@ for atoms appearing with negative exponents.
--      Preserves the Laurent structure more faithfully.
--
--   3. __Elimination ordering__ ('reduceElim'): use a block elimination order
--      that prioritises eliminating NestedRoot atoms (complex intermediates),
--      potentially expressing the result in terms of simpler atoms.
--
-- 'reduceRadExprAll' runs all three and returns the shortest result.
module Surd.Radical.Groebner
  ( -- * Context
    GroebnerContext
  , emptyContext
  , contextFromAtoms
  , extendContext
    -- * Reduction strategies
  , Strategy(..)
  , reduceNormExpr
  , reduceRadExpr
  , reduceRadExprWithCtx
  , reduceRadExprAll
    -- * Queries
  , contextAtoms
  , contextBasis
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Math.Polynomial.Multivariate (Var(..), Mono(..), MPoly(..), varPoly, constPoly,
                                      zeroPoly, isZero, mulPoly, addPoly, subPoly, scalePoly,
                                      numTerms)
import Math.Polynomial.Groebner (GroebnerBasis, MonoOrd, grevlex, groebnerBasis,
                                  extendBasis, reduce, gbPolys, elimOrd)
import Surd.Radical.NormalForm (Atom(..), Monomial(..), NormExpr(..), toNormExpr, fromNormExpr,
                                 normLit)
import Surd.Types (RadExpr(..))

-- ---------------------------------------------------------------------------
-- Strategies
-- ---------------------------------------------------------------------------

-- | Reduction strategy for handling Laurent monomials (negative exponents)
-- and monomial ordering.
data Strategy
  = ClearDenominators
    -- ^ Multiply through by atom powers to clear negative exponents.
    -- Uses grevlex ordering.
  | InverseVariables
    -- ^ Introduce inverse variables @a_inv@ with @a · a_inv = 1@.
    -- Negative exponents become positive exponents on the inverse variable.
    -- Uses grevlex ordering.
  | EliminateNested
    -- ^ Use elimination ordering to push NestedRoot atoms out of the result.
    -- Introduces inverse variables for Laurent support.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Context: atom ↔ variable mapping + Gröbner basis of relations
-- ---------------------------------------------------------------------------

-- | A Gröbner context tracks the mapping between radical atoms and
-- polynomial variables, plus the Gröbner basis of their defining relations.
data GroebnerContext = GroebnerContext
  { ctxAtomToVar :: Map Atom Var
    -- ^ Bijection: atom → variable (may include inverse-variable atoms)
  , ctxVarToAtom :: Map Var Atom
    -- ^ Inverse: variable → atom
  , ctxNextVar   :: !Int
    -- ^ Next fresh variable index
  , ctxBasis     :: GroebnerBasis Rational
    -- ^ Gröbner basis of the ideal of defining relations
  , ctxOrd       :: MonoOrd
    -- ^ Monomial ordering used for the basis
  , ctxInverseOf :: Map Atom Atom
    -- ^ For InverseVariables strategy: maps atom → its inverse-atom
  }

-- | Create an empty context with the default (grevlex) ordering.
emptyContext :: GroebnerContext
emptyContext = GroebnerContext
  { ctxAtomToVar = Map.empty
  , ctxVarToAtom = Map.empty
  , ctxNextVar   = 0
  , ctxBasis     = groebnerBasis grevlex []
  , ctxOrd       = grevlex
  , ctxInverseOf = Map.empty
  }

-- | Create a context from a list of atoms using the given strategy.
contextFromAtoms :: Strategy -> [Atom] -> GroebnerContext
contextFromAtoms strat atoms =
  let ctx0 = emptyContext
      (ctx1, _) = foldl addAtom (ctx0, []) atoms
      -- For InverseVariables / EliminateNested, add inverse variables
      ctx2 = case strat of
        ClearDenominators -> ctx1
        _                 -> addInverseVars atoms ctx1
      -- Set the monomial ordering
      ctx3 = case strat of
        EliminateNested ->
          let nestedVars = Set.fromList
                [ v | (NestedRoot _ _, v) <- Map.toList (ctxAtomToVar ctx2) ]
              -- Also eliminate ImagUnit
              iVars = Set.fromList
                [ v | (ImagUnit, v) <- Map.toList (ctxAtomToVar ctx2) ]
              elimVars = Set.union nestedVars iVars
          in ctx2 { ctxOrd = elimOrd elimVars }
        _ -> ctx2
      rels = concatMap (atomRelations ctx3) (Map.keys (ctxAtomToVar ctx3))
      basis = groebnerBasis (ctxOrd ctx3) rels
  in ctx3 { ctxBasis = basis }

-- | Extend a context with new atoms and their relations.
extendContext :: [Atom] -> GroebnerContext -> GroebnerContext
extendContext newAtoms ctx =
  let (ctx', _) = foldl addAtom (ctx, []) newAtoms
      newRels = concatMap (atomRelations ctx') newAtoms
      basis' = extendBasis newRels (ctxBasis ctx)
  in ctx' { ctxBasis = basis' }

-- | Add an atom to the context, assigning it a fresh variable.
addAtom :: (GroebnerContext, [Var]) -> Atom -> (GroebnerContext, [Var])
addAtom (ctx, vars) atom =
  case Map.lookup atom (ctxAtomToVar ctx) of
    Just v  -> (ctx, vars ++ [v])
    Nothing ->
      let v = Var (ctxNextVar ctx)
          ctx' = ctx
            { ctxAtomToVar = Map.insert atom v (ctxAtomToVar ctx)
            , ctxVarToAtom = Map.insert v atom (ctxVarToAtom ctx)
            , ctxNextVar   = ctxNextVar ctx + 1
            }
      in (ctx', vars ++ [v])

-- | For each atom in the list, add an inverse-variable atom and record
-- the @a · a_inv = 1@ relation (generated later by 'atomRelations').
addInverseVars :: [Atom] -> GroebnerContext -> GroebnerContext
addInverseVars atoms ctx = foldl addInv ctx atoms
  where
    addInv c atom =
      let invAtom = InverseAtom atom
      in case Map.lookup invAtom (ctxAtomToVar c) of
           Just _  -> c  -- already exists
           Nothing ->
             let v = Var (ctxNextVar c)
             in c { ctxAtomToVar = Map.insert invAtom v (ctxAtomToVar c)
                  , ctxVarToAtom = Map.insert v invAtom (ctxVarToAtom c)
                  , ctxNextVar   = ctxNextVar c + 1
                  , ctxInverseOf = Map.insert atom invAtom (ctxInverseOf c)
                  }

-- | Generate the defining polynomial relations for an atom.
atomRelations :: GroebnerContext -> Atom -> [MPoly Rational]
atomRelations ctx atom =
  case Map.lookup atom (ctxAtomToVar ctx) of
    Nothing -> []
    Just v  ->
      let xv = varPoly v
      in case atom of
        -- InverseAtom must come before NestedRoot (it's encoded as NestedRoot 0)
        InverseAtom origAtom ->
          -- a · a_inv = 1
          case Map.lookup origAtom (ctxAtomToVar ctx) of
            Just origV ->
              [mulPoly (varPoly origV) xv `subPoly` constPoly 1]
            Nothing -> []

        RatRoot n r ->
          [powMPoly xv n `subPoly` constPoly (fromRational r)]

        ImagUnit ->
          [mulPoly xv xv `addPoly` constPoly 1]

        NestedRoot n inner ->
          let radicandNE = toNormExpr inner
              radicandPoly = normExprToMPolyDirect ctx radicandNE
          in [powMPoly xv n `subPoly` radicandPoly]

-- ---------------------------------------------------------------------------
-- Conversion: NormExpr ↔ MPoly Rational
-- ---------------------------------------------------------------------------

-- | Convert a NormExpr to MPoly, clearing denominators for negative exponents.
-- Multiplies through by atom powers to make all exponents non-negative.
normExprToMPolyCleared :: GroebnerContext -> NormExpr -> MPoly Rational
normExprToMPolyCleared ctx (NormExpr terms) =
  let negExps = Map.foldlWithKey' collectNegs Map.empty terms
  in if Map.null negExps
     then normExprToMPolyDirect ctx (NormExpr terms)
     else foldl addPoly zeroPoly
            [ scalePoly coeff (monomialToMPolyShifted ctx negExps mono)
            | (mono, coeff) <- Map.toList terms
            ]
  where
    collectNegs acc (Monomial atoms) _ =
      Map.foldlWithKey' (\a atom e ->
        if e < 0 then Map.insertWith min atom e a else a
        ) acc atoms

-- | Convert a NormExpr to MPoly using inverse variables for negative exponents.
-- @a^{-k}@ becomes @(a_inv)^k@.
normExprToMPolyInverse :: GroebnerContext -> NormExpr -> MPoly Rational
normExprToMPolyInverse ctx (NormExpr terms) =
  foldl addPoly zeroPoly
    [ scalePoly coeff (monomialToMPolyInverse ctx mono)
    | (mono, coeff) <- Map.toList terms
    ]

-- | Direct conversion (assumes no negative exponents in relevant atoms).
-- Used for radicand polynomials inside NestedRoot, which are always
-- non-negative-exponent after NF.
normExprToMPolyDirect :: GroebnerContext -> NormExpr -> MPoly Rational
normExprToMPolyDirect ctx (NormExpr terms) =
  foldl addPoly zeroPoly
    [ scalePoly coeff (monomialToMPolyDirect' ctx mono)
    | (mono, coeff) <- Map.toList terms
    ]

-- | Convert a single Monomial (non-negative exponents only).
monomialToMPolyDirect' :: GroebnerContext -> Monomial -> MPoly Rational
monomialToMPolyDirect' ctx (Monomial atoms) =
  foldl mulPoly (constPoly 1)
    [ case Map.lookup atom (ctxAtomToVar ctx) of
        Just v  | e > 0    -> powMPoly (varPoly v) e
                | otherwise -> constPoly 1
        Nothing -> constPoly 1
    | (atom, e) <- Map.toList atoms
    ]

-- | Convert a Monomial, shifting exponents by clearing amounts.
monomialToMPolyShifted :: GroebnerContext -> Map Atom Int -> Monomial -> MPoly Rational
monomialToMPolyShifted ctx negExps (Monomial atoms) =
  let allAtoms = Map.unionWith (+) atoms (fmap abs negExps)
  in foldl mulPoly (constPoly 1)
    [ case Map.lookup atom (ctxAtomToVar ctx) of
        Just v  | e > 0    -> powMPoly (varPoly v) e
                | otherwise -> constPoly 1
        Nothing -> constPoly 1
    | (atom, e) <- Map.toList allAtoms
    ]

-- | Convert a Monomial using inverse variables for negative exponents.
monomialToMPolyInverse :: GroebnerContext -> Monomial -> MPoly Rational
monomialToMPolyInverse ctx (Monomial atoms) =
  foldl mulPoly (constPoly 1)
    [ if e > 0
      then case Map.lookup atom (ctxAtomToVar ctx) of
             Just v  -> powMPoly (varPoly v) e
             Nothing -> constPoly 1
      else -- Negative exponent: use inverse variable
        case Map.lookup atom (ctxInverseOf ctx) of
          Just invAtom ->
            case Map.lookup invAtom (ctxAtomToVar ctx) of
              Just iv -> powMPoly (varPoly iv) (abs e)
              Nothing -> constPoly 1
          Nothing -> constPoly 1  -- No inverse available, treat as 1
    | (atom, e) <- Map.toList atoms
    ]

-- | Convert an MPoly back to a NormExpr, using the context's variable mapping.
-- Inverse-variable atoms are converted back to negative exponents on the
-- original atom.
mpolyToNormExpr :: GroebnerContext -> MPoly Rational -> NormExpr
mpolyToNormExpr ctx (MPoly terms) =
  NormExpr $ Map.filter (/= 0) $ Map.fromListWith (+)
    [ (monoToMonomial ctx mono, coeff)
    | (mono, coeff) <- Map.toList terms
    , coeff /= 0
    ]

-- | Convert an MPoly Mono to a NormalForm Monomial.
-- InverseAtom variables become negative exponents on the original atom.
monoToMonomial :: GroebnerContext -> Mono -> Monomial
monoToMonomial ctx (Mono vars) =
  Monomial $ Map.filter (/= 0) $ Map.fromListWith (+)
    [ case Map.lookup v (ctxVarToAtom ctx) of
        Just (InverseAtom orig) -> (orig, negate e)
        Just atom               -> (atom, e)
        Nothing                 -> (RatRoot 1 1, 0)  -- shouldn't happen
    | (v, e) <- Map.toList vars
    , e /= 0
    ]

-- ---------------------------------------------------------------------------
-- Reduction
-- ---------------------------------------------------------------------------

-- | Reduce a NormExpr modulo the Gröbner basis using the given strategy.
reduceNormExpr :: Strategy -> GroebnerContext -> NormExpr -> NormExpr
reduceNormExpr strat ctx ne =
  let poly = case strat of
        ClearDenominators -> normExprToMPolyCleared ctx ne
        InverseVariables  -> normExprToMPolyInverse ctx ne
        EliminateNested   -> normExprToMPolyInverse ctx ne
      reduced = reduce (ctxBasis ctx) poly
  in if isZero reduced
     then normLit 0
     else mpolyToNormExpr ctx reduced

-- | Reduce a RadExpr using the default strategy (InverseVariables).
reduceRadExpr :: RadExpr Rational -> RadExpr Rational
reduceRadExpr = snd . reduceRadExprWithCtx

-- | Like 'reduceRadExpr' but also returns the 'GroebnerContext'.
reduceRadExprWithCtx :: RadExpr Rational -> (GroebnerContext, RadExpr Rational)
reduceRadExprWithCtx expr =
  let ne = toNormExpr expr
      atoms = collectAtomsNE ne
      atomList = Set.toList atoms
      ctx = contextFromAtoms InverseVariables atomList
      reduced = reduceNormExpr InverseVariables ctx ne
  in (ctx, fromNormExpr reduced)

-- | Run all three strategies and return the results, sorted by term count
-- (fewest terms first).  Each result includes the strategy name, term count,
-- and the reduced expression.
reduceRadExprAll :: RadExpr Rational -> [(Strategy, Int, RadExpr Rational)]
reduceRadExprAll expr =
  let ne = toNormExpr expr
      atoms = Set.toList (collectAtomsNE ne)
      results = [ tryStrategy strat atoms ne | strat <- [ClearDenominators, InverseVariables, EliminateNested] ]
      sorted = sortByTerms results
  in sorted
  where
    tryStrategy strat atomList ne =
      let ctx = contextFromAtoms strat atomList
          reduced = reduceNormExpr strat ctx ne
          result = fromNormExpr reduced
          terms = numTerms (unNormExpr reduced)
      in (strat, terms, result)

    sortByTerms = sortOn (\(_, n, _) -> n)

    sortOn f xs = map snd $ sortPairs [(f x, x) | x <- xs]
    sortPairs [] = []
    sortPairs (p:ps) =
      let (lt, ge) = partition (\(k, _) -> k < fst p) ps
      in sortPairs lt ++ [p] ++ sortPairs ge

    partition _ [] = ([], [])
    partition p (x:xs) =
      let (ls, rs) = partition p xs
      in if p x then (x:ls, rs) else (ls, x:rs)

    unNormExpr (NormExpr m) = MPoly $ Map.mapKeys monoToFakeMono m
    monoToFakeMono (Monomial atoms) =
      Mono $ Map.fromList [(Var i, e) | ((_, e), i) <- zip (Map.toList atoms) [0..]]

-- | Collect all atoms from a NormExpr, including atoms nested
-- inside NestedRoot radicands (recursively).
collectAtomsNE :: NormExpr -> Set.Set Atom
collectAtomsNE (NormExpr terms) =
  foldMap collectAtomsMono (Map.keys terms)
  where
    collectAtomsMono (Monomial atoms) =
      foldMap collectFromAtom (Map.keys atoms)

    collectFromAtom atom@(NestedRoot _ inner) =
      Set.insert atom (collectAtomsNE (toNormExpr inner))
    collectFromAtom atom = Set.singleton atom

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | All atoms in the context.
contextAtoms :: GroebnerContext -> [Atom]
contextAtoms = Map.keys . ctxAtomToVar

-- | The current Gröbner basis polynomials.
contextBasis :: GroebnerContext -> [MPoly Rational]
contextBasis = gbPolys . ctxBasis

-- ---------------------------------------------------------------------------
-- Auxiliary atom type for inverse variables
-- ---------------------------------------------------------------------------

-- | An inverse-variable atom.  Used internally to represent @1/a@ as a
-- polynomial variable with the relation @a · a_inv = 1@.
--
-- This extends the 'Atom' type; we piggyback on the existing Atom by
-- adding a new constructor.  Since Atom is defined in NormalForm, we
-- use a newtype wrapper approach: InverseAtom is an Atom value that
-- compares distinctly from all other atoms.

-- NOTE: We need InverseAtom to be part of the Atom type.  Since we can't
-- modify NormalForm.hs's Atom type, we use a sentinel encoding:
-- InverseAtom a = NestedRoot 0 (fromNormExpr (normAtomToNE a))
-- where root index 0 is never used by real atoms (always >= 2).

-- Actually, the cleanest approach: use a separate map for inverse variables
-- that bypasses the Atom type entirely.  The ctxAtomToVar map can hold
-- "virtual" atoms that are only meaningful within the Groebner context.

-- For now, we define InverseAtom as a pattern using NestedRoot 0:

pattern InverseAtom :: Atom -> Atom
pattern InverseAtom a <- NestedRoot 0 (inverseAtomDecode -> Just a)
  where InverseAtom a = NestedRoot 0 (inverseAtomEncode a)

inverseAtomEncode :: Atom -> RadExpr Rational
inverseAtomEncode (RatRoot n r) = Root (fromIntegral n) (Lit r)
inverseAtomEncode ImagUnit = Root 2 (Lit (-1))
inverseAtomEncode (NestedRoot n e) = Root (fromIntegral n) e

inverseAtomDecode :: RadExpr Rational -> Maybe Atom
inverseAtomDecode (Root n (Lit r))
  | n == 2 && r == -1 = Just ImagUnit
  | otherwise         = Just (RatRoot (fromIntegral n) r)
inverseAtomDecode (Root n e) = Just (NestedRoot (fromIntegral n) e)
inverseAtomDecode _ = Nothing

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Raise an MPoly to a positive integer power.
powMPoly :: (Eq k, Num k) => MPoly k -> Int -> MPoly k
powMPoly _ 0 = constPoly 1
powMPoly p 1 = p
powMPoly p n
  | even n    = let h = powMPoly p (n `div` 2) in mulPoly h h
  | otherwise = mulPoly p (powMPoly p (n - 1))
