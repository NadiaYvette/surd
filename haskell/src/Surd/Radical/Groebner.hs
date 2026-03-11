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
-- The reduction is incremental: 'GroebnerContext' can be extended with
-- new atoms and relations as they are discovered (e.g., at each layer
-- of a Gauss period descent).
module Surd.Radical.Groebner
  ( -- * Context
    GroebnerContext
  , emptyContext
  , contextFromAtoms
  , extendContext
    -- * Reduction
  , reduceNormExpr
  , reduceRadExpr
  , reduceRadExprWithCtx
    -- * Queries
  , contextAtoms
  , contextBasis
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Math.Polynomial.Multivariate (Var(..), Mono(..), MPoly(..), varPoly, constPoly,
                                      zeroPoly, isZero, mulPoly, addPoly, subPoly, scalePoly)
import Math.Polynomial.Groebner (GroebnerBasis, MonoOrd, grevlex, groebnerBasis,
                                  extendBasis, reduce, gbPolys)
import Surd.Radical.NormalForm (Atom(..), Monomial(..), NormExpr(..), toNormExpr, fromNormExpr,
                                 normLit)
import Surd.Types (RadExpr(..))

-- ---------------------------------------------------------------------------
-- Context: atom ↔ variable mapping + Gröbner basis of relations
-- ---------------------------------------------------------------------------

-- | A Gröbner context tracks the mapping between radical atoms and
-- polynomial variables, plus the Gröbner basis of their defining relations.
data GroebnerContext = GroebnerContext
  { ctxAtomToVar :: Map Atom Var
    -- ^ Bijection: atom → variable
  , ctxVarToAtom :: Map Var Atom
    -- ^ Inverse: variable → atom
  , ctxNextVar   :: !Int
    -- ^ Next fresh variable index
  , ctxBasis     :: GroebnerBasis Rational
    -- ^ Gröbner basis of the ideal of defining relations
  , ctxOrd       :: MonoOrd
    -- ^ Monomial ordering used for the basis
  }

-- | Create an empty context with the default (grevlex) ordering.
emptyContext :: GroebnerContext
emptyContext = GroebnerContext
  { ctxAtomToVar = Map.empty
  , ctxVarToAtom = Map.empty
  , ctxNextVar   = 0
  , ctxBasis     = groebnerBasis grevlex []
  , ctxOrd       = grevlex
  }

-- | Create a context from a list of atoms, generating their defining
-- relations and computing the initial Gröbner basis.
contextFromAtoms :: [Atom] -> GroebnerContext
contextFromAtoms atoms =
  let ctx0 = emptyContext
      (ctx1, _) = foldl addAtom (ctx0, []) atoms
      rels = concatMap (atomRelations ctx1) atoms
      basis = groebnerBasis (ctxOrd ctx1) rels
  in ctx1 { ctxBasis = basis }

-- | Extend a context with new atoms and their relations.
-- Only recomputes cross-pairs between new and existing generators.
extendContext :: [Atom] -> GroebnerContext -> GroebnerContext
extendContext newAtoms ctx =
  let (ctx', _) = foldl addAtom (ctx, []) newAtoms
      newRels = concatMap (atomRelations ctx') newAtoms
      basis' = extendBasis newRels (ctxBasis ctx)
  in ctx' { ctxBasis = basis' }

-- | Add an atom to the context, assigning it a fresh variable.
-- Returns the updated context and the assigned variable.
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

-- | Generate the defining polynomial relations for an atom.
--
-- * @RatRoot n r@: @x^n - r = 0@ (the atom is the principal nth root of r)
-- * @ImagUnit@: @x^2 + 1 = 0@ (i² = -1)
-- * @NestedRoot n e@: @x^n - poly(e) = 0@ where poly(e) is the polynomial
--   representation of the radicand (which may involve other atoms)
atomRelations :: GroebnerContext -> Atom -> [MPoly Rational]
atomRelations ctx atom =
  case Map.lookup atom (ctxAtomToVar ctx) of
    Nothing -> []
    Just v  ->
      let xv = varPoly v
      in case atom of
        RatRoot n r ->
          -- x^n - r = 0
          [powMPoly xv n `subPoly` constPoly (fromRational r)]

        ImagUnit ->
          -- x^2 + 1 = 0
          [mulPoly xv xv `addPoly` constPoly 1]

        NestedRoot n inner ->
          -- x^n - poly(radicand) = 0
          -- The radicand is a RadExpr; convert it to NormExpr, then to MPoly
          let radicandNE = toNormExpr inner
              radicandPoly = normExprToMPoly ctx radicandNE
          in [powMPoly xv n `subPoly` radicandPoly]

-- ---------------------------------------------------------------------------
-- Conversion: NormExpr ↔ MPoly Rational
-- ---------------------------------------------------------------------------

-- | Convert a NormExpr to an MPoly in the context's variables.
-- Atoms not in the context are silently ignored (their monomials
-- become constant 1).
normExprToMPoly :: GroebnerContext -> NormExpr -> MPoly Rational
normExprToMPoly ctx (NormExpr terms) =
  foldl addPoly zeroPoly
    [ scalePoly coeff (monomialToMPoly ctx mono)
    | (mono, coeff) <- Map.toList terms
    ]

-- | Convert a single Monomial to an MPoly.
monomialToMPoly :: GroebnerContext -> Monomial -> MPoly Rational
monomialToMPoly ctx (Monomial atoms) =
  foldl mulPoly (constPoly 1)
    [ case Map.lookup atom (ctxAtomToVar ctx) of
        Just v  -> powMPoly (varPoly v) e
        Nothing -> constPoly 1  -- Unknown atom treated as 1 (shouldn't happen)
    | (atom, e) <- Map.toList atoms
    ]

-- | Convert an MPoly back to a NormExpr, using the context's variable mapping.
mpolyToNormExpr :: GroebnerContext -> MPoly Rational -> NormExpr
mpolyToNormExpr ctx (MPoly terms) =
  NormExpr $ Map.fromListWith (+)
    [ (monoToMonomial ctx mono, coeff)
    | (mono, coeff) <- Map.toList terms
    , coeff /= 0
    ]

-- | Convert an MPoly Mono to a NormalForm Monomial.
monoToMonomial :: GroebnerContext -> Mono -> Monomial
monoToMonomial ctx (Mono vars) =
  Monomial $ Map.fromList
    [ (atom, e)
    | (v, e) <- Map.toList vars
    , Just atom <- [Map.lookup v (ctxVarToAtom ctx)]
    , e /= 0
    ]

-- ---------------------------------------------------------------------------
-- Reduction
-- ---------------------------------------------------------------------------

-- | Reduce a NormExpr modulo the Gröbner basis of atom relations.
--
-- This may simplify expressions that NormalForm cannot, by exploiting
-- polynomial dependencies between atoms.
reduceNormExpr :: GroebnerContext -> NormExpr -> NormExpr
reduceNormExpr ctx ne =
  let poly = normExprToMPoly ctx ne
      reduced = reduce (ctxBasis ctx) poly
  in if isZero reduced
     then normLit 0
     else mpolyToNormExpr ctx reduced

-- | Reduce a RadExpr via Gröbner basis.
-- Converts to NormExpr, collects atoms, builds context, reduces,
-- and converts back.
reduceRadExpr :: RadExpr Rational -> RadExpr Rational
reduceRadExpr = snd . reduceRadExprWithCtx

-- | Like 'reduceRadExpr' but also returns the 'GroebnerContext',
-- so it can be reused to reduce further expressions over the same atoms
-- without recomputing the Gröbner basis.
reduceRadExprWithCtx :: RadExpr Rational -> (GroebnerContext, RadExpr Rational)
reduceRadExprWithCtx expr =
  let ne = toNormExpr expr
      atoms = collectAtomsNE ne
      ctx = contextFromAtoms (Set.toList atoms)
      reduced = reduceNormExpr ctx ne
  in (ctx, fromNormExpr reduced)

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
-- Helpers
-- ---------------------------------------------------------------------------

-- | Raise an MPoly to a positive integer power.
powMPoly :: (Eq k, Num k) => MPoly k -> Int -> MPoly k
powMPoly _ 0 = constPoly 1
powMPoly p 1 = p
powMPoly p n
  | even n    = let h = powMPoly p (n `div` 2) in mulPoly h h
  | otherwise = mulPoly p (powMPoly p (n - 1))
