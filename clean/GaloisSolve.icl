implementation module GaloisSolve

import StdEnv
import RadExpr, Rational, Poly
import Identify, RadicalTower

solveInRadicals :: !(Poly Rational) -> ?([RadExpr Rational])
solveInRadicals f
    = case identifyGaloisGroup5 f of
        ?None -> ?None
        ?Just gr -> solveViaTower gr f
