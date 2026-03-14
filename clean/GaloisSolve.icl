implementation module GaloisSolve

import StdEnv
import RadExpr, Rational, Poly
import Identify, RadicalTower, TransitiveGroup

solveInRadicals :: !(Poly Rational) -> ?([RadExpr Rational])
solveInRadicals f
    = case identifyAndSolve f of
        ?None -> ?None
        ?Just (_, roots) -> ?Just roots

identifyAndSolve :: !(Poly Rational) -> ?({#Char}, [RadExpr Rational])
identifyAndSolve f
    | degree f <= 4 = ?None
    = case identifyGaloisGroup f of
        ?None -> ?None
        ?Just gr
            | not gr.grGroup.tgSolvable -> ?None
            -> case solveViaTowerN gr f of
                ?None -> ?None
                ?Just roots -> ?Just (gr.grGroup.tgName, roots)
