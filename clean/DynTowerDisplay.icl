implementation module DynTowerDisplay

import StdEnv
import RadExpr, Rational, Pretty

extractTower :: !{#Char} !(RadExpr Rational) -> {#Char}
extractTower label expr = label +++ " = " +++ pretty expr
