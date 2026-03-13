definition module TowerDescent

// Tower-based Gauss period descent (stub).

from RadExpr import :: RadExpr
from Rational import :: Rational

:: TowerResult = { trCos :: !(RadExpr Rational), trSin :: !(RadExpr Rational) }

allPeriodsViaTower :: !Int -> ?(TowerResult)
