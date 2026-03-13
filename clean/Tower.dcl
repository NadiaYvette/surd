definition module Tower

// Field extension tower (stub).

from Poly import :: Poly
from Rational import :: Rational

:: FieldTower = BaseTower | ExtTower !FieldTower !(Poly Rational)

baseTower :: FieldTower
