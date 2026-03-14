package surd

opaque type SquareFreePoly = Poly[Rational]
object SquareFreePoly:
  def apply(p: Poly[Rational]): SquareFreePoly = p
  extension (p: SquareFreePoly)
    def underlying: Poly[Rational] = p

opaque type MonicPoly = Poly[Rational]
object MonicPoly:
  def apply(p: Poly[Rational]): MonicPoly = p
  extension (p: MonicPoly)
    def underlying: Poly[Rational] = p
