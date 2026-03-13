package surd

/** Subresultant PRS (Polynomial Remainder Sequence) and resultant computation.
  *
  * The subresultant PRS avoids coefficient explosion that occurs with
  * naive Euclidean PRS over non-field coefficient rings (e.g., Z[x]).
  * Over Q (a field), this is equivalent to the standard Euclidean algorithm
  * but provides the resultant as a byproduct.
  */
object Resultant:

  /** Compute the resultant of two polynomials over a field.
    * res(f, g) = product of g(alpha_i) where alpha_i are roots of f,
    * times the leading coefficient of f raised to deg(g).
    *
    * Uses the subresultant PRS for numerical stability.
    */
  def resultant[K](f: Poly[K], g: Poly[K])(using fld: Field[K]): K =
    if f.isZero || g.isZero then fld.zero
    else
      val m = f.degree
      val n = g.degree
      if m == 0 then pow(f.coeffs(0), n)(using fld)
      else if n == 0 then pow(g.coeffs(0), m)(using fld)
      else subresultantPRS(f, g)

  /** Subresultant PRS resultant computation. */
  private def subresultantPRS[K](f: Poly[K], g: Poly[K])(using fld: Field[K]): K =
    var a = f
    var b = g
    var s = fld.one
    var sign = 1

    while !b.isZero do
      val da = a.degree
      val db = b.degree
      if da < db then
        val tmp = a; a = b; b = tmp
        if da % 2 == 1 && db % 2 == 1 then sign = -sign

      val la = a.leadCoeff.get
      val (_, rem) = Poly.divMod(a, b)
      if rem.isZero then
        if b.degree == 0 then
          val lb = b.leadCoeff.get
          s = fld.mul(s, pow(lb, a.degree - b.degree)(using fld))
          return if sign < 0 then fld.negate(s) else s
        else
          return fld.zero

      val delta = a.degree - b.degree
      s = fld.mul(s, pow(b.leadCoeff.get, delta)(using fld))
      if delta % 2 == 0 && a.degree % 2 == 1 && b.degree % 2 == 1 then
        sign = -sign
      a = b
      b = rem

    fld.zero

  /** Power function for field elements. */
  private def pow[K](base: K, exp: Int)(using fld: Field[K]): K =
    if exp == 0 then fld.one
    else if exp == 1 then base
    else
      val half = pow(base, exp / 2)
      val sq = fld.mul(half, half)
      if exp % 2 == 0 then sq else fld.mul(sq, base)

  /** Compute the resultant using the Sylvester matrix determinant (reference implementation).
    * Less efficient but useful for testing.
    */
  def resultantSylvester[K](f: Poly[K], g: Poly[K])(using fld: Field[K]): K =
    val m = f.degree
    val n = g.degree
    if m < 0 || n < 0 then return fld.zero
    val sz = m + n
    if sz == 0 then return fld.one

    // Build Sylvester matrix as Vector[Vector[K]]
    val initRow = Vector.fill(sz)(fld.zero)
    var mat = Vector.fill(sz)(initRow)

    // n rows from f
    for i <- 0 until n do
      for j <- 0 to m do
        mat = mat.updated(i, mat(i).updated(i + j, f.coeffs(m - j)))

    // m rows from g
    for i <- 0 until m do
      for j <- 0 to n do
        mat = mat.updated(n + i, mat(n + i).updated(i + j, g.coeffs(n - j)))

    // Gaussian elimination
    var det = fld.one
    for col <- 0 until sz do
      // Find pivot
      var pivotRow = -1
      for row <- col until sz do
        if pivotRow < 0 && mat(row)(col) != fld.zero then pivotRow = row
      if pivotRow < 0 then return fld.zero
      if pivotRow != col then
        val tmp = mat(col)
        mat = mat.updated(col, mat(pivotRow)).updated(pivotRow, tmp)
        det = fld.negate(det)
      det = fld.mul(det, mat(col)(col))
      val pivotInv = fld.inv(mat(col)(col))
      for row <- col + 1 until sz do
        val factor = fld.mul(mat(row)(col), pivotInv)
        var newRow = mat(row)
        for j <- col until sz do
          newRow = newRow.updated(j, fld.sub(newRow(j), fld.mul(factor, mat(col)(j))))
        mat = mat.updated(row, newRow)
    det
