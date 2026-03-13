// Demo: generate a table of exact trigonometric values.
//
// Prints cos(p*pi/q) and sin(p*pi/q) for various rational multiples
// of pi, using both text and LaTeX rendering.
module surd/demo/trig-table

import surd/rational
import surd/rad-expr
import surd/trig
import surd/pretty
import surd/latex

// List of (p, q) pairs for the trig table.
val angles : list<(int, int)> = [
  (0, 1),   // 0
  (1, 6),   // pi/6
  (1, 4),   // pi/4
  (1, 3),   // pi/3
  (1, 2),   // pi/2
  (2, 3),   // 2pi/3
  (3, 4),   // 3pi/4
  (5, 6),   // 5pi/6
  (1, 1),   // pi
  (1, 5),   // pi/5
  (1, 8),   // pi/8
  (1, 10),  // pi/10
  (1, 12),  // pi/12
  (2, 5),   // 2pi/5
  (3, 8),   // 3pi/8
  (5, 12)   // 5pi/12
]

// Format an angle as a string.
fun show-angle(p : int, q : int) : string
  if p == 0 then "0"
  elif p == 1 && q == 1 then "pi"
  elif p == 1 then "pi/" ++ q.show
  elif q == 1 then p.show ++ "*pi"
  else p.show ++ "*pi/" ++ q.show

// Format an angle as LaTeX.
fun latex-angle(p : int, q : int) : string
  if p == 0 then "0"
  elif p == 1 && q == 1 then "\\pi"
  elif p == 1 then "\\frac{\\pi}{" ++ q.show ++ "}"
  elif q == 1 then p.show ++ "\\pi"
  else "\\frac{" ++ p.show ++ "\\pi}{" ++ q.show ++ "}"

pub fun main() : <console,div,exn> ()
  // Text table
  println(repeat-char('=', 80))
  println("  Exact Trigonometric Values")
  println(repeat-char('=', 80))
  println("")
  println("  Angle            cos                          sin")
  println(repeat-char('-', 80))

  angles.foreach fn(angle)
    val p = angle.fst
    val q = angle.snd
    val angle-str = show-angle(p, q)
    val cos-result = cos-exact(p, q)
    val sin-result = sin-exact(p, q)
    val cos-str = trig-result/pretty(cos-result)
    val sin-str = trig-result/pretty(sin-result)
    println("  " ++ right-pad(angle-str, 18) ++ right-pad(cos-str, 30) ++ sin-str)

  println("")
  println("")

  // LaTeX table
  println("% LaTeX table")
  println("\\begin{tabular}{ccc}")
  println("\\toprule")
  println("$\\theta$ & $\\cos\\theta$ & $\\sin\\theta$ \\\\")
  println("\\midrule")

  angles.foreach fn(angle)
    val p = angle.fst
    val q = angle.snd
    val angle-latex = latex-angle(p, q)
    val cos-result = cos-exact(p, q)
    val sin-result = sin-exact(p, q)
    val cos-latex = trig-result/latex(cos-result)
    val sin-latex = trig-result/latex(sin-result)
    println("$" ++ angle-latex ++ "$ & $" ++ cos-latex ++ "$ & $" ++ sin-latex ++ "$ \\\\")

  println("\\bottomrule")
  println("\\end{tabular}")

// Pad a string with spaces on the right to a given width.
fun right-pad(s : string, w : int) : string
  val n = s.count
  if n >= w then s
  else s ++ repeat-char(' ', w - n)

// Create a string of n copies of a character.
fun repeat-char(c : char, n : int) : string
  list(1, n).map(fn(_) c.string).join("")
