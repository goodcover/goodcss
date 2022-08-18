package web.css

import cats.data.NonEmptySeq
import cats.syntax.functor._

/**
  * Clamp values to [[MediaProfile]]s. Uses a combination of CSS's `clamp()` and `calc()` to produce a single
  * CSS value.
  *
  * Use via the `clamp*` functions.
  *
  * Works only with pixel values. An alternative for other scalars is the [[CssExprs.apply]] function which produces
  * multiple CSS values scoped by media query. See [[CssRule]] for how these [[CssExprs]] are printed to CSS.
  */
sealed case class CssClamp(values: NonEmptySeq[(MediaProfile, CssScalar[Px])]) {
  def expr: CssExpr = CssClamp.clampValues(values)

  def map(f: CssScalar[Px] => CssScalar[Px]): CssClamp = CssClamp(values.map { case (p, x) => p -> f(x) })

  def unary_- : CssClamp = CssClamp(values.map(_.map(-_)))
}

object CssClamp {
  @inline implicit def toBinOpOps(x: CssClamp): CssBinOpOps[CssClamp] = new CssBinOpOps[CssClamp](x)

  @inline implicit def toDim(x: CssClamp): CssDim = x.expr

  def breakpoint(width: CssScalar[Px]): CssExpr = {
    val w1          = width - 1.px
    val clampMinMax = clamp(w1, 100.vw, width)
    clampMinMax - w1
  }

  private[css] def clampValues(values: NonEmptySeq[(MediaProfile, CssScalar[Px])]): CssExpr = values match {
    case NonEmptySeq((_, v), Seq()) => v
    case NonEmptySeq((_, a), tail)  =>
      values.toSeq
        .zip(tail)
        .map {
          case ((_, x), (p, y)) =>
            val break = breakpoint(p.minWidth)
            (y - x).n * break
        }
        .foldLeft[CssExpr](a)(_ #+ _)
  }
}
