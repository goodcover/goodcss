package web

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric
import io.estatico.newtype.macros.newsubtype

package object css extends CssFacade with CssImplicits with CssKeywords with CssProps {

  type CssDim = CssRhs[CssExpr]

  type Hue = Double

  type BoundedPercent = Double Refined numeric.Interval.Closed[0.0, 100.0]
  type BoundedFloat   = Double Refined numeric.Interval.Closed[0.0, 1.0]
  type BoundedInt     = Int Refined numeric.Interval.Closed[0, 255]

  @inline implicit def toPrinterOps[A: CssPrinter](target: A): CssPrinter.Ops[A] = new CssPrinter.Ops[A](target)

  // Why does CssSelector.fromClassName not get selected?
  @inline implicit def classNameToSelector(name: ClassName): CssSelector = CssSelector.fromClassName(name)

  @newsubtype case class CssKeywordAndExpr(kw: CssKeyword) {
    def expr: CssExpr = CssExpr.Unsafe(kw.keyword)
  }

  object CssKeywordAndExpr {
    implicit def toExpr(kw: CssKeywordAndExpr): CssExpr = kw.expr
  }
}
