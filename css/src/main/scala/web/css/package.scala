package web

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric

package object css extends CssFacade with CssImplicits with CssKeywords with CssProps {

  type CssDim = CssRhs[CssExpr]

  type Hue = Double

  type BoundedPercent = Double Refined numeric.Interval.Closed[0.0, 100.0]
  type BoundedFloat   = Double Refined numeric.Interval.Closed[0.0, 1.0]
  type BoundedInt     = Int Refined numeric.Interval.Closed[0, 255]

  @inline implicit def toPrinterOps[A: CssPrinter](target: A): CssPrinter.Ops[A] = new CssPrinter.Ops[A](target)
}
