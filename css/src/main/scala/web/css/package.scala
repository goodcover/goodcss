package web

import scala.scalajs.js.|
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric

package object css extends CssFacade with CssImplicits with CssKeywords with CssProps {

  type CssDim = CssRhs[CssExpr]

  type CssSize = CssExpr | CssKeyword

  type BoundedPercent = Int Refined numeric.Interval.Closed[0, 100]
  type BoundedFloat   = Double Refined numeric.Interval.Closed[0.0, 1.0]
  type BoundedInt     = Int Refined numeric.Interval.Closed[0, 255]

}
