package web

import io.estatico.newtype.macros.newsubtype
import io.estatico.newtype.ops.toCoercibleIdOps

package object css extends CssFacade with CssImplicits with CssKeywords with CssProps {

  type CssDim = CssRhs[CssExpr]

  type Hue = Double

  // 0-100.0 inclusive
  type BoundedPercent = Double
  // 0-1.0 inclusive
  type BoundedFloat   = Double
  // 0-255 inclusive
  type BoundedInt     = Int

  @inline implicit def toPrinterOps[A: CssPrinter](target: A): CssPrinter.Ops[A] = new CssPrinter.Ops[A](target)

  // Why does CssSelector.fromClassName not get selected?
  @inline implicit def classNameToSelector(name: ClassName): CssSelector = CssSelector.fromClassName(name)

  @newsubtype class Align(val value: CssKeyword)

  object Align {

    type T = Align

    type Choose = Align.type => T

    implicit def resolveChoice(choose: Choose): T = choose(Align)

    final implicit class ChooseOps(val choose: Choose) {
      def choice: T = choose(Align)
    }

    val normal: T    = kw.normal.coerce
    val flexStart: T = kw.flexStart.coerce
    val flexEnd: T   = kw.flexEnd.coerce
    val start: T     = kw.start.coerce
    val `end`: T     = kw.end.coerce
    val center: T    = kw.center.coerce
    val selfStart: T = kw.selfStart.coerce
    val selfEnd: T   = kw.selfEnd.coerce
    val baseline: T  = kw.baseline.coerce
    val stretch: T   = kw.stretch.coerce
  }
}
