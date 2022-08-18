package web.css

import slinky.web.{html => <}
import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.macros.RefineMacro

trait CssImplicits {
  import CssSyntax._

  implicit def cssToCssOps(css: Css): CssOps = new CssOps(css)

  implicit def ruleSyntaxProp(name: CssProp): CssRuleSyntax = new CssRuleSyntax(name)

  implicit def ruleSyntaxExprVar(name: CssExprVar): CssExprVarRuleSyntax =
    new CssExprVarRuleSyntax(name)

  implicit def ruleSyntaxValueVar(name: CssValueVar): CssValueVarRuleSyntax =
    new CssValueVarRuleSyntax(name)

  implicit def ruleSyntaxMetaProp(name: CssMetaProp): CssMetaRuleSyntax = new CssMetaRuleSyntax(name)

  implicit def intScalarOps(n: Int)       = new CssNumberScalarOps(n.toDouble)
  implicit def doubleScalarOps(n: Double) = new CssNumberScalarOps(n)
  implicit def longScalarOps(n: Long)     = new CssNumberScalarOps(n.toDouble)

  implicit def cssInterpolators(sc: StringContext) = new CssInterpolators(sc)

  implicit def richClassName(attr: <.className.type) = new RichClassName(attr)
  implicit def richStyle(attr: <.style.type)         = new RichStyle(attr)

  // Copied here from eu.timepit.refined.auto.autoRefineV so hsl(x, y, z) just works.
  implicit def autoRefineV[T, P](t: T)(implicit rt: RefType[Refined], v: Validate[T, P]): Refined[T, P] =
    macro RefineMacro.impl[Refined, T, P]
}
