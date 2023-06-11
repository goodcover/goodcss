package web.css

import eu.timepit.refined.api.{RefType, Refined, Validate}
import eu.timepit.refined.macros.RefineMacro

trait CssImplicits {
  import CssSyntax._

  implicit def cssToCssOps(css: Css): CssOps = new CssOps(css)

  implicit def ruleSyntaxForProp(name: CssProp): CssRuleValueSyntax = new CssRuleValueSyntax(name.propName)

  implicit def ruleSyntaxForExprVar(name: CssExprVar): CssRuleExprSyntax =
    new CssRuleExprSyntax(name.varName)

  implicit def ruleSyntaxForValueVar(name: CssValueVar): CssRuleValueSyntax =
    new CssRuleValueSyntax(name.varName)

  implicit def ruleSyntaxForMetaProp(name: CssMetaProp): CssRulePropSyntax = new CssRulePropSyntax(name.propName)

  implicit def intScalarOps(n: Int): CssNumberScalarOps       = new CssNumberScalarOps(n.toDouble)
  implicit def doubleScalarOps(n: Double): CssNumberScalarOps = new CssNumberScalarOps(n)
  implicit def longScalarOps(n: Long): CssNumberScalarOps     = new CssNumberScalarOps(n.toDouble)

  implicit def cssInterpolators(sc: StringContext): CssInterpolators = new CssInterpolators(sc)

  // Copied here from eu.timepit.refined.auto.autoRefineV so hsl(x, y, z) just works.
  implicit def autoRefineV[T, P](t: T)(implicit rt: RefType[Refined], v: Validate[T, P]): Refined[T, P] =
    macro RefineMacro.impl[Refined, T, P]
}
