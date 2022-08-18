package web.css

import web.css.CssPrinter.ops._

private[css] object CssSyntax {
  final class CssOps(val css: Css) extends AnyVal {
    def cn: ClassName = GoodMotion.css(css.print)
  }

  final class CssRuleSyntax(val name: CssProp) extends AnyVal {
    def :-(rhs: CssRhs[CssValue]): CssRule = CssRule(name.propName, rhs)
    def :-(rhs: CssValue): CssRule = CssRule(name.propName, CssRhs.Value(rhs))
  }

  final class CssMetaRuleSyntax(val name: CssMetaProp) extends AnyVal {
    def :-(prop: CssProp): CssRule = CssRule(name.propName, CssRhs.Value(CssKeyword(prop.propName)))
  }

  final class CssExprVarRuleSyntax(val name: CssExprVar) extends AnyVal {
    def :-(rhs: CssRhs[CssExpr]): CssRule = CssRule(name.varName, rhs)
    def :-(rhs: CssExpr): CssRule = CssRule(name.varName, CssRhs.Value(rhs))
  }

  final class CssValueVarRuleSyntax(val name: CssValueVar) extends AnyVal {
    def :-(rhs: CssRhs[CssValue]): CssRule = CssRule(name.varName, rhs)
    def :-(rhs: CssValue): CssRule = CssRule(name.varName, CssRhs.Value(rhs))
  }
}
