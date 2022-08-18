package web.css

private[css] object CssSyntax {

  final class CssOps private[css] (val css: Css) extends AnyVal {
    def cn: ClassName = GoodMotion.css(css.print)
  }

  final class CssRuleValueSyntax private[css] (val name: String) extends AnyVal {
    def :-(rhs: CssRhs[CssValue]): CssRule = CssRule(name, rhs)

    def ?-[A](rhs: Option[A])(implicit ev: A => CssRhs[CssValue]): CssRule =
      CssRule(name, rhs.fold(CssRhs.empty)(ev))
  }

  final class CssRulePropSyntax private[css] (val name: String) extends AnyVal {
    def :-(prop: CssProp): CssRule = CssRule(name, CssRhs.Value(CssKeyword(prop.propName)))

    def ?-(prop: Option[CssProp]): CssRule =
      CssRule(name, prop.fold(CssRhs.empty)(p => CssRhs.Value(CssKeyword(p.propName))))
  }

  final class CssRuleExprSyntax private[css] (val name: String) extends AnyVal {
    def :-(rhs: CssRhs[CssExpr]): CssRule = CssRule(name, rhs)

    def ?-[A](rhs: Option[A])(implicit ev: A => CssRhs[CssExpr]): CssRule =
      CssRule(name, rhs.fold(CssRhs.empty)(ev))
  }
}
