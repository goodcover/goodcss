package web.css

final class CssInterpolators(val sc: StringContext) extends AnyVal {
  def q(ss: String*): CssQuoted = CssQuoted(sc.s(ss: _*))

  def kw(ss: String*): CssKeyword = CssKeyword(sc.s(ss: _*))

  def prop(ss: String*): CssProp = CssProp(sc.s(ss: _*))

  def sel(ss: CssSelector*): CssSelector = CssSelector(sc.s(ss.map(_.selector): _*))
}
