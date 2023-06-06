package web.css

final class CssInterpolators(val sc: StringContext) extends AnyVal {

  /** Quoted CSS string */
  def q(ss: String*): CssQuoted = CssQuoted(sc.s(ss: _*))

  /** CSS keyword */
  def kw(ss: String*): CssKeyword = CssKeyword(sc.s(ss: _*))

  /** CSS property name */
  def prop(ss: String*): CssProp = CssProp(sc.s(ss: _*))

  /** CSS selector */
  def sel(ss: CssSelector*): CssSelector = CssSelector(sc.s(ss.map(_.selector): _*))

  /** CSS class name */
  def cn(ss: String*): ClassName = ClassName(sc.s(ss: _*))

  /** CSS class name with "gc-" prefix */
  def gc(ss: String*): ClassName = web.css.gc(sc.s(ss: _*))

  /** CSS class name with "gcn-" prefix */
  def gcn(ss: String*): ClassName = web.css.gcn(sc.s(ss: _*))
}
