package web.css

import cats.data.NonEmptySeq

trait CssFacade extends CssKeyframes {

  /** Stylis parent selector */
  val & : CssSelector = CssSelector("&")

  /** Empty CSS selector
    *
    * Use when you need an operator or other selector without rhs
    *
    * Example:
    *     sel > cn"ant-foo",
    *     sel.lastChild
    *
    * will produce:
    *     > .ant-foo,
    *     :lastChild
    */
  val sel: CssSelector = CssSelector.empty

  /** Create top-level CSS scope
    *
    * (Top-level wrt to whatever the CSS ends up bound to)
    */
  def css(css: Css*): Css = CssScope(Seq.empty, css)

  /** Create CSS scoped by query */
  def css(query: MediaQuery)(body: Css*): Css = CssScope(Seq(query), body)

  /** Create CSS scoped by selectors */
  def css(sel: CssSelector*)(body: Css*): Css = CssScope(sel, body)

  /** Create an overriding CSS scope */
  def clobber(body: Css*): Css = CssScope(Seq(CssSelector("&&&&")), body)

  /** Create an overriding CSS scope by repeating the supplied selector */
  def clobber(selector: String)(body: Css*): Css = CssScope(Seq(CssSelector(selector * 4)), body)

  /** Create a class name with "gcn-" prefix */
  def gcn(name: String): ClassName = ClassName(name).map("gcn-" + _)

  /** Create a CSS variable for expressions */
  def exprVar(name: String): CssExprVar = CssExprVar(name)

  /** Create a CSS variable for any CSS value */
  def valueVar(name: String): CssValueVar = CssValueVar(name)

  def spaced(xs: CssValue*): CssValue             = CssDelimited(xs)
  def delim(sep: String)(xs: CssValue*): CssValue = CssDelimited(xs, sep)

  @inline def call(fn: String, exprs: CssExpr*): CssExpr = CssExpr.Call(fn, exprs)

  def min(exprs: CssExpr*): CssExpr                      = call("min", exprs: _*)
  def max(exprs: CssExpr*): CssExpr                      = call("max", exprs: _*)
  def minmax(min: CssExpr, max: CssExpr): CssExpr        = call("minmax", min, max)
  def clamp(x: CssExpr, y: CssExpr, z: CssExpr): CssExpr = call("clamp", x, y, z)

  def counter(c: CssKeyword): CssValue                     = CssBuiltin("counter", Seq(c.keyword))
  def counters(c: CssKeyword, separator: String): CssValue = CssBuiltin("counter", Seq(c.keyword, separator))

  def url(s: String): CssValue = CssBuiltin("url", Seq(s))

  def hsl(h: Hue, s: BoundedPercent, l: BoundedPercent): CssHsl                  = CssHsl(h, s, l)
  def hsl(h: Hue, s: BoundedPercent, l: BoundedPercent, a: BoundedFloat): CssHsl = CssHsl(h, s, l, a)

  def rgb(r: BoundedInt, g: BoundedInt, b: BoundedInt): CssRgb                  = CssRgb(r, g, b)
  def rgb(r: BoundedInt, g: BoundedInt, b: BoundedInt, a: BoundedFloat): CssRgb = CssRgb(r, g, b, a)

  def blackToColor(rgb: CssRgb): CssValue = CssSvgColorFilter.blackToRgb(rgb)
  def blackToColor(hsl: CssHsl): CssValue = CssSvgColorFilter.blackToHsl(hsl)

  def scale(x: Double): CssExpr            = call("scale", x.n)
  def scale(x: Double, y: Double): CssExpr = call("scale", x.n, y.n)

  def rotate(r: CssExpr): CssExpr = call("rotate", r)

  def translate(x: CssExpr, y: CssExpr): CssExpr = call("translate", x, y)
  def translateX(x: CssExpr): CssExpr            = call("translateX", x)
  def translateY(y: CssExpr): CssExpr            = call("translateY", y)

  def byFold[A, Q](x: (Q, A), xs: (Q, A)*)(implicit conv: CssMediaConv[A, Q]): conv.T = conv(NonEmptySeq(x, xs))

  def stackingContext(zIndex: Int = 0, position: CssKeyword = relative): Css =
    css(prop.zIndex :- zIndex.n, prop.position :- position)

  @deprecated("Use sel\"\" interpolator or CssSelector methods", "v1.8.7")
  def css(sel: String)(body: Css*): Css = CssScope(Seq(CssSelector(sel)), body)

  @deprecated("Use css(...).cn instead", "v1.8.7")
  def cn(body: Css*): ClassName = CssScope(Seq.empty, body).cn

  @deprecated("Use css(..)(...).cn instead", "v1.8.7")
  def cn(query: MediaQuery)(body: Css*): ClassName = CssScope(Seq(query), body).cn

  @deprecated("Use css(...)(...).cn instead", "v1.8.7")
  def cn(sel: CssSelector*)(body: Css*): ClassName = CssScope(sel, body).cn

  @deprecated("Use css(..)(...).cn instead", "v1.8.7")
  def cn(sel: String)(body: Css*): ClassName = CssScope(Seq(CssSelector(sel)), body).cn

}
