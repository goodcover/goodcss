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

  val clobber: CssSelector = CssSelector("&&&&")

  /** Create top-level CSS scope
    *
    * (Top-level wrt to whatever the CSS ends up bound to)
    */
  def css(css: Css*): Css = CssScope(Seq.empty, css)

  /** Create CSS scoped by query */
  def css(query: MediaQuery)(body: Css*): Css = CssScope(Seq(query), body)

  /** Create CSS scoped by selectors */
  def css(selector: CssSelector, selectors: CssSelector*)(body: Css*): Css =
    CssScope(selector +: selectors, body)

  /** Create CSS scoped by selectors (with auto-complete) */
  def css(selector: CssSelector.Endo, selectors: CssSelector.Endo*)(body: Css*): Css =
    CssScope(selector(sel) +: selectors.map(f => f(sel)), body)

  /** Create a class name with "gcn-" prefix */
  def gcn(name: String): ClassName = ClassName(name).map("gcn-" + _)

  /** Create a CSS variable for expressions */
  def exprVar(name: String): CssExprVar = CssExprVar(name)

  /** Create a CSS variable for any CSS value */
  def valueVar(name: String): CssValueVar = CssValueVar(name)

  def spaced(xs: CssValue*): CssValue                   = CssDelimited(xs)
  def delim(separator: String)(xs: CssValue*): CssValue = CssDelimited(xs, separator = separator)
  def bracket(xs: CssValue*): CssValue                  = CssDelimited(xs, "[", " ", "]")

  @inline def call(fn: String, exprs: CssExpr*): CssExpr = CssExpr.Call(fn, exprs)

  // Functions that may be used anywhere a <dimension>, <percentage>, or <number> is allowed.
  def min(exprs: CssExpr*): CssExpr                      = call("min", exprs: _*)
  def max(exprs: CssExpr*): CssExpr                      = call("max", exprs: _*)
  def clamp(x: CssExpr, y: CssExpr, z: CssExpr): CssExpr = call("clamp", x, y, z)

  // Function that may be used anywhere a <string> is allowed.
  def counter(c: CssKeyword): CssValue                     = CssBuiltin("counter", Seq(c.keyword))
  def counters(c: CssKeyword, separator: String): CssValue = CssBuiltin("counter", Seq(c.keyword, separator))

  // Functions that may be used with CSS Grids.
  def minmax(min: CssValue, max: CssValue): CssBuiltin      = CssBuiltin("minmax", Seq(min.print, max.print))
  def fitContent(value: CssExpr): CssBuiltin                = CssBuiltin("fit-content", Seq(value.print))
  def repeat(count: CssValue, tracks: CssValue): CssBuiltin = CssBuiltin("repeat", Seq(count.print, tracks.print))

  // Function to include a file.
  def url(s: String): CssValue = CssBuiltin("url", Seq(s))

  // Color functions.
  def hsl(h: Hue, s: BoundedPercent, l: BoundedPercent): CssHsl                  = CssHsl(h, s, l)
  def hsl(h: Hue, s: BoundedPercent, l: BoundedPercent, a: BoundedFloat): CssHsl = CssHsl(h, s, l, a)
  def rgb(r: BoundedInt, g: BoundedInt, b: BoundedInt): CssRgb                   = CssRgb(r, g, b)
  // TODO rename this to rgba(...)
  def rgb(r: BoundedInt, g: BoundedInt, b: BoundedInt, a: BoundedFloat): CssRgb  = CssRgb(r, g, b, a)
  def blackToColor(rgb: CssRgb): CssValue                                        = CssSvgColorFilter.blackToRgb(rgb)
  def blackToColor(hsl: CssHsl): CssValue                                        = CssSvgColorFilter.blackToHsl(hsl)

  // <transform-function>s
  def rotate(r: CssExpr): CssValue                = CssBuiltin("rotate", Seq(r.print))
  def scale(x: CssExpr): CssValue                 = CssBuiltin("scale", Seq(x.print))
  def scale(x: CssExpr, y: CssExpr): CssValue     = CssBuiltin("scale", Seq(x.print, y.print))
  def scaleX(x: CssExpr): CssValue                = CssBuiltin("scaleX", Seq(x.print))
  def scaleY(y: CssExpr): CssValue                = CssBuiltin("scaleY", Seq(y.print))
  def translate(x: CssExpr, y: CssExpr): CssValue = CssBuiltin("translate", Seq(x.print, y.print))
  def translateX(x: CssExpr): CssValue            = CssBuiltin("translateX", Seq(x.print))
  def translateY(y: CssExpr): CssValue            = CssBuiltin("translateY", Seq(y.print))

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
