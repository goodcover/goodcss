package web.css

import cats.data.NonEmptySeq

trait CssFacade extends CssKeyframes {
  def css(css: Css*): Css               = CssScope(None, css)
  def css(sel: CssSelector)(body: Css*) = CssScope(Some(sel), body)

  def cn(body: Css*): ClassName                   = CssScope(None, body).cn
  def cn(sel: CssSelector)(body: Css*): ClassName = CssScope(Some(sel), body).cn

  def clobber(body: Css*): Css                         = CssScope(Some("&&&&"), body)
  def clobber(selector: String = "&")(body: Css*): Css = CssScope(Some(selector * 4), body)

  def named(name: String): ClassName = ClassName(s"gcn-$name")

  def exprVar(name: String): CssExprVar   = CssExprVar(name)
  def valueVar(name: String): CssValueVar = CssValueVar(name)

  def spaced(xs: CssValue*): CssValue = CssSpaced(xs)

  def sep(sep: String)(xs: CssValue*): CssValue = CssSpaced(xs, sep)

  @inline def call(fn: String, exprs: CssExpr*): CssExpr = CssExpr.Call(fn, exprs)

  def min(exprs: CssExpr*): CssExpr                      = call("min", exprs: _*)
  def max(exprs: CssExpr*): CssExpr                      = call("max", exprs: _*)
  def minmax(min: CssExpr, max: CssExpr): CssExpr        = call("minmax", min, max)
  def clamp(x: CssExpr, y: CssExpr, z: CssExpr): CssExpr = call("clamp", x, y, z)

  def counter(c: CssKeyword): CssValue                     = CssBuiltin("counter", Seq(c.keyword))
  def counters(c: CssKeyword, separator: String): CssValue = CssBuiltin("counter", Seq(c.keyword, separator))

  def url(s: String): CssValue = CssBuiltin("url", Seq(s))

  def hsl(h: Int, s: BoundedPercent, l: BoundedPercent): CssHsl                  = CssHsl(h, s, l)
  def hsl(h: Int, s: BoundedPercent, l: BoundedPercent, a: BoundedFloat): CssHsl = CssHsl(h, s, l, a)

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
}
