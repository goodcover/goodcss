package web.css

import scala.scalajs.js
import cats.data.NonEmptySeq
import web.css.MediaPartition._

trait CssFacade extends CssKeyframes {
  def css(css: Css*): Css                      = CssScope(None, css)
  def scope(sel: CssSelector)(body: Css*): Css = CssScope(Some(sel), body)
  def cn(body: Css*): ClassName                = css(body: _*).cn
  def named(name: String): ClassName           = ClassName(s"gcn-$name")

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

  def boxForeground(h: js.UndefOr[Int]): CssHsl = h.fold(CssHsl(0, 0, 35))(CssHsl(_, 100, 25))
  def boxBackground(h: js.UndefOr[Int]): CssHsl = h.fold(CssHsl(0, 0, 95))(CssHsl(_, 100, 95))
  def boxOutline(h: js.UndefOr[Int]): CssHsl    = h.fold(CssHsl(0, 0, 80))(CssHsl(_, 70, 70))

  def boxColors(h: js.UndefOr[Int]): Css =
    css(color :- boxForeground(h), backgroundColor :- boxBackground(h), borderColor :- boxOutline(h))

  def byFold[A, Q](x: (Q, A), xs: (Q, A)*)(implicit conv: CssMediaConv[A, Q]): conv.T = conv(NonEmptySeq(x, xs))

  def byDevice[A](phone: A, tablet: A, desktop: A)(implicit
    pmap: (ByDevice => A) => NonEmptySeq[(MediaProfile, A)],
    conv: CssMediaConv[A, MediaProfile]
  ): conv.T = conv(pmap(ByDevice.fold(phone, tablet, desktop)))

  def byPhone[A](phone: A, notPhone: A)(implicit
    pmap: (ByDevice => A) => NonEmptySeq[(MediaProfile, A)],
    conv: CssMediaConv[A, MediaProfile]
  ): conv.T =
    byDevice(phone, notPhone, notPhone)

  def byDesktop[A](mobile: A, desktop: A)(implicit
    pmap: (ByDevice => A) => NonEmptySeq[(MediaProfile, A)],
    conv: CssMediaConv[A, MediaProfile]
  ): conv.T =
    byDevice(mobile, mobile, desktop)

  def byColumn[A](one: A, two: A)(implicit
    pmap: (ByColumn => A) => NonEmptySeq[(MediaProfile, A)],
    conv: CssMediaConv[A, MediaProfile]
  ): conv.T = conv(pmap(ByColumn.fold(one, two)))

  def stackingContext(zIndex: Int = 0, position: CssKeyword = relative): Css =
    css(prop.zIndex :- zIndex.n, prop.position :- position)

  def clobber(css: Css): ClassName = cn(scope("&&&&")(css))
}