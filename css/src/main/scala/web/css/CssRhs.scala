package web.css

import scala.scalajs.js
import cats.syntax.functor._
import cats.{Eq, Functor, Monoid}
import scala.concurrent.duration._
import eu.timepit.refined.auto._

sealed trait CssRhs[+A] {
  def values: Seq[(MediaQuery, A)]
}

object CssRhs {
  val empty: CssRhs[CssValue] = Values(Nil)

  final case class Value[A](value: A) extends CssRhs[A] {
    override val values: Seq[(MediaQuery, A)] = Seq(MediaQuery.all -> value)
  }

  final case class Values[A](values: Seq[(MediaQuery, A)]) extends CssRhs[A]

  implicit val cssRhsFunctor: Functor[CssRhs] = new Functor[CssRhs] {

    override def map[A, B](fa: CssRhs[A])(f: A => B): CssRhs[B] = fa match {
      case Value(x)   => Value(f(x))
      case Values(xs) => Values(xs.map(_.map(f)))
    }
  }

  @inline implicit def toBinOpOps(x: CssRhs[CssExpr]): CssBinOpOps[CssRhs[CssExpr]] =
    new CssBinOpOps[CssRhs[CssExpr]](x)

  implicit def eq[A]: Eq[CssRhs[A]]         = (x, y) => x.values == y.values
  implicit def monoid[A]: Monoid[CssRhs[A]] = Monoid.instance(Values(Nil), (x, y) => Values(x.values ++ y.values))

  implicit class Ops(val rhs: CssRhs[CssExpr]) extends AnyVal {
    def unary_- : CssRhs[CssExpr] = rhs.map(-_)
  }
}

sealed trait CssValue

object CssValue {

  implicit val printer: CssPrinter[CssValue] = _ match {
    case CssKeyword(k)               => k
    case CssQuoted(s)                => s"'$s'"
    case CssSpaced(xs, sep)          => xs.map(_.print).mkString(sep)
    case CssBuiltin(name, args, sep) => args.mkString(s"$name(", sep, ")")
    case CssValueVar(name)           => s"var($name)"
    case CssHsl(h, s, l, a)          => s"hsl($h $s% $l% / $a)"
    case CssRgb(r, g, b, a)          => s"rgb(${r * 255} ${g * 255} ${b * 255} / $a)"
    case x: CssExpr                  => CssExpr.printer.print(x)
  }

  @inline implicit def toRhs(x: CssValue): CssRhs[CssValue] = CssRhs.Value(x)
}

sealed trait CssSize extends CssValue

final case class CssKeyword(keyword: String) extends CssValue with CssSize

final case class CssQuoted(text: String) extends CssValue

final case class CssSpaced(values: Seq[CssValue], separator: String = " ") extends CssValue

final case class CssBuiltin(name: String, args: Seq[String], separator: String = ", ") extends CssValue

final case class CssValueVar(varName: String) extends CssValue {
  def fallback(orElse: CssValue): CssValue = CssBuiltin("var", Seq(orElse.print))
}

sealed trait CssColor extends CssValue

final case class CssHsl(h: Int, s: BoundedPercent, l: BoundedPercent, a: BoundedFloat = 1.0) extends CssColor

object CssHsl {
  val black       = CssHsl(0, 0, 0)
  val white       = CssHsl(0, 0, 100)
  val red         = CssHsl(0, 100, 25)
  val green       = CssHsl(120, 100, 25)
  val blue        = CssHsl(240, 100, 25)
  val transparent = CssHsl(0, 0, 0, 0.0)
}

final case class CssRgb private (r: Double, g: Double, b: Double, a: Double) extends CssColor {
  def *(x: CssRgb): CssRgb = CssRgb(r = r * x.r, g = g * x.g, b = b * x.b, a = a * x.a)
}

object CssRgb {

  def apply(r: BoundedInt, g: BoundedInt, b: BoundedInt, a: BoundedFloat = 1d): CssRgb =
    new CssRgb(r / 255d, g / 255d, b / 255d, a)

  def fromFloats(r: BoundedFloat, g: BoundedFloat, b: BoundedFloat, a: BoundedFloat = 1d): CssRgb =
    new CssRgb(r, g, b, a)

  val black       = fromFloats(0d, 0d, 0d)
  val white       = fromFloats(1d, 1d, 1d)
  val red         = apply(128, 0, 0)
  val green       = apply(0, 128, 0)
  val blue        = apply(0, 0, 128)
  val transparent = fromFloats(0d, 0d, 0d, 0d)

  def opacity(a: BoundedFloat): CssRgb = fromFloats(1d, 1d, 1d, a)

  // Algorithm from https://www.w3.org/TR/css-color-3/#hsl-color
  def fromHsl(h: Int, s: BoundedPercent, l: BoundedPercent, a: BoundedFloat = 1.0): CssRgb = {
    val hmod = h % 360

    val hue = if (hmod < 0) hmod + 360 else hmod
    val sat = s / 100d
    val lit = l / 100d

    def f(n: Int): Double = {
      val k = (n + hue / 30d) % 12
      val x = sat * js.Math.min(lit, 1 - lit)
      lit - x * js.Math.max(-1, js.Math.min(k - 3, 9 - k, 1))
    }

    new CssRgb(f(0), f(8), f(4), a)
  }

  def fromHsl(hsl: CssHsl): CssRgb = fromHsl(hsl.h, hsl.s, hsl.l, hsl.a)
}

sealed trait CssExpr extends CssValue with CssSize {

  def unary_- : CssExpr = this match {
    case x: CssScalar[_] => -x
    case x               => 0.px - x
  }
}

object CssExpr {
  final case class Op(l: CssExpr, op: CssBinOperator, r: CssExpr) extends CssExpr
  final case class Call(f: String, args: Seq[CssExpr])            extends CssExpr
  final case class Unsafe(exprString: String)                     extends CssExpr

  val printer: CssPrinter[CssExpr] = _ match {
    case x: CssScalar[_]   => print(x)
    case x: CssExpr.Op     => s"calc(${print(x)})"
    case x: CssExpr.Call   => print(x)
    case x: CssExprVar     => print(x)
    case x: CssExpr.Unsafe => s"calc(${print(x)})"
  }

  @inline implicit def toBinOpOps(x: CssExpr): CssBinOpOps[CssExpr] = new CssBinOpOps[CssExpr](x)
  @inline implicit def toRhs(x: CssExpr): CssRhs[CssExpr]           = CssRhs.Value(x)

  private def print(expr: CssExpr): String = expr match {
    case x: CssScalar[_]       => x.print
    case CssExpr.Op(l, op, r)  => s"${bracket(l)} ${op.token} ${bracket(r)}"
    case CssExpr.Call(f, args) => args.map(_.print).mkString(s"$f(", ", ", ")")
    case CssExprVar(name)      => s"var($name)"
    case CssExpr.Unsafe(s)     => s
  }

  private def bracket(expr: CssExpr): String = expr match {
    case x: CssScalar[_] => print(x)
    case x: Op           => s"(${print(x)})"
    case x: Call         => print(x)
    case x: CssExprVar   => print(x)
    case x: Unsafe       => s"(${print(x)})"
  }

}

final case class CssExprVar(varName: String) extends CssExpr {
  def fallback(orElse: CssExpr): CssExpr   = CssExpr.Call("var", Seq(orElse))
  def fallback(orElse: CssValue): CssValue = CssBuiltin("var", Seq(orElse.print))
}

/**
  * CSS scalar value. Runtime value is unitless. The UnitSuffix type class provides the appropriate suffix.
  *
  * [[CssNumberScalarOps]] provides convenient syntax: 1.px, 2.em, 3.percent etc.
  *
  * Use `.n` get a unitless [[CssScalar[Num]]] value.
  */
final case class CssScalar[Q](unitless: Double)(implicit val unit: UnitSuffix[Q]) extends CssExpr {
  def add(r: CssScalar[Q]): CssScalar[Q]        = CssScalar[Q](unitless + r.unitless)
  def subtract(r: CssScalar[Q]): CssScalar[Q]   = CssScalar[Q](unitless - r.unitless)
  def multiply(r: CssScalar[Num]): CssScalar[Q] = CssScalar[Q](unitless * r.unitless)
  def divide(r: CssScalar[Num]): CssScalar[Q]   = CssScalar[Q](unitless / r.unitless)
  def lt(r: CssScalar[Q]): Boolean              = unitless < r.unitless
  def gt(r: CssScalar[Q]): Boolean              = unitless > r.unitless
  def lte(r: CssScalar[Q]): Boolean             = unitless <= r.unitless
  def gte(r: CssScalar[Q]): Boolean             = unitless >= r.unitless

  @inline def <(r: CssScalar[Q]): Boolean  = this lt r
  @inline def >(r: CssScalar[Q]): Boolean  = this gt r
  @inline def <=(r: CssScalar[Q]): Boolean = this lte r
  @inline def >=(r: CssScalar[Q]): Boolean = this gte r

  override def unary_- : CssScalar[Q] = CssScalar[Q](-unitless)

  def n: CssScalar[Num] = CssScalar[Num](unitless)

  def print: String = s"$unitless${unit.unitSuffix}"

  override def toString(): String = print
}

object CssScalar {
  def max[Q: UnitSuffix](xs: CssScalar[Q]*): CssScalar[Q] = CssScalar[Q](js.Math.max(xs.map(_.unitless): _*))
  def min[Q: UnitSuffix](xs: CssScalar[Q]*): CssScalar[Q] = CssScalar[Q](js.Math.min(xs.map(_.unitless): _*))

  @inline implicit def toBinOpOps[Q](x: CssScalar[Q]): CssBinOpOps[CssScalar[Q]] = new CssBinOpOps[CssScalar[Q]](x)

  // Without this, CssBinOp[Add.type, CssExpr, CssScalar[Px]] doesn't get summoned. o_O
  @inline implicit def toExpr[Q](x: CssScalar[Q]): CssExpr = x

  implicit def secToFiniteDuration(s: CssScalar[Sec]): FiniteDuration = s.unitless.seconds
  implicit def msToFiniteDuration(s: CssScalar[Ms]): FiniteDuration   = s.unitless.milliseconds

  implicit def printer[Q: UnitSuffix]: CssPrinter[CssScalar[Q]] = x => s"${x.unitless}${UnitSuffix[Q].unitSuffix}"
}

object CssDim {
  val empty: CssDim = CssRhs.Values[CssExpr](Nil)

  def mapN(f: Seq[CssExpr] => CssExpr): Seq[CssDim] => CssDim = dim => {
    val values = dim
      .map(_.values)
      .foldLeft[Seq[(MediaQuery, Seq[CssExpr])]](Seq(MediaProfile.all -> Seq.empty)) { (xs, ys) =>
        xs.flatMap {
          case (xq, x) =>
            ys.flatMap {
              case (yq, y) =>
                if (overlap(xq, yq)) Seq((xq and yq) -> (x :+ y)) else Seq.empty
            }
        }
      }
      .map(_.map(f))
    CssRhs.Values(values)
  }

  def zip(a: CssDim, b: CssDim)(f: (CssExpr, CssExpr) => CssExpr): CssDim =
    mapN(xs => f(xs(0), xs(1)))(Seq(a, b))

  private def overlap(x: MediaQuery, y: MediaQuery) = (x, y) match {
    case (x: MediaProfile, y: MediaProfile) => x.minWidth < y.maxWidth && x.maxWidth > y.minWidth
    case _                                  => true
  }
}
