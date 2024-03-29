package web.css

import cats.{Eq, Monoid}
import cats.syntax.monoid._
import scala.scalajs.js

sealed trait Css {
  def when(test: Boolean): Css           = if (test) this else Css.empty
  @inline def unless(test: Boolean): Css = when(!test)
}

object Css {

  case object Empty extends Css

  val empty: Css = Empty

  implicit lazy val printer: CssPrinter[Css] = _ match {
    case Empty                         => ""
    case CssScope(sel, body)           =>
      val wrapped = body.filterNot(_.isEmpty).map(_.print).mkString("\n")
      sel match {
        case Seq()                  => wrapped
        case Seq(MediaQuery("all")) => wrapped
        case selectors              => s"${printSelectors(selectors)} { $wrapped }"
      }
    case CssRule(name, rhs, important) =>
      val bang = if (important) " !important" else ""
      rhs match {
        case CssRhs.Empty      => ""
        case CssRhs.Value(x)   => s"$name: ${x.print}$bang;"
        case CssRhs.Values(xs) =>
          xs.map {
            case (MediaQuery("all"), x) => s"$name: ${x.print}$bang;"
            case (q, x)                 => s"${q.selector} { $name: ${x.print}$bang; }"
          }.toSeq
            .mkString("\n")
      }
  }

  implicit def fromOption(o: Option[Css]): Css      = o.getOrElse(Empty)
  implicit def fromUndefOr(o: js.UndefOr[Css]): Css = o.getOrElse(Empty)

  val emptyEq: Eq[Css]             = Eq.instance {
    case (Empty, Empty)             => true
    case (scope: CssScope, Empty)   => scope.isEmpty
    case (Empty, scope: CssScope)   => scope.isEmpty
    case (a: CssScope, b: CssScope) => a.isEmpty && b.isEmpty
    case _                          => false
  }
  implicit val eq: Eq[Css]         = Eq.or(emptyEq, Eq.fromUniversalEquals)
  implicit val monoid: Monoid[Css] = Monoid.instance(empty, css(_, _))

  private def printSelectors(selectors: Seq[CssSelectorLike]): String = selectors.map(_.selector).mkString(", ")
}

final case class ClassName private[css] (name: String) {
  def unwrap: String = name

  def map(f: String => String): ClassName = ClassName(name.split(" +").map(f).mkString(" "))

  def when(test: Boolean): ClassName           = if (test) this else ClassName.empty
  @inline def unless(test: Boolean): ClassName = when(!test)

  def toSelector: CssSelector = CssSelector(name.split(" +").mkString(".", ".", ""))

  override def toString(): String = name
}

object ClassName {
  val empty = ClassName("")

  def fromMixed(xs: Seq[Style]): ClassName = {
    val (name, css) = xs.foldLeft((ClassName.empty, Css.empty)) { (x, y) =>
      val (name, css) = x
      (y: Any) match {
        case c: ClassName => (name |+| css.cn |+| c, Css.empty)
        case c: Css       => (name, css |+| c)
        case _            => throw js.JavaScriptException("Inconceivable! Pattern match fall-through.")
      }
    }
    name |+| css.cn
  }

  def fromMixed(x: Style): ClassName = fromMixed(Seq(x))

  implicit val eq: Eq[ClassName] = Eq.fromUniversalEquals

  implicit val monoid: Monoid[ClassName] = Monoid.instance(empty, GoodMotion.cx(_, _))
}

final case class CssScope private[css] (selector: Seq[CssSelectorLike], body: Seq[Css]) extends Css {
  def isEmpty: Boolean = body.forall(_.isEmpty)
}

final case class CssRule private[css] (name: String, rhs: CssRhs[CssValue], isImportant: Boolean = false) extends Css {
  def important: CssRule = copy(isImportant = true)
}

final case class CssProp(propName: String)

final case class CssMetaProp(propName: String)
