package web.css

import cats.{Eq, Monoid}
import scala.scalajs.js

sealed trait Css {
  def when(test: Boolean): Css           = if (test) this else Css.empty
  @inline def unless(test: Boolean): Css = when(!test)
}

object Css {

  case object Empty extends Css

  val empty: Css = Empty

  implicit val printer: CssPrinter[Css] = _ match {
    case Empty                         => ""
    case CssScope(sel, body)           =>
      val wrapped = body.map(_.print).mkString("\n")
      sel match {
        case None                    => wrapped
        case Some(MediaQuery("all")) => wrapped
        case Some(sel)               => s"${sel.selector} { $wrapped }"
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

  implicit val monoid: Monoid[Css] = Monoid.instance(empty, css(_, _))
}

final case class ClassName private[css] (name: String) {
  def unwrap: String = name

  def when(test: Boolean): ClassName           = if (test) this else ClassName.empty
  @inline def unless(test: Boolean): ClassName = when(!test)

  override def toString(): String = name
}

object ClassName {
  val empty = ClassName("")

  implicit val eq: Eq[ClassName] = Eq.fromUniversalEquals

  implicit val monoid: Monoid[ClassName] =
    Monoid.instance(empty, (x, y) => ClassName(s"${x.unwrap} ".stripLeading + y.unwrap))
}

final case class CssScope private[css] (selector: Option[CssSelector], body: Seq[Css]) extends Css

final case class CssRule private[css] (name: String, rhs: CssRhs[CssValue], isImportant: Boolean = false) extends Css {
  def important: CssRule = copy(isImportant = true)
}

final case class CssProp(propName: String)

final case class CssMetaProp(propName: String)
