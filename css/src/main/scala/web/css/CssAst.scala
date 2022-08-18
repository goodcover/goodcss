package web.css

import cats.Monoid
import web.css.CssPrinter.ops._

sealed trait Css

object Css {

  implicit val printer: CssPrinter[Css] = _ match {
    case CssScope(sel, body) =>
      val wrapped = body.map(_.print).mkString(" ")
      sel match {
        case None                    => wrapped
        case Some(MediaQuery("all")) => wrapped
        case Some(sel)               => s"${sel.selector} { $wrapped }"
      }
    case CssRule(name, rhs)  =>
      rhs match {
        case CssRhs.Value(x)   => s"$name: ${x.print}"
        case CssRhs.Values(xs) =>
          xs.map {
            case (MediaQuery("all"), x) => s"$name: ${x.print}"
            case (q, x)                 => s"${q.selector} { ${x.print} }"
          }.mkString("\n")
      }
  }
}

final case class ClassName private[css] (className: String)

object ClassName {
  val empty = ClassName("")

  implicit val monoid: Monoid[ClassName] =
    Monoid.instance(empty, (x, y) => ClassName(s"${x.className} ".trim() + y.className))
}

final case class CssScope private[css] (selector: Option[CssSelector], body: Seq[Css]) extends Css

final case class CssRule private[css] (name: String, rhs: CssRhs[CssValue]) extends Css

final case class CssProp(propName: String)

final case class CssMetaProp(propName: String)
