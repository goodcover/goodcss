package web.css

sealed trait Css {
  def print: String

  override def cn: ClassName = GoodMotion.css(print)
}

final case class ClassName private[css] (print: String)

final case class CssScope private[css] (selector: Option[CssSelector], body: Seq[Css]) extends Css {

  private def wrap(x: String) = {
    selector match {
      case None                    => x
      case Some(MediaQuery("all")) => x
      case Some(sel)               => s"${sel.selector} { $x }"
    }
  }

  override def print: String = wrap(body.map(_.print).mkString(" "))
}

final case class CssRule private[css] (name: String, rhs: CssRhs) extends Css {

  def print: String = rhs match {
    case v: CssValue     => s"$name: ${v.print}"
    case CssValues(vs)   =>
      vs.map {
        case (MediaQuery("all"), v) => s"$name: ${v.print}"
        case (q, v)                 => s"${q.selector} { ${v.print} }"
      }.mkString("\n")
    case CssOverride(rs) => rs.map(CssRule(name, _).print).mkString("\n")
  }
}
