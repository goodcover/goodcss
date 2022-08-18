package web.css

/**
  * A CSS selector-like.
  * - selector: `.a, .b .c`
  * - media query: `only screen and (min-width: 600px)`
  */
sealed trait CssSelector {
  def selector: String

  override def toString: String = s"CssSelector($selector)"
}

object CssSelector {
  private[css] def apply(sel: String): CssSelector = new CssSelector { override val selector = sel }

  private[css] def unapply(sel: CssSelector): Option[String] = Some(sel.selector)

  implicit def fromString(s: String): CssSelector = apply(s)
}

/**
  * A CSS media query 'selector'.
  */
sealed trait MediaQuery extends CssSelector {

  /** Media query string without `@media` prefix */
  def query: String

  /** Media query string with `@media` prefix */
  override lazy val selector = "@media " + query

  def or(that: MediaQuery): MediaQuery =
    MediaQuery(s"(${this.query}) or (${that.query})")

  def and(that: MediaQuery): MediaQuery = (this, that) match {
    case (x: MediaProfile, y: MediaProfile) => x and y
    case (x, y)                             =>
      val l = if (x.query == "all") "" else x.query
      val r = if (y.query == "all") "" else y.query
      val q = if (x.query == y.query) x.query else if (l.isEmpty || r.isEmpty) s"$l$r" else s"($l) and ($r)"
      MediaQuery(q)
  }

  def isAll: Boolean = query == "all"

  override def toString: String = s"MediaQuery($query)"
}

object MediaQuery {

  private[css] def apply(q: String): MediaQuery =
    new MediaQuery {
      val query: String = q
    }

  private[css] def unapply(q: MediaQuery): Option[String] = Some(q.query)

  val all: MediaQuery = apply("all")
}

/**
  * Fixed media profile. Used in Theme to define desktop, tablet, phone break points.
  */
final case class MediaProfile(minWidth: CssScalar[Px] = 0.px, maxWidth: CssScalar[Px] = Double.PositiveInfinity.px)
    extends MediaQuery {

  def encompassesWidth(width: CssScalar[Px]): Boolean =
    minWidth <= width && maxWidth >= width

  private val isEmpty = minWidth == 0.px && maxWidth == Double.PositiveInfinity.px

  private def nonEmptyQuery  = Seq(
    Option.unless(minWidth == 0.px)(s"(min-width: $minWidth)"),
    Option.unless(maxWidth == Double.PositiveInfinity.px)(s"(max-width: $maxWidth)"),
  ).collect { case Some(x) => x }.reduce((x, y) => s"$x and $y")

  override val query: String =
    if (isEmpty) MediaQuery.all.query
    else nonEmptyQuery

  def and(that: MediaProfile): MediaProfile =
    MediaProfile(CssScalar.max(minWidth, that.minWidth), CssScalar.min(maxWidth, that.maxWidth))

  def breakpoint: CssExpr = CssClamp.breakpoint(minWidth)
}

object MediaProfile {

  val all: MediaProfile = MediaProfile()
}
