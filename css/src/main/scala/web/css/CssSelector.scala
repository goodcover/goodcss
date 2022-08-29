package web.css

import cats.Eq
import web.css.CssSelector.Endo

/**
  * A CSS selector-like.
  * - selector: `.a, .b .c`
  * - media query: `only screen and (min-width: 600px)`
  */
sealed trait CssSelectorLike {
  def selector: String
}

/** A CSS selector
  *
  * NOTE: descendent selection is defined by the >> operator
  *
  * Example:
  *     &(cn"abc") >> cn"foo" > sel"*"
  *
  * will produce:
  *     &.abc .foo > *
  *
  * Import web.elem._ and slinky tags can be used to denote tag selectors, as can TagElemImpl. In both cases, class
  * names attached to the tag will be used to form the resulting selector.
  *
  * Example:
  *     import slinky.web.{html => <}
  *
  *     & > <.img.named(cn("foo"))
  *
  * will produce:
  *     & > img.foo
  */
final case class CssSelector(selector: String) extends CssSelectorLike {

  def apply(selector: CssSelector): CssSelector  = sel"$this$selector"
  @inline def apply(selector: Endo): CssSelector = apply(selector(sel))

  def >>(selector: CssSelector): CssSelector  = sel"$this $selector"
  @inline def >>(selector: Endo): CssSelector = this >> selector(sel)

  def >(selector: CssSelector): CssSelector  = sel"$this > $selector"
  @inline def >(selector: Endo): CssSelector = this > selector(sel)

  def ~(selector: CssSelector): CssSelector  = sel"$this ~ $selector"
  @inline def ~(selector: Endo): CssSelector = this ~ selector(sel)

  def +(selector: CssSelector): CssSelector  = sel"$this + $selector"
  @inline def +(selector: Endo): CssSelector = this + selector(sel)

  def * : CssSelector = sel"$this *"

  def id(id: String): CssSelector = CssSelector(s"$selector#$id")

  def attr(expr: String): CssSelector = CssSelector(s"$selector[$expr]")

  def attr(name: String, value: String, caseSensitive: Boolean = true, operator: String = "="): CssSelector =
    attr(s"""$name$operator"$value${if (caseSensitive) "" else " i"}"""")

  def attrList(name: String, value: String, caseSensitive: Boolean = true): CssSelector =
    attr(name, value, caseSensitive, "~=")

  def attrHyphen(name: String, value: String, caseSensitive: Boolean = true): CssSelector =
    attr(name, value, caseSensitive, "|=")

  def attrPrefix(name: String, value: String, caseSensitive: Boolean = true): CssSelector =
    attr(name, value, caseSensitive, "^=")

  def attrSuffix(name: String, value: String, caseSensitive: Boolean = true): CssSelector =
    attr(name, value, caseSensitive, "$=")

  def attrContains(name: String, value: String, caseSensitive: Boolean = true): CssSelector =
    attr(name, value, caseSensitive, "*=")

  def clobber: CssSelector = CssSelector(selector * 4)

  // Pseudo classes
  def active: CssSelector       = sel"$this:active"
  def anyLink: CssSelector      = sel"$this:any-link"
  def blank: CssSelector        = sel"$this:blank"
  def checked: CssSelector      = sel"$this:checked"
  def default: CssSelector      = sel"$this:default"
  def disabled: CssSelector     = sel"$this:disabled"
  def empty: CssSelector        = sel"$this:empty"
  def enabled: CssSelector      = sel"$this:enabled"
  def firstChild: CssSelector   = sel"$this:first-child"
  def firstOfType: CssSelector  = sel"$this:first-of-type"
  def focus: CssSelector        = sel"$this:focus"
  def focusVisible: CssSelector = sel"$this:focus-visible"
  def focusWithin: CssSelector  = sel"$this:focus-within"
  def hover: CssSelector        = sel"$this:hover"
  def invalid: CssSelector      = sel"$this:invalid"
  def lastChild: CssSelector    = sel"$this:last-child"
  def lastOfType: CssSelector   = sel"$this:last-of-type"
  def link: CssSelector         = sel"$this:link"
  def localLink: CssSelector    = sel"$this:local-link"

  def not(selector: CssSelector): CssSelector  = sel"$this:not($selector)"
  @inline def not(selector: Endo): CssSelector = not(selector(sel))

  def onlyChild: CssSelector  = sel"$this:only-child"
  def onlyOfType: CssSelector = sel"$this:only-of-type"
  def optional: CssSelector   = sel"$this:optional"
  def required: CssSelector   = sel"$this:required"
  def root: CssSelector       = sel"$this:root"
  def target: CssSelector     = sel"$this:target"
  def valid: CssSelector      = sel"$this:valid"
  def visited: CssSelector    = sel"$this:visited"

  // Pseudo elements
  def after: CssSelector       = sel"$this::before"
  def before: CssSelector      = sel"$this::before"
  def firstLetter: CssSelector = sel"$this::first-letter"
  def firstLine: CssSelector   = sel"$this::first-line"
  def placeholder: CssSelector = sel"$this::placeholder"
  def selection: CssSelector   = sel"$this::selection"
}

object CssSelector {

  private[css] type Endo = CssSelector => CssSelector

  val empty: CssSelector = CssSelector("")

  implicit def fromClassName(name: ClassName): CssSelector = name.toSelector

  implicit val eq: Eq[CssSelector] = _.selector == _.selector
}

/**
  * A CSS media query 'selector'.
  */
sealed trait MediaQuery extends CssSelectorLike {

  /** Media query string without `@media` prefix */
  def query: String

  /** Media query string with `@media` prefix */
  override def selector: String = "@media " + query

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

  implicit val eq: Eq[MediaQuery] = _.selector == _.selector
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
