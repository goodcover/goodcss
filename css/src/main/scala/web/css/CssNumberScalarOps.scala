package web.css

final class CssNumberScalarOps(val d: Double) extends AnyVal {

  def n: CssScalar[Num] = CssScalar(d)

  def px: CssScalar[Px] = CssScalar(d)

  def percent: CssScalar[Percent] = CssScalar(d)

  /** Font size of element, or in the case of font-size, the element itself. */
  def em: CssScalar[Em] = CssScalar(d)

  /** x-height of element's font. */
  def ex: CssScalar[Ex] = CssScalar(d)

  /** Width of the glyph "0" of the element's font. */
  def ch: CssScalar[Ch] = CssScalar(d)

  /** Font size of root element. */
  def rem: CssScalar[Rem] = CssScalar(d)

  /** Line height of element. */
  def lh: CssScalar[Lh] = CssScalar(d)

  /** 1% of viewport width. */
  def vw: CssScalar[Vw] = CssScalar(d)

  /** 1% of viewport height. */
  def vh: CssScalar[Vh] = CssScalar(d)

  /** 1% of viewport's smaller dimension. */
  def vmin: CssScalar[Vmin] = CssScalar(d)

  /** 1% of viewport's larger dimension. */
  def vmax: CssScalar[Vmax] = CssScalar(d)

  /**
    */
  def fr: CssScalar[Fr] = CssScalar(d)

  /** Seconds */
  def s: CssScalar[Sec] = CssScalar(d)

  /** Milliseconds */
  def ms: CssScalar[Ms] = CssScalar(d)

  /** Degrees (angles) */
  def deg: CssScalar[Deg] = CssScalar(d)
}
