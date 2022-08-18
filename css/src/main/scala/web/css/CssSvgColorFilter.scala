package web.css

import org.scalajs.dom.document

object CssSvgColorFilter {
  val svgns = "http://www.w3.org/2000/svg"

  def filterId(rgb: CssRgb) = {
    import rgb._
    s"gc-svg-color-filter-$r-$g-$b-${(a * 100).toInt}"
  }

  def createColorFilter(rgb: CssRgb) = {
    import rgb._
    val fid    = filterId(rgb)
    val fsvg   = document.createElementNS(svgns, "svg")
    fsvg.setAttribute("width", "0")
    fsvg.setAttribute("height", "0")
    fsvg.setAttribute("margin", "0")
    fsvg.setAttribute("padding", "0")
    val filter = document.createElementNS(svgns, "filter")
    filter.setAttribute("color-interpolation-filters", "sRGB")
    filter.setAttribute("id", fid)
    val matrix = document.createElementNS(svgns, "feColorMatrix")
    matrix.setAttribute("in", "SourceGraphic")
    matrix.setAttribute("type", "matrix")
    matrix.setAttribute("values", s"""
      1 0 0 0 $r
      0 1 0 0 $g
      0 0 1 0 $b
      0 0 0 $a 0
    """)
    fsvg.appendChild(filter)
    filter.appendChild(matrix)
    fsvg
  }

  def filterContainer = {
    val id        = "gc-svg-color-filters"
    val container = document.getElementById(id)
    if (container != null) container
    else {
      val container = document.createElement("div")
      container.id = id
      container.setAttribute("style", "display: none")
      document.body.appendChild(container)
      container
    }
  }

  /**
    * Return a value suitable for supplying to the CSS `filter` property.
    *
    * The filter will transform a black image to the supplied color.
    */
  def blackToRgb(rgb: CssRgb): CssValue = {
    if (rgb == CssRgb.black) none
    else {
      val id = filterId(rgb)
      if (document.getElementById(id) == null) {
        val svg = createColorFilter(rgb)
        filterContainer.appendChild(svg)
      }
      url(s"#$id")
    }
  }

  def blackToHsl(hsl: CssHsl): CssValue = blackToRgb(CssRgb.fromHsl(hsl))
}
