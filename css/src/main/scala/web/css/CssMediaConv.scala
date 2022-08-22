package web.css

import cats.data.NonEmptySeq

trait CssMediaConv[-A, -Q] {
  type T

  def apply(xs: NonEmptySeq[(Q, A)]): T
}

trait CssMediaConvLowPriorityImplicits3 {

  sealed class CssMediaConvCss extends CssMediaConv[Css, MediaQuery] {
    type T = Css

    def apply(xs: NonEmptySeq[(MediaQuery, Css)]): Css = CssScope(Nil, xs.toSeq.map { case (q, x) => css(q)(x) })
  }

  implicit def cssMediaConvCss: CssMediaConvCss = new CssMediaConvCss
}

trait CssMediaConvLowPriorityImplicits2 extends CssMediaConvLowPriorityImplicits3 {

  sealed class CssMediaConvRhs extends CssMediaConv[CssValue, MediaQuery] {
    type T = CssRhs[CssValue]

    def apply(xs: NonEmptySeq[(MediaQuery, CssValue)]): CssRhs[CssValue] = CssRhs.Values(xs)
  }

  implicit def cssMediaConvRhs: CssMediaConvRhs = new CssMediaConvRhs
}

trait CssMediaConvLowPriorityImplicits1 extends CssMediaConvLowPriorityImplicits2 {

  sealed class CssMediaConvDim extends CssMediaConv[CssExpr, MediaQuery] {
    type T = CssDim

    def apply(xs: NonEmptySeq[(MediaQuery, CssExpr)]): CssDim = {
      val pixels = xs.collect {
        case (q: MediaProfile, x @ CssScalar(n)) if x.unit == UnitSuffix[Px] => q -> n.px
      }
      // Specialize the output to CssClamp if all inputs meet the criteria.
      if (pixels.length == xs.length) CssRhs.Value(CssClamp(NonEmptySeq.fromSeqUnsafe(pixels)).expr)
      else CssRhs.Values(xs)
    }
  }

  implicit def cssMediaConvDim: CssMediaConvDim = new CssMediaConvDim
}

object CssMediaConv extends CssMediaConvLowPriorityImplicits1 {

  implicit object CssMediaConvClamp extends CssMediaConv[CssScalar[Px], MediaProfile] {
    type T = CssClamp

    def apply(xs: NonEmptySeq[(MediaProfile, CssScalar[Px])]): CssClamp =
      CssClamp(xs)
  }
}
