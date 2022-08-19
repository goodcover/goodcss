package web.css

import org.scalacheck._
import org.scalacheck.Arbitrary._

object CssTestInstances {

  implicit val arbMediaQuery: Arbitrary[MediaQuery] = Arbitrary(Gen.asciiStr.map(MediaQuery(_)))

  implicit val arbCssExpr: Arbitrary[CssExpr] =
    Arbitrary(arbInt.arbitrary.map(_.px))

  implicit val arbCssDim: Arbitrary[CssDim] =
    Arbitrary(
      Gen.oneOf[CssDim](
        arbCssExpr.arbitrary.map(CssRhs.Value(_)),
        Gen.listOf(arbTuple2(arbMediaQuery, arbCssExpr).arbitrary).map(CssRhs(_))
      )
    )
}
