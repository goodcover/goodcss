package web.css

import cats.tests.StrictCatsEquality
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

object CssDimCalcSpec {
  val qa = MediaQuery("a")
  val qb = MediaQuery("b")

  val pa = MediaProfile(minWidth = 100.px, maxWidth = 300.px)
  val pb = MediaProfile(minWidth = 200.px, maxWidth = 400.px)
  val pc = MediaProfile(minWidth = 500.px, maxWidth = 700.px)
  val pd = MediaProfile(minWidth = 600.px, maxWidth = 800.px)
  val pe = MediaProfile(minWidth = 700.px, maxWidth = 900.px)

  val b: CssDim = 1.px
  val c: CssDim = CssRhs(Seq(qa -> 2.px))
  val d: CssDim = CssRhs(Seq(qb -> 3.px))
  val e: CssDim = CssRhs(Seq(pa -> 4.px))
  val f: CssDim = CssRhs(Seq(pb -> 5.px))
  val g: CssDim = CssRhs(Seq(pc -> 6.px))
  val h: CssDim = CssRhs(Seq(pd -> 7.px))
  val i: CssDim = CssRhs(Seq(pe -> 8.px))
}

class CssDimCalcSpec extends AsyncFlatSpec with Matchers with StrictCatsEquality with TableDrivenPropertyChecks {

  import CssTestInstances._
  import CssDimCalcSpec._
  import CssBinOperator._

  private val expectations = Table(
    ("CssDim", "Expected"),
    (b #+ b, CssRhs.Value(CssExpr.Op(1.px, Add, 1.px))),
    (b #+ c, CssRhs(Seq(qa -> CssExpr.Op(1.px, Add, 2.px)))),
    (c #+ b, CssRhs(Seq(qa -> CssExpr.Op(2.px, Add, 1.px)))),
    (c #+ c, CssRhs(Seq(qa -> CssExpr.Op(2.px, Add, 2.px)))),
    (c #+ d, CssRhs(Seq((qa and qb) -> CssExpr.Op(2.px, Add, 3.px)))),
    (d #+ c, CssRhs(Seq((qb and qa) -> CssExpr.Op(3.px, Add, 2.px)))),
    (c #+ e, CssRhs(Seq((qa and pa) -> CssExpr.Op(2.px, Add, 4.px)))),
    (e #+ c, CssRhs(Seq((pa and qa) -> CssExpr.Op(4.px, Add, 2.px)))),
    (e #+ f, CssRhs(Seq((pa and pb) -> CssExpr.Op(4.px, Add, 5.px)))),
    (f #+ e, CssRhs(Seq((pb and pa) -> CssExpr.Op(5.px, Add, 4.px)))),
    (e #+ g, CssRhs.empty),
    (g #+ e, CssRhs.empty),
    (g #+ h, CssRhs(Seq((pc and pd) -> CssExpr.Op(6.px, Add, 7.px)))),
    (h #+ g, CssRhs(Seq((pd and pc) -> CssExpr.Op(7.px, Add, 6.px)))),
    (h #+ i, CssRhs(Seq((pd and pe) -> CssExpr.Op(7.px, Add, 8.px)))),
    (i #+ h, CssRhs(Seq((pe and pd) -> CssExpr.Op(8.px, Add, 7.px))))
  )

  behavior of "CssDim"

  it should "calculate correctly" in {
    forAll(expectations) { (dim, expected) =>
      dim must ===(expected)
    }
  }
}
