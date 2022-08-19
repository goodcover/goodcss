package web.css

import cats.kernel.laws.discipline._
import org.scalacheck.Arbitrary._
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
// import shared.models.b1.{ItemType, ItemTypeMap}

class CssDimSpec extends AnyFunSuite with FunSuiteDiscipline with Configuration with Checkers {
  import CssTestInstances._

  checkAll("CssDim monoid laws", MonoidTests[CssDim].monoid)
}
