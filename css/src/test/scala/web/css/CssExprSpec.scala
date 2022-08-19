package web.css

import scalajs.js
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import CssExpr._
import CssBinOperator._

class CssExprSpec extends AsyncFlatSpec with Matchers with TableDrivenPropertyChecks {

  private val expectations = Table(
    ("CssExpr", "String", "calc()", "note"),
    (1.px, "1px", false, "Top-level scalars should be printed as is."),
    (Op(1.px, Add, 1.px), "1px + 1px", true, "Top-level operations should not be bracketed."),
    (Call("max", Seq(1.px, 2.px, 3.px)), "max(1px, 2px, 3px)", false, "Top-level calls should not bracketed."),
    (Unsafe("1 / 2"), "1 / 2", true, "Top-level unsafe strings should not be bracketed."),
    (Op(1.px, Add, Op(2.px, Subtract, 3.px)), "1px + (2px - 3px)", true, "Nested operations should be bracketed."),
    (Call("abs", Seq(Op(1.px, Add, 2.px))), "abs(1px + 2px)", false, "Top-level args should not be bracketed."),
    (Op(1.px, Add, Unsafe("3px")), "1px + (3px)", true, "Nested unsafe strings should be bracketed."),
    (
      Op(
        2.n,
        Multiply,
        Call("max", Seq(1.px, Call("min", Seq(2.px, Op(3.px, Multiply, Op(4.px, Subtract, 5.px)))), Unsafe("1 / 2")))
      ),
      "2 * max(1px, min(2px, 3px * (4px - 5px)), 1 / 2)",
      true,
      "Kitchen sink test."
    )
  )

  behavior of "CssExpr"

  it should "print correctly" in {
    forAll(expectations) { (expr, expected, _, _) =>
      CssExpr.print(expr) mustBe expected
    }
  }

  it should "print correctly inside a rule" in {
    forAll(expectations) { (expr, expected0, calc, _) =>
      val expected = if (calc) s"calc($expected0)" else expected0
      (width :- expr).print mustBe s"width: $expected;"
    }
  }

  it should "implicitly convert correctly" in {
    forAll(expectations) { (expr, _, _, _) =>
      val rhs: CssDim = expr
      (rhs match {
        case CssRhs.Value(s) => s
        case _           => throw new js.JavaScriptException("Unexpected!")
      }) mustBe expr
    }
  }
}
