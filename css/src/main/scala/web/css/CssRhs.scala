package web.css

import cats.syntax.functor._

sealed trait CssRhs {
  def map(f: CssValue => CssValue): CssRhs
}

sealed trait CssValue extends CssRhs {
  override def map(f: CssValue => CssValue): CssValue = f(this)

  def print: String
}

sealed trait CssDim

final case class CssKeyword(keyword: String) extends CssValue {
  override def print: String = keyword
}

sealed trait CssExpr extends CssDim with CssValue

final case class CssScalar[Q](unitless: Double)(implicit unit: UnitSuffix[Q]) extends CssExpr {
  override def print: String = s"${unitless}${unit.unitSuffix}"
}

final case class CssOp(l: CssExpr, op: CssBinOperator, r: CssExpr) extends CssExpr {
  override def print: String = s"${bracket(l)} ${op.token} ${bracket(r)}"

  private def bracket(expr: CssExpr): String = expr match {
    case x: CssScalar[_]     => x.print
    case x: CssOp         => s"(${x.print})"
    case x: CssCall       => x.print
    case x: CssExprUnsafe => s"(${x.print})"
  }
}

final case class CssCall(f: String, args: Seq[CssExpr]) extends CssExpr {
  override def print: String = args.map(_.print).mkString(s"$f(", ", ", ")")
}

final case class CssExprUnsafe(exprString: String) extends CssExpr {
  override def print: String = exprString
}

final case class CssExprs private[css] (values: Seq[(MediaQuery, CssExpr)]) extends CssDim

final case class CssValues private[css] (values: Seq[(MediaQuery, CssValue)]) extends CssRhs {
  override def map(f: CssValue => CssValue): CssValues = CssValues(values.map(_.map(f)))
}

final case class CssOverride private[css] (values: Seq[CssRhs]) extends CssRhs {
  override def map(f: CssValue => CssValue): CssOverride = CssOverride(values.map(_.map(f)))
}
