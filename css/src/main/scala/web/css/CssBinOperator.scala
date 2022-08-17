package web.css

sealed abstract class CssBinOperator(val token: String)

object CssBinOperator {
  implicit case object Add      extends CssBinOperator("+")
  implicit case object Subtract extends CssBinOperator("-")
  implicit case object Multiply extends CssBinOperator("*")
  implicit case object Divide   extends CssBinOperator("/")

  type Add      = Add.type
  type Subtract = Subtract.type
  type Multiply = Multiply.type
  type Divide   = Divide.type

  @inline def apply[O <: CssBinOperator](implicit op: O): O = op
}
