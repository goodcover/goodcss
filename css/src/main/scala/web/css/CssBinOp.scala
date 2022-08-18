package web.css

import scala.annotation.unused

import CssBinOperator._

trait CssBinOp[O <: CssBinOperator, A, B] {
  type T

  def apply(x: A, y: B): T
}

final class CssBinOpOps[A](val x: A) extends AnyVal {
  @inline def +[B](y: B)(implicit binop: CssBinOp[Add, A, B]): binop.T      = binop(x, y)
  @inline def -[B](y: B)(implicit binop: CssBinOp[Subtract, A, B]): binop.T = binop(x, y)
  @inline def *[B](y: B)(implicit binop: CssBinOp[Multiply, A, B]): binop.T = binop(x, y)
  @inline def /[B](y: B)(implicit binop: CssBinOp[Divide, A, B]): binop.T   = binop(x, y)

  @inline def #+[B](y: B)(implicit binop: CssBinOp[Add, A, B]): binop.T = binop(x, y)
}

trait CssBinOpDimA { self: CssBinOpExprA =>

  sealed class BinOpDimA[O <: CssBinOperator, A](implicit op: O, ev: A => CssDim) extends CssBinOp[O, CssDim, A] {
    type T = CssDim

    @inline def apply(x: CssDim, y: A): CssDim =
      CssDim.zip(x, ev(y))(self.binOpExprA[O, CssExpr].apply(_, _))
  }

  implicit def binOpDimA[O <: CssBinOperator, A](implicit op: O, ev: A => CssDim): BinOpDimA[O, A] =
    new BinOpDimA[O, A]
}

trait CssBinOpADim extends CssBinOpDimA { self: CssBinOpExprA =>

  sealed class BinOpADim[O <: CssBinOperator, A](implicit op: O, ev: A => CssDim) extends CssBinOp[O, A, CssDim] {
    type T = CssDim

    @inline def apply(x: A, y: CssDim): CssDim =
      CssDim.zip(ev(x), y)(self.binOpExprA[O, CssExpr].apply(_, _))
  }

  implicit def binOpADim[O <: CssBinOperator, A](implicit op: O, ev: A => CssDim): BinOpADim[O, A] =
    new BinOpADim[O, A]
}

trait CssBinOpExprA extends CssBinOpADim {

  sealed class BinOpExprA[O <: CssBinOperator, A](implicit op: O, ev: A => CssExpr) extends CssBinOp[O, CssExpr, A] {
    type T = CssExpr

    @inline def apply(x: CssExpr, y: A): CssExpr = CssExpr.Op(x, op, ev(y))
  }

  implicit def binOpExprA[O <: CssBinOperator, A](implicit op: O, ev: A => CssExpr): BinOpExprA[O, A] =
    new BinOpExprA[O, A]
}

trait CssBinOpAExpr extends CssBinOpExprA {

  sealed class BinOpAExpr[O <: CssBinOperator, A](implicit op: O, ev: A => CssExpr) extends CssBinOp[O, A, CssExpr] {
    type T = CssExpr

    @inline def apply(x: A, y: CssExpr): CssExpr = CssExpr.Op(ev(x), op, y)
  }

  implicit def binOpAExpr[O <: CssBinOperator, A](implicit op: O, ev: A => CssExpr): BinOpAExpr[O, A] =
    new BinOpAExpr[O, A]
}

trait CssBinOpScalarScalarExpr extends CssBinOpAExpr {

  sealed class BinOpScalarScalarExpr[O <: CssBinOperator, A: UnitSuffix, B: UnitSuffix](
    f: (CssExpr, CssExpr) => CssExpr
  ) extends CssBinOp[O, CssScalar[A], CssScalar[B]] {
    type T = CssExpr

    @inline def apply(x: CssScalar[A], y: CssScalar[B]): CssExpr = f(x, y)
  }

  implicit def binOpAddScalarExprB[A: UnitSuffix, B: UnitSuffix](implicit
    @unused ev: RelativeUnit[A]
  ): BinOpScalarScalarExpr[Add, A, B] =
    new BinOpScalarScalarExpr(CssExpr.Op(_, CssBinOperator.Add, _))

  implicit def binOpSubtractScalarExprB[A: UnitSuffix, B: UnitSuffix](implicit
    @unused ev: RelativeUnit[A]
  ): BinOpScalarScalarExpr[Subtract, A, B] =
    new BinOpScalarScalarExpr(CssExpr.Op(_, CssBinOperator.Subtract, _))
}

object CssBinOp extends CssBinOpScalarScalarExpr {

  sealed class BinOpScalarScalar[O <: CssBinOperator, U](f: (CssScalar[U], CssScalar[U]) => CssScalar[U])
      extends CssBinOp[O, CssScalar[U], CssScalar[U]] {
    type T = CssScalar[U]

    @inline def apply(x: CssScalar[U], y: CssScalar[U]): CssScalar[U] = f(x, y)

    @inline def units[A]: BinOpScalarScalar[O, A] = this.asInstanceOf[BinOpScalarScalar[O, A]]
  }

  sealed class BinOpScalarNum[O <: CssBinOperator, U](f: (CssScalar[U], CssScalar[Num]) => CssScalar[U])
      extends CssBinOp[O, CssScalar[U], CssScalar[Num]] {
    type T = CssScalar[U]

    @inline def apply(x: CssScalar[U], y: CssScalar[Num]): CssScalar[U] = f(x, y)

    @inline def units[A]: BinOpScalarNum[O, A] = this.asInstanceOf[BinOpScalarNum[O, A]]
  }

  sealed class BinOpNumScalar[O <: CssBinOperator, U](f: (CssScalar[Num], CssScalar[U]) => CssScalar[U])
      extends CssBinOp[O, CssScalar[Num], CssScalar[U]] {
    type T = CssScalar[U]

    @inline def apply(x: CssScalar[Num], y: CssScalar[U]): CssScalar[U] = f(x, y)

    @inline def units[A]: BinOpNumScalar[O, A] = this.asInstanceOf[BinOpNumScalar[O, A]]
  }

  sealed class BinOpClampPx[O <: CssBinOperator](f: (CssScalar[Px], CssScalar[Px]) => CssScalar[Px])
      extends CssBinOp[O, CssClamp, CssScalar[Px]] {
    type T = CssClamp

    @inline def apply(x: CssClamp, y: CssScalar[Px]): CssClamp = x.map(f(_, y))
  }

  sealed class BinOpPxClamp[O <: CssBinOperator](f: (CssScalar[Px], CssScalar[Px]) => CssScalar[Px])
      extends CssBinOp[O, CssScalar[Px], CssClamp] {
    type T = CssClamp

    @inline def apply(x: CssScalar[Px], y: CssClamp): CssClamp = y.map(f(x, _))
  }

  sealed class BinOpClampNum[O <: CssBinOperator](f: (CssScalar[Px], CssScalar[Num]) => CssScalar[Px])
      extends CssBinOp[O, CssClamp, CssScalar[Num]] {
    type T = CssClamp

    @inline def apply(x: CssClamp, y: CssScalar[Num]): CssClamp = x.map(f(_, y))
  }

  sealed class BinOpNumClamp[O <: CssBinOperator](f: (CssScalar[Num], CssScalar[Px]) => CssScalar[Px])
      extends CssBinOp[O, CssScalar[Num], CssClamp] {
    type T = CssClamp

    @inline def apply(x: CssScalar[Num], y: CssClamp): CssClamp = y.map(f(x, _))
  }

  private val binOpAddScalarScalarImpl: BinOpScalarScalar[Add, _] = new BinOpScalarScalar[Add, Any](_ add _)

  private val binOpSubtractScalarScalarImpl: BinOpScalarScalar[Subtract, _] =
    new BinOpScalarScalar[Subtract, Any](_ subtract _)

  private val binOpMultiplyScalarNumImpl: BinOpScalarNum[Multiply, _] =
    new BinOpScalarNum[Multiply, Any](_ multiply _)
  private val binOpDivideScalarNumImpl: BinOpScalarNum[Divide, _]     = new BinOpScalarNum[Divide, Any](_ divide _)

  private val binOpMultiplyNumScalarImpl: BinOpNumScalar[Multiply, _] =
    new BinOpNumScalar[Multiply, Any]((x, y) => y multiply x)

  private val binOpDivideNumScalarImpl: BinOpNumScalar[Divide, _] =
    new BinOpNumScalar[Divide, Any]((x, y) => y divide x)

  implicit def binOpAddScalarScalar[U]: BinOpScalarScalar[Add, U]           = binOpAddScalarScalarImpl.units[U]
  implicit def binOpSubtractScalarScalar[U]: BinOpScalarScalar[Subtract, U] = binOpSubtractScalarScalarImpl.units[U]

  implicit def binOpAddScalarExprA[A: UnitSuffix, B: UnitSuffix](implicit
    @unused ev: RelativeUnit[B]
  ): BinOpScalarScalarExpr[Add, A, B] =
    new BinOpScalarScalarExpr(CssExpr.Op(_, CssBinOperator.Add, _))

  implicit def binOpSubtractScalarExprA[A: UnitSuffix, B: UnitSuffix](implicit
    @unused ev: RelativeUnit[B]
  ): BinOpScalarScalarExpr[Subtract, A, B] =
    new BinOpScalarScalarExpr(CssExpr.Op(_, CssBinOperator.Subtract, _))

  implicit def binOpMultiplyScalarNum[U]: BinOpScalarNum[Multiply, U] = binOpMultiplyScalarNumImpl.units[U]
  implicit def binOpDivideScalarNum[U]: BinOpScalarNum[Divide, U]     = binOpDivideScalarNumImpl.units[U]

  implicit def binOpMultiplyNumScalar[U]: BinOpNumScalar[Multiply, U] = binOpMultiplyNumScalarImpl.units[U]
  implicit def binOpDivideNumScalar[U]: BinOpNumScalar[Divide, U]     = binOpDivideNumScalarImpl.units[U]

  implicit val binOpAddClampPx: BinOpClampPx[Add]           = new BinOpClampPx[Add](_ add _)
  implicit val binOpSubtractClampPx: BinOpClampPx[Subtract] = new BinOpClampPx[Subtract](_ subtract _)

  implicit val binOpAddPxClamp: BinOpPxClamp[Add]           = new BinOpPxClamp[Add](_ add _)
  implicit val binOpSubtractPxClamp: BinOpPxClamp[Subtract] = new BinOpPxClamp[Subtract](_ subtract _)

  implicit val binOpMultiplyClampNum: BinOpClampNum[Multiply] = new BinOpClampNum[Multiply](_ multiply _)
  implicit val binOpDivideClampNum: BinOpClampNum[Divide]     = new BinOpClampNum[Divide](_ divide _)

  implicit val binOpMultiplyNumClamp: BinOpNumClamp[Multiply] = new BinOpNumClamp[Multiply]((x, y) => y multiply x)
  implicit val binOpDivideNumClamp: BinOpNumClamp[Divide]     = new BinOpNumClamp[Divide]((x, y) => y divide x)
}
