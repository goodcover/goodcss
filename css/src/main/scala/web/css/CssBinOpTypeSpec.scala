package web.css

import scala.annotation.unused

import CssBinOperator._

// Test that pairings of A and B summon the expected instance of CssBinOp.
object CssBinOpTypeSpec {

  trait No[A]

  sealed trait LowPriorityNo {
    implicit def lowPriorityNo[A]: No[A] = new No[A] {}
  }

  object No extends LowPriorityNo {
    implicit def ambiguousPriorityNo1[A](implicit @unused ev: A): No[A] = new No[A] {}
    implicit def ambiguousPriorityNo2[A](implicit @unused ev: A): No[A] = new No[A] {}
  }

  def summon[O <: CssBinOperator, A, B](implicit @unused binop: CssBinOp[O, A, B]): binop.T = ???

  def summonAll[A, B](implicit
    @unused add: CssBinOp[Add, A, B],
    @unused subtract: CssBinOp[Subtract, A, B],
    @unused multiply: CssBinOp[Multiply, A, B],
    @unused divide: CssBinOp[Divide, A, B]
  ): (add.T, subtract.T, multiply.T, divide.T) = ???

  /**
    * Check that no such CssBinOp instance exists.
    *
    * NOTE that this is different from `EvaluateTo[T].no[X](summon[A, B])`, which checks that the instance exists, but
    * evaluates to a type other than T.
    */
  def never[O <: CssBinOperator, A, B](implicit @unused ev: No[CssBinOp[O, A, B]]): Unit = ???

  /** Check that no such CssBinOp instance exists for all CssBinOpToken values. */
  def neverAll[A, B](implicit
    @unused add: No[CssBinOp[Add, A, B]],
    @unused subtract: No[CssBinOp[Subtract, A, B]],
    @unused multiply: No[CssBinOp[Multiply, A, B]],
    @unused divide: No[CssBinOp[Divide, A, B]]
  ): Unit = ???

  /**
    * Test that the summoned instances evaluate or do not to T.
    *
    * NOTE that the test implementation cannot simply implicit search for `CssBinOp[A, B] { type T = X }` because that
    * will succeed with more generic but lower priority matches.
    *
    * E.g. you can successfully summon `CssBinOp[CssExpr, CssExpr] { type T = CssDim }`, but that's not how the class
    * is used in real code. What we need to test is summoning `CssBinOp[CssExpr, CssExpr]` and checking that what is
    * returned is the implementation where `type T = CssExpr`.
    */
  trait EvaluateTo[T] {
    def yes(@unused t: T): Unit                                                 = ???
    def yes(@unused ts: (T, T, T, T)): Unit                                     = ???
    def no[X](@unused x: X)(implicit @unused ev: No[X =:= T]): Unit             = ???
    def no[X](@unused xs: (X, X, X, X))(implicit @unused ev: No[X =:= T]): Unit = ???
  }

  /**
    * CssClamp requires conversion to either CssExpr or CssDim for calculation. The decision of which cannot be made
    * automatically, especially since the conversion to CssExpr causes information loss on IE11.
    */
  object NeverCssClamp {
    neverAll[CssClamp, CssClamp]
  }

  /**
    * Doubles must be converted (E.g. by `.n` method) before use in CSS calculations. One small typed safeguard for
    * code, one giant bug avoided for coder kind.
    */
  object NeverDoubles {
    neverAll[CssScalar[Num], Double]
    neverAll[Double, CssScalar[Num]]
  }

  /**
    * Multiplying and dividing scalars with units is not supported in CSS. Neither is adding and subtraction united
    * scalars with unitless ones. This cannot be checked for CssExpr and CssDim values.
    */
  object NeverScalars {
    never[Multiply, CssScalar[Px], CssScalar[Px]]
    never[Divide, CssScalar[Px], CssScalar[Px]]
    never[Add, CssScalar[Px], CssScalar[Num]]
    never[Subtract, CssScalar[Px], CssScalar[Num]]
    never[Add, CssScalar[Num], CssScalar[Px]]
    never[Subtract, CssScalar[Num], CssScalar[Px]]
  }

  /**
    * CssDim is the most generic numeric type and therefore has the lowest priority matches.
    */
  new EvaluateTo[CssDim] {
    yes(summonAll[CssDim, CssDim])
    yes(summonAll[CssDim, CssExpr])
    yes(summonAll[CssExpr, CssDim])
    yes(summonAll[CssDim, CssClamp])
    yes(summonAll[CssClamp, CssDim])
    yes(summonAll[CssDim, CssScalar[Px]])
    yes(summonAll[CssScalar[Px], CssDim])
    yes(summonAll[CssDim, CssScalar[Num]])
    yes(summonAll[CssScalar[Num], CssDim])

    no(summonAll[CssExpr, CssExpr])
    no(summon[Add, CssScalar[Px], CssScalar[Px]])
    no(summon[Add, CssScalar[Num], CssScalar[Num]])
  }

  new EvaluateTo[CssExpr] {
    yes(summonAll[CssExpr, CssExpr])
    yes(summonAll[CssExpr, CssExpr])
    yes(summonAll[CssExpr, CssScalar[Px]])
    yes(summonAll[CssScalar[Px], CssExpr])
    yes(summonAll[CssExpr, CssScalar[Num]])
    yes(summonAll[CssScalar[Num], CssExpr])
    yes(summonAll[CssExpr, CssClamp])
    yes(summonAll[CssClamp, CssExpr])

    no(summonAll[CssDim, CssDim])
    no(summonAll[CssDim, CssExpr])
    no(summonAll[CssExpr, CssDim])
  }

  /**
    * CssClamp can be combined with pixel scalars or unitless scalars depending on the operation.
    */
  new EvaluateTo[CssClamp] {
    yes(summon[Add, CssClamp, CssScalar[Px]])
    yes(summon[Subtract, CssClamp, CssScalar[Px]])
    yes(summon[Add, CssScalar[Px], CssClamp])
    yes(summon[Subtract, CssScalar[Px], CssClamp])

    yes(summon[Multiply, CssClamp, CssScalar[Num]])
    yes(summon[Divide, CssClamp, CssScalar[Num]])
    yes(summon[Multiply, CssScalar[Num], CssClamp])
    yes(summon[Divide, CssScalar[Num], CssClamp])

    no(summonAll[CssDim, CssClamp])
    no(summonAll[CssClamp, CssDim])
  }
}
