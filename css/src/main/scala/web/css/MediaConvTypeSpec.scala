package web.css

import scala.annotation.unused

// Test that pairings of A and Q summon an instance of CssMediaConv with the expected and most specific type T applicable.
object CssMediaConvTypeSpec {
  trait No[A]

  sealed trait LowPriorityNo {
    implicit def lowPriorityNo[A]: No[A] = new No[A] {}
  }

  object No extends LowPriorityNo {
    implicit def ambiguousPriorityNo1[A](implicit @unused ev: A): No[A] = new No[A] {}
    implicit def ambiguousPriorityNo2[A](implicit @unused ev: A): No[A] = new No[A] {}
  }

  def summon[A, Q](implicit @unused conv: CssMediaConv[A, Q]): conv.T = ???

  trait ConversionTo[T] {
    // Can't use implicit search for `CssMediaConv[A, Q] { type T = X }` because that will succeed with more generic but
    // lower priority matches.
    //
    // E.g. you can successfully summon `CssMediaConv[CssExpr, MediaQuery] { type T = CssRhs }`, but that's not how the
    // class is used in real code. What we need to test is summoning `CssMediaConv[CssExpr, MediaQuery]` and checking
    // that what is returned is the implementation where `type T = CssDim`.

    def yes(@unused t: T): Unit                                     = ???
    def no[X](@unused x: X)(implicit @unused ev: No[X =:= T]): Unit = ???
  }

  new ConversionTo[CssRhs[CssValue]] {
    yes(summon[CssValue, MediaQuery])
    yes(summon[CssValue, MediaProfile])
    // yes(summon[CssKeyword, MediaProfile])
  }

  new ConversionTo[CssRhs[CssExpr]] {
    yes(summon[CssExpr, MediaQuery])
    yes(summon[CssExpr, MediaProfile])
    // yes(summon[CssScalar[Num], MediaProfile])
    // yes(summon[CssScalar[Px], MediaQuery])

    no(summon[CssScalar[Px], MediaProfile])
  }

  new ConversionTo[CssClamp] {
    yes(summon[CssScalar[Px], MediaProfile])

    // no(summon[CssScalar[Px], MediaQuery])
    // no(summon[CssScalar[Num], MediaProfile])
    no(summon[CssExpr, MediaProfile])
  }
}
