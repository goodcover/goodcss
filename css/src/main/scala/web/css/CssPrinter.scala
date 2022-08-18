package web.css

trait CssPrinter[-A] {
  def print(a: A): String
}

object CssPrinter {
  @inline def apply[A](implicit p: CssPrinter[A]): CssPrinter[A] = p

  object ops {

    implicit class CssPrinterOps[A: CssPrinter](target: A) {
      def print: String = CssPrinter[A].print(target)
    }
  }
}
