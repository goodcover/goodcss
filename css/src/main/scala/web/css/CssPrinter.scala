package web.css

trait CssPrinter[-A] {
  def print(a: A): String
}

object CssPrinter {
  @inline def apply[A](implicit p: CssPrinter[A]): CssPrinter[A] = p

  class Ops[A: CssPrinter](target: A) {
    def print: String = CssPrinter[A].print(target)
  }
}
