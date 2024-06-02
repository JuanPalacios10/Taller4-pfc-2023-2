package taller4

import common._

class NewtonParalela {
  val newton = new Newton()

  def mostrarPar(e: Expr): String = {
    e match {
      case Numero(_) => newton.mostrar(e)
      case Atomo(_) => newton.mostrar(e)
      case Suma(e1, e2) =>
        val (m1, m2) = parallel(mostrarPar(e1), mostrarPar(e2))
        s"($m1 + $m2)"
      case Prod(e1, e2) =>
        val (m1, m2) = parallel(mostrarPar(e1), mostrarPar(e2))
        s"($m1 * $m2)"
      case Resta(e1, e2) =>
        val (m1, m2) = parallel(mostrarPar(e1), mostrarPar(e2))
        s"($m1 - $m2)"
      case Div(e1, e2) =>
        val (m1, m2) = parallel(mostrarPar(e1), mostrarPar(e2))
        s"($m1 / $m2)"
      case Expo(e1, e2) =>
        val (m1, m2) = parallel(mostrarPar(e1), mostrarPar(e2))
        s"($m1 ^ $m2)"
      case Logaritmo(e1) => newton.mostrar(e1)
      case _ => newton.mostrar(e)
    }
  }
}
