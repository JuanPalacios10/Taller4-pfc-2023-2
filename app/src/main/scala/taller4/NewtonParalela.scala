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
      case Logaritmo(_) => newton.mostrar(e)
      case _ => newton.mostrar(e)
    }
  }

  def derivarPar(f: Expr, a: Atomo): Expr = {
    f match {
      case Numero(_) => newton.derivar(f, a)
      case Atomo(_) => newton.derivar(f, a)
      case Suma(e1, e2) =>
        val (d1, d2) = parallel(derivarPar(e1, a), derivarPar(e2, a))
        Suma(d1, d2)
      case Resta(e1, e2) =>
        val (d1, d2) = parallel(derivarPar(e1, a), derivarPar(e2, a))
        Resta(d1, d2)
      case Prod(e1, e2) =>
        val (d1, d2) = parallel(derivarPar(e1, a), derivarPar(e2, a))
        Suma(Prod(d1, e2), Prod(e1, d2))
      case Div(e1, e2) =>
        val (d1, d2) = parallel(derivarPar(e1, a), derivarPar(e2, a))
        Div(Resta(Prod(d1, e2), Prod(e1, d2)), Expo(e2, Numero(2)))
      case Logaritmo(_) => newton.derivar(f, a)
      case Expo(e1, e2) =>
        val (d1, d2) = parallel(derivarPar(e1, a), derivarPar(e2, a))
        Prod(Expo(e1, e2), Suma(Div(Prod(d1, e2), e1), Prod(d2, Logaritmo(e1))))
      case _ => newton.derivar(f, a)
    }
  }
}
