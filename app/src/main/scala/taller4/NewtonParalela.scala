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

  def evaluarPar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => newton.evaluar(f, a, v)
    case Atomo(x) => newton.evaluar(f,a, v)
    case Suma(e1, e2) =>
      val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
      ev1 + ev2
    case Resta(e1, e2) =>
      val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
      ev1 - ev2
    case Prod(e1, e2) =>
      val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
      ev1 * ev2
    case Div(e1, e2) =>
      if (evaluarPar(e2, a, v) == 0) newton.evaluar(f,a,v)
      else {
        val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
        ev1 / ev2
      }
    case Expo(e1, e2) =>
      val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
      math.pow(ev1,ev2)
    case Logaritmo(e1) => newton.evaluar(f,a,v)
    case _ => newton.evaluar(f,a,v)
  }
}
