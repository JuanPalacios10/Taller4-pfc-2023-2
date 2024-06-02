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

  def evaluarPar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(_) => newton.evaluar(f, a, v)
    case Atomo(_) => newton.evaluar(f, a, v)
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
      val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
      if (ev2 == 0) throw new ArithmeticException("No se puede dividir por cero")
      else ev1 / ev2
    case Expo(e1, e2) =>
      val (ev1, ev2) = parallel(evaluarPar(e1, a, v), evaluarPar(e2, a, v))
      math.pow(ev1, ev2)
    case Logaritmo(_) => newton.evaluar(f, a, v)
    case _ => newton.evaluar(f, a, v)
  }
  def limpiarPar(f: Expr): Expr = f match {
    case Numero(_) => newton.limpiar(f)
    case Atomo(_) => newton.limpiar(f)

    case Suma(e1, e2) =>
      val (l1, l2) = parallel(limpiarPar(e1), limpiarPar(e2))

      (l1, l2) match {
        case (Numero(0), l2) => l2
        case (l1, Numero(0)) => l1
        case (l1, l2) => Suma(l1, l2)
      }
    case Resta(e1, e2) =>
      val (l1, l2) = parallel(limpiarPar(e1), limpiarPar(e2))

      (l1,l2) match {
        case (Numero(0), l2) => Prod(Numero(-1), l2)
        case (l1, Numero(0)) => l1
        case (l1, l2) => if(l1==l2) Numero(0) else Resta(l1, l2)
      }
    case Prod(e1, e2) =>
      val (l1, l2) = parallel(limpiarPar(e1), limpiarPar(e2))

      (l1, l2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), l2) => l2
        case (l1, Numero(1)) => l1
        case (l1,l2) => Prod(l1, l2)
      }
    case Div(e1, e2) =>
      val (l1, l2) = parallel(limpiarPar(e1), limpiarPar(e2))

      (l1, l2) match {
        case (l1, Numero(1)) => l1
        case (_, Numero(0)) => throw new ArithmeticException("No se puede dividir por cero")
        case (l1, l2) => Div(l1, l2)
      }
    case Expo(e1, e2) =>
      val (l1, l2) = parallel(limpiarPar(e1), limpiarPar(e2))

      (l1, l2) match {
        case (_, Numero(0)) => Numero(1)
        case (_, Numero(1)) => l1
        case(l1, l2) => Expo(l1, l2)
      }
    case Logaritmo(_) => newton.limpiar(f)
    case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
  }
}
