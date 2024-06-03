package taller4

import common._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

class NewtonParalela {
  val newton = new Newton()

  def mostrarPar(maxProf: Int)(e: Expr, limite: Int = 0): String = {
    if (limite > maxProf) newton.mostrar(e)
    else {
      e match {
        case Numero(d) => s"$d"
        case Atomo(x) => s"$x"
        case Suma(e1, e2) =>
          val (m1, m2) = parallel(mostrarPar(maxProf)(e1, limite + 1), mostrarPar(maxProf)(e2, limite + 1))
          s"($m1 + $m2)"
        case Prod(e1, e2) =>
          val (m1, m2) = parallel(mostrarPar(maxProf)(e1, limite + 1), mostrarPar(maxProf)(e2, limite + 1))
          s"($m1 * $m2)"
        case Resta(e1, e2) =>
          val (m1, m2) = parallel(mostrarPar(maxProf)(e1, limite + 1), mostrarPar(maxProf)(e2, limite + 1))
          s"($m1 - $m2)"
        case Div(e1, e2) =>
          val (m1, m2) = parallel(mostrarPar(maxProf)(e1, limite + 1), mostrarPar(maxProf)(e2, limite + 1))
          s"($m1 / $m2)"
        case Expo(e1, e2) =>
          val (m1, m2) = parallel(mostrarPar(maxProf)(e1, limite + 1), mostrarPar(maxProf)(e2, limite + 1))
          s"($m1 ^ $m2)"
        case Logaritmo(_) => newton.mostrar(e)
        case _ => throw new IllegalArgumentException("No se encontro una expresión valida")
      }
    }
  }

  def derivarPar(maxProf: Int)(f: Expr, a: Atomo, limite: Int = 0): Expr = {
    if (limite > maxProf) newton.derivar(f, a)
    else {
      f match {
        case Numero(_) => Numero(0)
        case Atomo(x) => if(x == a.x) Numero(1) else Numero(0)
        case Suma(e1, e2) =>
          val (d1, d2) = parallel(derivarPar(maxProf)(e1, a, limite + 1), derivarPar(maxProf)(e2, a, limite + 1))
          Suma(d1, d2)
        case Resta(e1, e2) =>
          val (d1, d2) = parallel(derivarPar(maxProf)(e1, a, limite + 1), derivarPar(maxProf)(e2, a, limite + 1))
          Resta(d1, d2)
        case Prod(e1, e2) =>
          val (d1, d2) = parallel(derivarPar(maxProf)(e1, a, limite + 1), derivarPar(maxProf)(e2, a, limite + 1))
          Suma(Prod(d1, e2), Prod(e1, d2))
        case Div(e1, e2) =>
          val (d1, d2) = parallel(derivarPar(maxProf)(e1, a, limite + 1), derivarPar(maxProf)(e2, a, limite + 1))
          Div(Resta(Prod(d1, e2), Prod(e1, d2)), Expo(e2, Numero(2)))
        case Logaritmo(_) => newton.derivar(f, a)
        case Expo(e1, e2) =>
          val (d1, d2) = parallel(derivarPar(maxProf)(e1, a, limite + 1), derivarPar(maxProf)(e2, a, limite + 1))
          Prod(Expo(e1, e2), Suma(Div(Prod(d1, e2), e1), Prod(d2, Logaritmo(e1))))
        case _ => throw new IllegalArgumentException("No se encontro una expresión valida")
      }
    }
  }

  def evaluarPar(maxProf: Int)(f: Expr, a: Atomo, v: Double, limite: Int = 0): Double = {
    if (limite > maxProf) newton.evaluar(f, a, v)
    else {
      f match {
        case Numero(d) => d
        case Atomo(x) => if (x == a.x) v else throw new IllegalArgumentException(s"Se encontró un átomo inesperado $x")
        case Suma(e1, e2) =>
          val (ev1, ev2) = parallel(evaluarPar(maxProf)(e1, a, v, limite + 1), evaluarPar(maxProf)(e2, a, v, limite + 1))
          ev1 + ev2
        case Resta(e1, e2) =>
          val (ev1, ev2) = parallel(evaluarPar(maxProf)(e1, a, v, limite + 1), evaluarPar(maxProf)(e2, a, v, limite + 1))
          ev1 - ev2
        case Prod(e1, e2) =>
          val (ev1, ev2) = parallel(evaluarPar(maxProf)(e1, a, v, limite + 1), evaluarPar(maxProf)(e2, a, v, limite + 1))
          ev1 * ev2
        case Div(e1, e2) =>
          val (ev1, ev2) = parallel(evaluarPar(maxProf)(e1, a, v, limite + 1), evaluarPar(maxProf)(e2, a, v, limite + 1))
          if (ev2 == 0) throw new ArithmeticException("No se puede dividir por cero") else ev1 / ev2
        case Expo(e1, e2) =>
          val (ev1, ev2) = parallel(evaluarPar(maxProf)(e1, a, v, limite + 1), evaluarPar(maxProf)(e2, a, v, limite + 1))
          math.pow(ev1, ev2)
        case Logaritmo(_) => newton.evaluar(f, a, v)
        case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
      }
    }
  }

  def limpiarPar(maxProf: Int)(f: Expr, limite: Int = 0): Expr = {
    if (limite > maxProf) newton.limpiar(f)
    else {
      f match {
        case Numero(d) => Numero(d)
        case Atomo(x) => Atomo(x)

        case Suma(e1, e2) =>
          val (l1, l2) = parallel(limpiarPar(maxProf)(e1, limite + 1), limpiarPar(maxProf)(e2, limite + 1))

          (l1, l2) match {
            case (Numero(0), l2) => l2
            case (l1, Numero(0)) => l1
            case (l1, l2) => Suma(l1, l2)
          }

        case Resta(e1, e2) =>
          val (l1, l2) = parallel(limpiarPar(maxProf)(e1, limite + 1), limpiarPar(maxProf)(e2, limite + 1))

          (l1,l2) match {
            case (Numero(0), l2) => Prod(Numero(-1), l2)
            case (l1, Numero(0)) => l1
            case (l1, l2) => if(l1==l2) Numero(0) else Resta(l1, l2)
          }

        case Prod(e1, e2) =>
          val (l1, l2) = parallel(limpiarPar(maxProf)(e1, limite + 1), limpiarPar(maxProf)(e2, limite + 1))

          (l1, l2) match {
            case (Numero(0), _) => Numero(0)
            case (_, Numero(0)) => Numero(0)
            case (Numero(1), l2) => l2
            case (l1, Numero(1)) => l1
            case (l1,l2) => Prod(l1, l2)
          }

        case Div(e1, e2) =>
          val (l1, l2) = parallel(limpiarPar(maxProf)(e1, limite + 1), limpiarPar(maxProf)(e2, limite + 1))

          (l1, l2) match {
            case (l1, Numero(1)) => l1
            case (_, Numero(0)) => throw new ArithmeticException("No se puede dividir por cero")
            case (l1, l2) => Div(l1, l2)
          }

        case Expo(e1, e2) =>
          val (l1, l2) = parallel(limpiarPar(maxProf)(e1, limite + 1), limpiarPar(maxProf)(e2, limite + 1))

          (l1, l2) match {
            case (_, Numero(0)) => Numero(1)
            case (l1, Numero(1)) => l1
            case(l1, l2) => Expo(l1, l2)
          }

        case Logaritmo(Numero(1)) => Numero(0)
        case Logaritmo(e1) =>
          val l1 = newton.limpiar(e1)

          l1 match {
            case Numero(1) => Numero(0)
            case l1 => Logaritmo(l1)
          }

        case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
      }
    }
  }

  def raizNewtonPar(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Future[Double] = {
    (1 to 100).foldLeft(Future.successful(x0)) { (futureX, _) =>
      futureX.flatMap { x =>
        if (ba(f, a, x)) Future.successful(x)
        else {
          val fx = evaluarPar(15)(f, a, x)
          val dfx = evaluarPar(15)(limpiarPar(15)(derivarPar(15)(f, a)), a, x0)
          Future.successful(x - fx / dfx)
        }
      }
    }
  }
}
