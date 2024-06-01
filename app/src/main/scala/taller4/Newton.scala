package taller4

import scala.annotation.tailrec

trait Expr
case class Numero (d:Double) extends Expr
case class Atomo (x:Char) extends Expr
case class Suma(e1:Expr, e2:Expr) extends Expr
case class Prod(e1:Expr, e2:Expr) extends Expr
case class Resta(e1:Expr, e2:Expr) extends Expr
case class Div(e1:Expr, e2:Expr) extends Expr
case class Expo(e1:Expr, e2:Expr) extends Expr
case class Logaritmo(e1:Expr) extends Expr

class Newton {
  def mostrar(e: Expr): String = {
    e match {
      case Numero(d) => s"$d"
      case Atomo(x) => s"$x"
      case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
      case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
      case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
      case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
      case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
      case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"
      case _ => throw new IllegalArgumentException("No se encontro una expresión valida")
    }
  }
  
  def derivar(f: Expr, a: Atomo): Expr = {
    f match {
      case Numero(d) => Numero(0)
      case Atomo(x) =>
        if(x == a.x) Numero(1)
        else Numero(0)
      case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
      case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
      case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
      case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
      case Logaritmo(e1) => Div(derivar(e1, a), e1)
      case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Div(Prod(derivar(e1, a), e2), e1), Prod(derivar(e2, a), Logaritmo(e1))))
      case _ => throw new IllegalArgumentException("No se encontro una expresión valida")
    }
  }
    
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) =>
      if (x == a.x) v
      else throw new IllegalArgumentException(s"Se encontró un átomo inesperado $x")
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Div(e1, e2) =>
      if (evaluar(e2, a, v) == 0) throw new ArithmeticException("No se puede dividir por cero")
      else evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
    case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
  }

  def limpiar(f: Expr): Expr = f match {
    case Numero(d) => Numero(d)
    case Atomo(x) => Atomo(x)

    case Suma(Numero(0), e2) => limpiar(e2)
    case Suma(e1, Numero(0)) => limpiar(e1)
    case Suma(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)

      (l1, l2) match {
        case (Numero(0), l2) => l2
        case (l1, Numero(0)) => l1
        case (l1, l2) => Suma(l1, l2)
      }

    case Resta(Numero(0), e2) => Prod(Numero(-1), limpiar(e2))
    case Resta(e1, Numero(0)) => limpiar(e1)
    case Resta(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)

      (l1, l2) match {
        case (Numero(0), l2) => Prod(Numero(-1), l2)
        case (l1, Numero(0)) => l1
        case (l1, l2) => if (l1 == l2) Numero(0) else Resta(l1, l2)
      }

    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(Numero(1), e2) => limpiar(e2)
    case Prod(e1, Numero(1)) => limpiar(e1)
    case Prod(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)

      (l1, l2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), l2) => l2
        case (l1, Numero(1)) => l1
        case (l1, l2) => Prod(l1, l2)
      }

    case Div(e1, Numero(1)) => limpiar(e1)
    case Div(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)

      (l1, l2) match {
        case (l1, Numero(1)) => l1
        case (l1, l2) => Div(l1, l2)
      }
    case Div(_, Numero(0)) => throw new ArithmeticException("La división por cero no es válida")

    case Expo(_, Numero(0)) => Numero(1)
    case Expo(e1, Numero(1)) => limpiar(e1)
    case Expo(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)

      (l1, l2) match {
        case (_, Numero(0)) => Numero(1)
        case (l1, Numero(1)) => l1
        case (l1, l2) => Expo(l1, l2)
      }

    case Logaritmo(Numero(1)) => Numero(0)
    case Logaritmo(e1) =>
      val l1 = limpiar(e1)

      l1 match {
        case Numero(1) => Numero(0)
        case l1 => Logaritmo(l1)
      }
    case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
  }

  @tailrec
  final def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean, acc: Int = 0): Double = {
    if (ba(f, a, x0) || acc >= 1000000) x0
    else {
      val evaluacion = evaluar(f, a, x0)
      val devaluacion = evaluar(limpiar(derivar(f, a)), a, x0)
      val nuevopunto = x0 - (evaluacion/devaluacion)
      raizNewton(f, a, nuevopunto, ba, acc + 1)
    }
  }

  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    Math.abs(evaluar(f, a, d)) < 0.0001
  }
}
