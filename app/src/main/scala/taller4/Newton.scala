package taller4

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
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
    case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
  }

  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e2) => limpiar(e2)
    case Suma(e1, Numero(0)) => limpiar(e1)
    case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))

    case Resta(e1, Numero(0)) => limpiar(e1)
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))

    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(Numero(1), e2) => limpiar(e2)
    case Prod(e1, Numero(1)) => limpiar(e1)
    case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))

    case Div(e1, Numero(1)) => limpiar(e1)
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Div(_, Numero(0)) => throw new ArithmeticException("La división por cero no es válida")

    case Expo(_, Numero(0)) => Numero(1)
    case Expo(e1, Numero(1)) => limpiar(e1)
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))

    case Logaritmo(Numero(1)) => Numero(0)
    case Logaritmo(e1) => Logaritmo(limpiar(e1))

    case Numero(d) => Numero(d)
    case Atomo(x) => Atomo(x)
    case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
  }
}
