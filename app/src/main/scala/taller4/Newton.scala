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
}
