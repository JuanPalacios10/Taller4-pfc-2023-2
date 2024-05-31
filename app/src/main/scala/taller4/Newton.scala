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
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
    case _ => throw new IllegalArgumentException("No se conoce el tipo de expresión")
  }
}
