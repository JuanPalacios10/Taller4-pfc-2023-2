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

}
