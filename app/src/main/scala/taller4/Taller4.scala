/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4{

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    val newton = new Newton()
    val newtonPar = new NewtonParalela()
    /*val expr1 = Suma(Atomo('x') , Numero(2))
    val expr2 = Prod(Atomo('x') , Atomo('x'))
    val expr3 = Suma(expr1 , Expo(expr2 ,Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1 , expr2) , Resta(expr3 , expr4))
    val expr6 = Expo(Atomo('x') ,Numero(3))
    val expr7 = Suma(expr1, Atomo('y'))*/
    val expr1 = Resta(Prod(Numero(6), Expo(Numero(2), Numero(3))), Div(Numero(18), Suma(Numero(5), Numero(2))))
    val expr2 = Suma(Prod(Numero(8), Expo(Numero(3), Numero(2))), Resta(Numero(12), Numero(4)))
    val expr3 = Resta(Prod(Numero(2), Numero(5)), Div(Numero(15), Expo(Numero(3), Numero(2))))
    val expr4 = Div(expr1, Suma(expr2, expr3))
    val expr5 = Prod(expr1, Div(expr2, Resta(expr3, expr4)))
    val expr6 = Suma(expr5, Resta(expr4, Prod(expr3, Div(expr2, expr1))))
    val expr7 = Prod(expr6, Expo(expr5, expr4))
    val expr8 = Div(expr7, Suma(expr6, expr5))
    val expr9 = Resta(expr8, expr7)
    val expr10 = Suma(expr9, expr8)
    val expr11 = Div(expr10, expr9)
    val expr12 = Prod(expr11, expr10)
    val expr13 = Resta(expr12, expr11)
    val expr14 = Suma(expr13, expr12)
    val expr15 = Div(expr14, expr13)
    val expr16 = Prod(expr15, expr14)
    val expr17 = Resta(expr16, expr15)
    val expr18 = Suma(expr17, expr16)
    val expr19 = Div(expr18, expr17)
    val expr20 = Prod(expr19, expr18)
    val expr21 = Resta(expr20, expr19)
    val expr22 = Prod(expr21, expr20)
    val expr23 = Div(expr22, expr21)
    val expr24 = Suma(expr23, expr22)
    val expr25 = Resta(expr24, expr23)
    val expr26 = Prod(expr25, expr24)
    val expr27 = Div(expr26, expr25)
    val expr28 = Suma(expr27, expr26)
    val expr29 = Resta(expr28, expr27)
    val expr30 = Prod(expr29, expr28)

    println(saludo())

    val seq = withWarmer(new Warmer.Default) measure {
      newton.limpiar(expr30)
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.limpiarPar(expr30)
    }

    println(seq, par, seq.value / par.value)
  }
 }
