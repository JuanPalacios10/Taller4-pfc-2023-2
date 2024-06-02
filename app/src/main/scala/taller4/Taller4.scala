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
    val expr1 = Suma(Atomo('x') , Numero(2))
    val expr2 = Prod(Atomo('x') , Atomo('x'))
    val expr3 = Suma(expr1 , Expo(expr2 ,Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1 , expr2) , Resta(expr3 , expr4))
    val expr6 = Expo(Atomo('x') ,Numero(3))

    println(saludo())

    val seq = withWarmer(new Warmer.Default) measure {
      newton.mostrar(expr6)
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.mostrarPar(expr6)
    }

    println(seq, par, seq.value / par.value)
  }
 }
