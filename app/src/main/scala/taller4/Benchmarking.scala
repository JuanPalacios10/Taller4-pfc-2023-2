package taller4

import org.scalameter._
import taller4.Taller4.saludo
class Benchmarking {
  val newton = new Newton()
  val newtonPar = new NewtonParalela()

  val expr = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Prod(Numero(5), Atomo('x'))), Numero(6))
  val expr1 = Resta(Prod(Atomo('x'), Expo(Atomo('x'), Numero(3))), Div(Numero(18), Suma(Atomo('x'), Numero(2))))
  val expr2 = Suma(Prod(Atomo('x'), Expo(Numero(3), Atomo('x'))), Resta(Numero(12), Atomo('x')))
  val expr3 = Resta(Prod(Atomo('x'), Atomo('x')), Div(Atomo('x'), Expo(Atomo('x'), Numero(2))))
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


  def comparacionDerivar(): Unit = {

    val seq = withWarmer(new Warmer.Default) measure {
      newton.derivar(expr28, Atomo('x'))
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.derivarPar(15)(expr28, Atomo('x'))
    }

    println(s"Tiempo de la derivada secuencial (seq): ${seq.value}," +
      s" Tiempo de la derivada paralela (par): ${par.value}," +
      s" Diferencia: ${seq.value / par.value}")
  }
  def comparacionMostrar(): Unit = {

    val seq = withWarmer(new Warmer.Default) measure {
      newton.mostrar(expr30)
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.mostrarPar(15)(expr30)
    }

    println(s"Tiempo de mostrar secuencial (seq): ${seq.value}," +
      s" Tiempo de la mostrar paralela (par): ${par.value}," +
      s" Diferencia: ${seq.value / par.value}")
  }
  def comparacionLimpiar(): Unit = {

    val seq = withWarmer(new Warmer.Default) measure {
      newton.limpiar(expr30)
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.limpiarPar(15)(expr30)
    }

    println(s"Tiempo de limpiar secuencial (seq): ${seq.value}," +
      s" Tiempo de la limpiar paralela (par): ${par.value}," +
      s" Diferencia: ${seq.value / par.value}")
  }
  def comparacionEvaluar(): Unit = {

    val seq = withWarmer(new Warmer.Default) measure {
      newton.evaluar(expr30, Atomo('x'), 2.0)
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.evaluarPar(15)(expr30, Atomo('x'), 2.0)
    }

    println(s"Tiempo de evaluar secuencial (seq): ${seq.value}," +
      s" Tiempo de la evaluar paralela (par): ${par.value}," +
      s" Diferencia: ${seq.value / par.value}")
  }
  def comparacionNewton(): Unit = {

    val seq = withWarmer(new Warmer.Default) measure {
      newton.raizNewton(expr5, Atomo('x'), 600, newton.buenaAprox)
    }

    val par = withWarmer(new Warmer.Default) measure {
      newtonPar.raizNewtonPar(expr5, Atomo('x'), 600, newton.buenaAprox)
    }

    println(s"Tiempo del metodo de newton secuencial (seq): ${seq.value}," +
      s" Tiempo del metodo de newton paralelo (par): ${par.value}," +
      s" Diferencia: ${seq.value / par.value}")
  }
}

