/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4


object Taller4{

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    val benchmarking = new Benchmarking()
    benchmarking.comparacionMostrar()
    benchmarking.comparacionDerivar()
    benchmarking.comparacionEvaluar()
    benchmarking.comparacionLimpiar()
    benchmarking.comparacionNewton()

  }
 }
