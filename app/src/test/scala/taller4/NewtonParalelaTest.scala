package taller4

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.funsuite.AnyFunSuiteLike
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonParalelaTest extends AnyFunSuiteLike {
  private val newton = new Newton()
  private val newtonPar = new NewtonParalela()
  private val expr1 = Suma(Atomo('x') , Numero(2))
  private val expr2 = Prod(Atomo('x') , Atomo('x'))
  private val expr3 = Suma(expr1 , Expo(expr2 ,Numero(5)))
  private val expr4 = Resta(Prod(Atomo('x'), Logaritmo(Atomo('x'))), Atomo('x'))
  private val expr5 = Prod(Div(expr1 , expr2) , Resta(expr3 , expr4))
  private val expr6 = Prod(Expo(Atomo('x') ,Numero(3)), expr5)
  private val expr7 = Suma(Resta(expr2, Prod(Numero(5), Atomo('x'))), Numero(6))
  private val expr8 = Suma(Prod(expr2, Numero(0.0)), Numero(0.0))

  test("Función Mostrar Paralelo") {
    assert(newtonPar.mostrarPar(15)(expr2) == "(x * x)")
    assert(newtonPar.mostrarPar(15)(expr3) == "((x + 2.0) + ((x * x) ^ 5.0))")
    assert(newtonPar.mostrarPar(15)(expr4) == "((x * (lg(x))) - x)")
    assert(newtonPar.mostrarPar(15)(expr5) == "(((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - ((x * (lg(x))) - x)))")
    assert(newtonPar.mostrarPar(15)(expr6) == "((x ^ 3.0) * (((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - ((x * (lg(x))) - x))))")
  }

  test("Función Derivar Paralelo") {
    assert(newtonPar.derivarPar(15)(expr2, Atomo('x')) == Suma(Prod(Numero(1.0), Atomo('x')), Prod(Atomo('x'), Numero(1.0))))
    assert(newtonPar.derivarPar(15)(expr1, Atomo('y')) == Suma(Numero(0.0), Numero(0.0)))
    assert(newtonPar.derivarPar(15)(expr3, Atomo('x')) == Suma(Suma(Numero(1.0), Numero(0.0)), Prod(Expo(Prod(Atomo('x'), Atomo('x')), Numero(5.0)), Suma(Div(Prod(Suma(Prod(Numero(1.0), Atomo('x')), Prod(Atomo('x'), Numero(1.0))), Numero(5.0)), Prod(Atomo('x'), Atomo('x'))), Prod(Numero(0.0), Logaritmo(Prod(Atomo('x'), Atomo('x'))))))))
    assert(newtonPar.derivarPar(15)(expr4, Atomo('x')) == Resta(Suma(Prod(Numero(1.0), Logaritmo(Atomo('x'))), Prod(Atomo('x'), Div(Numero(1.0), Atomo('x')))), Numero(1.0)))
    assert(newtonPar.derivarPar(15)(expr7, Atomo('x')) == Suma(Resta(Suma(Prod(Numero(1.0), Atomo('x')), Prod(Atomo('x'), Numero(1.0))), Suma(Prod(Numero(0.0), Atomo('x')), Prod(Numero(5.0), Numero(1.0)))), Numero(0.0)))
  }

  test("Función Evaluar Paralelo") {
    assert(newtonPar.evaluarPar(15)(expr2, Atomo('x'), 5.0) == 25.0)
    assert(newtonPar.evaluarPar(15)(expr3, Atomo('x'), 2.0) == 1028.0)
    assert(newtonPar.evaluarPar(15)(expr4, Atomo('x'), 2.0) == ((2.0 * math.log(2.0)) - 2.0))
    assert(newtonPar.evaluarPar(15)(expr7, Atomo('x'), 3.0) == 0.0)
    assert(newtonPar.evaluarPar(15)(expr5, Atomo('x'), 1.0) == 15.0)
  }

  test("Función Limpiar Paralela") {
    assert(newtonPar.limpiarPar(15)(expr8) == Numero(0.0))
    assert(newtonPar.limpiarPar(15)(newtonPar.derivarPar(15)(expr2, Atomo('x'))) == Suma(Atomo('x'), Atomo('x')))
    assert(newtonPar.limpiarPar(15)(newtonPar.derivarPar(15)(expr7, Atomo('x'))) == Resta(Suma(Atomo('x'), Atomo('x')), Numero(5.0)))
    assert(newtonPar.limpiarPar(15)(newtonPar.derivarPar(15)(expr4, Atomo('x'))) == Resta(Suma(Logaritmo(Atomo('x')), Prod(Atomo('x'), Div(Numero(1.0), Atomo('x')))), Numero(1.0)))
    assert(newtonPar.limpiarPar(15)(newtonPar.derivarPar(15)(expr3, Atomo('x'))) == Suma(Numero(1.0), Prod(Expo(Prod(Atomo('x'), Atomo('x')), Numero(5.0)), Div(Prod(Suma(Atomo('x'), Atomo('x')), Numero(5.0)), Prod(Atomo('x'), Atomo('x'))))))
  }

  test("Función raizNewton Paralela") {
    newtonPar.raizNewtonPar(expr2, Atomo('x'), 100, newton.buenaAprox).map(x => assert(x == 0.0244140625))
    newtonPar.raizNewtonPar(expr7, Atomo('x'), 100, newton.buenaAprox).map(x => assert(x == 3.000027464159137))
    newtonPar.raizNewtonPar(expr7, Atomo('x'), -100, newton.buenaAprox).map(x => assert(x == 1.9999541565075942))
    newtonPar.raizNewtonPar(expr4, Atomo('x'), 100, newton.buenaAprox).map(x => assert(x == 2.71828226330606))
    newtonPar.raizNewtonPar(expr6, Atomo('x'), 100, newton.buenaAprox).map(x => assert(x == 7.546086478652566E-5))
  }
}
