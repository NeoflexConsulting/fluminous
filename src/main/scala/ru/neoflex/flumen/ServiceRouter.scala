package ru.neoflex.flumen

import ru.neoflex.flumen.ServiceMatrixCompose.InnerServiceMatrixCompose
import shapeless.{HList, HNil}
class NotFoundException extends Exception

// TODO Implement Unique typeclasses





//Класс типов, Умеет для матрицы M возвращать Service[A,U]
trait MatrixGet[M <: ServiceMatrix,I,O] {
  def apply(t: M): Service[I,O]
}
object MatrixGet {
  def apply[M <: ServiceMatrix,I,O](implicit got: MatrixGet[M,I,O]): MatrixGet[M,I,O] = got
  //У нас 3 базовых случая
  //Когда у нас M <: ServiceMatrix,I,O и есть implicit ContrGet[M.ContrSignature, I,O]
  implicit def selectContrvariant[M <: ServiceMatrix,CONTR<: ServicesWithInput[I], COV <: ServicesWithOutput[I],I,O](implicit c: GetServiceByOutput[I,O,CONTR]):
  MatrixGet[InnerServiceMatrixCompose[I,M,CONTR,COV],I,O] = {
    new MatrixGet[InnerServiceMatrixCompose[I,M,CONTR,COV],I,O] {
      override def apply(t: InnerServiceMatrixCompose[I, M, CONTR, COV]): Service[I, O] = c(t.serviceWithInput)
    }
  }
  //Когда у нас M <: ServiceMatrix,I,O и есть implicit CovGet[M.CovSignature, I,O]
  implicit def selectCovariant[M <: ServiceMatrix,CONTR<: ServicesWithInput[O], COV <: ServicesWithOutput[O],I,O](implicit c: GetServiceByInput[I,O,COV]):
  MatrixGet[InnerServiceMatrixCompose[O,M,CONTR,COV],I,O] = new MatrixGet[InnerServiceMatrixCompose[O,M,CONTR,COV],I,O]{
    def apply(t : InnerServiceMatrixCompose[O,M,CONTR,COV]):Service[I,O] = c(t.servicesWithOutput)
  }

  //Когда у нас M <: ServiceMatrix,M.Diagonal,M.diagonal
  implicit def selectDiagonal[M <: ServiceMatrix,H, CONTR<: ServicesWithInput[H], COV <: ServicesWithOutput[H]]:
  MatrixGet[InnerServiceMatrixCompose[H,M,CONTR,COV],H,H] =
    new MatrixGet[InnerServiceMatrixCompose[H,M,CONTR,COV],H,H] {
      def apply(t : InnerServiceMatrixCompose[H,M,CONTR,COV]):Service[H,H] = t.service
    }

  //И рекурсивный случай, перебирающий матрицы
  implicit def recurse[M <: ServiceMatrix,H, CONTR<: ServicesWithInput[H], COV <: ServicesWithOutput[H], I,O]
  (implicit mt : MatrixGet[M, I, O]): MatrixGet[InnerServiceMatrixCompose[H,M,CONTR,COV],I,O] =
    new MatrixGet[InnerServiceMatrixCompose[H,M,CONTR,COV],I,O] {
      def apply(t :InnerServiceMatrixCompose[H,M,CONTR,COV]):Service[I,O]  = mt(t.tail)
    }
}


object Test {
  def main(args: Array[String]): Unit = {
    val upperCaseService = Service[String,String]("upper",_.toUpperCase)
    val incrementService = Service[Int,Int]("increment",_ + 1)
    val toStringService = Service[Int, String]("to_string",_.toString)
    val toIntService = Service[String,Int]("to_int",_.toInt)

    val serviceMatrix = ServiceMatrix(upperCaseService)
    val bigServiceMatrix = serviceMatrix.
          enlarge(ServicesWithInput(toStringService), ServicesWithOutput(toIntService),incrementService)

    println(serviceMatrix.get[String,String].invoke("some String from Service Matrix1"))
    println(bigServiceMatrix.get[Int,Int].invoke(3))
    println(bigServiceMatrix.get[Int,String].invoke(3))
    println(bigServiceMatrix.get[String,Int].invoke("3"))
    println(bigServiceMatrix.get[String,String].invoke("some String"))
  }
}
