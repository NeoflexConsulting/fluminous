package ru.neoflex.flumen

import ru.neoflex.flumen.ServiceMatrixCompose.InnerServiceMatrixCompose
class NotFoundException extends Exception

// TODO Implement Unique typeclasses

trait MatrixGet[I, O, M <: ServiceMatrix] {
  def apply(t: M): Service[I, O]
}
object MatrixGet {
  def apply[I, O, M <: ServiceMatrix](implicit got: MatrixGet[I, O, M]): MatrixGet[I, O, M] = got

  implicit def selectContrvariant[
    I,
    O,
    TM <: ServiceMatrix,
    CONTR <: ServicesWithInput[I],
    COV <: ServicesWithOutput[I]
  ](implicit c: GetServiceByOutput[I, O, CONTR]
  ): MatrixGet[I, O, InnerServiceMatrixCompose[I, TM, CONTR, COV]] = {
    new MatrixGet[I, O, InnerServiceMatrixCompose[I, TM, CONTR, COV]] {
      override def apply(t: InnerServiceMatrixCompose[I, TM, CONTR, COV]): Service[I, O] = c(t.serviceWithInput)
    }
  }
  //Когда у нас M <: ServiceMatrix,I,O и есть implicit CovGet[M.CovSignature, I,O]
  implicit def selectCovariant[I, O, TM <: ServiceMatrix, CONTR <: ServicesWithInput[O], COV <: ServicesWithOutput[O]](
    implicit c: GetServiceByInput[I, O, COV]
  ): MatrixGet[I, O, InnerServiceMatrixCompose[O, TM, CONTR, COV]] =
    new MatrixGet[I, O, InnerServiceMatrixCompose[O, TM, CONTR, COV]] {
      def apply(t: InnerServiceMatrixCompose[O, TM, CONTR, COV]): Service[I, O] = c(t.servicesWithOutput)
    }

  //Когда у нас M <: ServiceMatrix,M.Diagonal,M.diagonal
  implicit def selectDiagonal[
    M <: ServiceMatrix,
    H,
    CONTR <: ServicesWithInput[H],
    COV <: ServicesWithOutput[H]
  ]: MatrixGet[H, H, InnerServiceMatrixCompose[H, M, CONTR, COV]] =
    new MatrixGet[H, H, InnerServiceMatrixCompose[H, M, CONTR, COV]] {
      def apply(t: InnerServiceMatrixCompose[H, M, CONTR, COV]): Service[H, H] = t.service
    }

  //И рекурсивный случай, перебирающий матрицы
  implicit def recurse[M <: ServiceMatrix, H, CONTR <: ServicesWithInput[H], COV <: ServicesWithOutput[H], I, O](
    implicit mt: MatrixGet[I, O, M]
  ): MatrixGet[I, O, InnerServiceMatrixCompose[H, M, CONTR, COV]] =
    new MatrixGet[I, O, InnerServiceMatrixCompose[H, M, CONTR, COV]] {
      def apply(t: InnerServiceMatrixCompose[H, M, CONTR, COV]): Service[I, O] = mt(t.tail)
    }
}

object Test {
  def main(args: Array[String]): Unit = {
    val upperCaseService = Service[String, String]("upper", _.toUpperCase)
    val incrementService = Service[Int, Int]("increment", _ + 1)
    val toStringService  = Service[Int, String]("to_string", _.toString)
    val toIntService     = Service[String, Int]("to_int", _.toInt)

    val serviceMatrix = ServiceMatrix(upperCaseService)
    val bigServiceMatrix =
      serviceMatrix.enlarge(ServicesWithInput(toStringService), ServicesWithOutput(toIntService), incrementService)

    println(serviceMatrix.get[String, String].invoke("some String from Service Matrix1"))
    println(bigServiceMatrix.get[Int, Int].invoke(3))
    println(bigServiceMatrix.get[Int, String].invoke(3))
    println(bigServiceMatrix.get[String, Int].invoke("3"))
    println(bigServiceMatrix.get[String, String].invoke("some String"))
  }
}
