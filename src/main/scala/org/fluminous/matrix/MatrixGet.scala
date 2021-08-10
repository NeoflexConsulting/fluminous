package org.fluminous.matrix

import ServiceMatrixCompose.InnerServiceMatrixCompose

trait MatrixGet[I, O, M <: ServiceMatrix] {
  def apply(t: M): Service[I, O]
}
object MatrixGet {
  def apply[I, O, M <: ServiceMatrix](implicit got: MatrixGet[I, O, M]): MatrixGet[I, O, M] = got

  implicit def selectFromServicesWithInput[
    I,
    O,
    TM <: ServiceMatrix,
    SERVICES_WITH_INPUT_TYPE <: ServicesWithInput[I],
    SERVICES_WITH_OUTPUT_TYPE <: ServicesWithOutput[I]
  ](implicit c: GetServiceByOutput[I, O, SERVICES_WITH_INPUT_TYPE]
  ): MatrixGet[I, O, InnerServiceMatrixCompose[I, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] = {
    new MatrixGet[I, O, InnerServiceMatrixCompose[I, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] {
      override def apply(
        t: InnerServiceMatrixCompose[I, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]
      ): Service[I, O] = c(t.serviceWithInput)
    }
  }

  implicit def selectFromServicesWithOutput[
    I,
    O,
    TM <: ServiceMatrix,
    SERVICES_WITH_INPUT_TYPE <: ServicesWithInput[O],
    SERVICES_WITH_OUTPUT_TYPE <: ServicesWithOutput[O]
  ](implicit c: GetServiceByInput[I, O, SERVICES_WITH_OUTPUT_TYPE]
  ): MatrixGet[I, O, InnerServiceMatrixCompose[O, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] =
    new MatrixGet[I, O, InnerServiceMatrixCompose[O, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] {
      def apply(
        t: InnerServiceMatrixCompose[O, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]
      ): Service[I, O] = c(t.servicesWithOutput)
    }

  //Когда у нас M <: ServiceMatrix,M.Diagonal,M.diagonal
  implicit def selectFromDiagonal[
    M <: ServiceMatrix,
    H,
    SERVICES_WITH_INPUT_TYPE <: ServicesWithInput[H],
    SERVICES_WITH_OUTPUT_TYPE <: ServicesWithOutput[H]
  ]: MatrixGet[H, H, InnerServiceMatrixCompose[H, M, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] =
    new MatrixGet[H, H, InnerServiceMatrixCompose[H, M, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] {
      def apply(
        t: InnerServiceMatrixCompose[H, M, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]
      ): Service[H, H] = t.service
    }

  //И рекурсивный случай, перебирающий матрицы
  implicit def recurse[
    I,
    O,
    TM <: ServiceMatrix,
    H,
    SERVICES_WITH_INPUT_TYPE <: ServicesWithInput[H],
    SERVICES_WITH_OUTPUT_TYPE <: ServicesWithOutput[H]
  ](implicit mt: MatrixGet[I, O, TM]
  ): MatrixGet[I, O, InnerServiceMatrixCompose[H, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] =
    new MatrixGet[I, O, InnerServiceMatrixCompose[H, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]] {
      def apply(
        t: InnerServiceMatrixCompose[H, TM, SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE]
      ): Service[I, O] = mt(t.tail)
    }
}
