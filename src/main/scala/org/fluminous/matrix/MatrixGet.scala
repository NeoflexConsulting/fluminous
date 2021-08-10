package org.fluminous.matrix

trait MatrixGet[I, O, M <: ServiceMatrix] {
  def apply(t: M): Service[I, O]
}
object MatrixGet {
  def apply[I, O, M <: ServiceMatrix](implicit got: MatrixGet[I, O, M]): MatrixGet[I, O, M] = got

  implicit def selectFromServicesWithInput[
    I,
    O,
    TM <: ServiceMatrix,
    SERVICES_WITH_INPUT_TYPE[I] <: ServicesWithInput[I],
    SERVICES_WITH_OUTPUT_TYPE[O] <: ServicesWithOutput[O]
  ](implicit c: GetServiceByOutput[I, O, SERVICES_WITH_INPUT_TYPE[I]]
  ): MatrixGet[I, O, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, I, TM]] = {
    new MatrixGet[I, O, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, I, TM]] {
      override def apply(
        t: ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, I, TM]
      ): Service[I, O] = c(t.servicesWithInput)
    }
  }

  implicit def selectFromServicesWithOutput[
    I,
    O,
    TM <: ServiceMatrix,
    SERVICES_WITH_INPUT_TYPE[I] <: ServicesWithInput[I],
    SERVICES_WITH_OUTPUT_TYPE[O] <: ServicesWithOutput[O]
  ](implicit c: GetServiceByInput[I, O, SERVICES_WITH_OUTPUT_TYPE[O]]
  ): MatrixGet[I, O, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, O, TM]] =
    new MatrixGet[I, O, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, O, TM]] {
      def apply(t: ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, O, TM]): Service[I, O] =
        c(t.servicesWithOutput)
    }

  //Когда у нас M <: ServiceMatrix,M.Diagonal,M.diagonal
  implicit def selectFromDiagonal[
    M <: ServiceMatrix,
    H,
    SERVICES_WITH_INPUT_TYPE[H] <: ServicesWithInput[H],
    SERVICES_WITH_OUTPUT_TYPE[H] <: ServicesWithOutput[H]
  ]: MatrixGet[H, H, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, H, M]] =
    new MatrixGet[H, H, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, H, M]] {
      def apply(t: ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, H, M]): Service[H, H] =
        t.service
    }

  //И рекурсивный случай, перебирающий матрицы
  implicit def recurse[
    I,
    O,
    TM <: ServiceMatrix,
    H,
    SERVICES_WITH_INPUT_TYPE[H] <: ServicesWithInput[H],
    SERVICES_WITH_OUTPUT_TYPE[H] <: ServicesWithOutput[H]
  ](implicit mt: MatrixGet[I, O, TM]
  ): MatrixGet[I, O, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, H, TM]] =
    new MatrixGet[I, O, ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, H, TM]] {
      def apply(t: ServiceMatrixCompose[SERVICES_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, H, TM]): Service[I, O] =
        mt(t.tail)
    }
}
