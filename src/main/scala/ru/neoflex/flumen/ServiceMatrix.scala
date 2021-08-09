package ru.neoflex.flumen

import shapeless.{ HList, HNil }

sealed trait ServiceMatrix {
  type THIS <: ServiceMatrix
  type DIAGONAL_TYPE
  type SERVICE_WITH_INPUT_TYPE[I] <: ServicesWithInput[I]
  type SERVICES_WITH_OUTPUT_TYPE[O] <: ServicesWithOutput[O]
}

object NilServiceMatrix extends ServiceMatrix {
  type THIS                         = NilServiceMatrix.type
  type DIAGONAL_TYPE                = Nothing
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputNil[A]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputNil[A]
}

object ServiceMatrixCompose {
  def apply[TYPE, TAIL <: ServiceMatrix](
    tail: TAIL
  )(
    service: Service[TYPE, TYPE],
    conditions: Seq[Condition[TYPE]],
    servicesWitInput: tail.SERVICE_WITH_INPUT_TYPE[TYPE],
    servicesWithOutput: tail.SERVICES_WITH_OUTPUT_TYPE[TYPE]
  ) = {

    InnerServiceMatrixCompose(tail, service, conditions, servicesWitInput, servicesWithOutput)
  }

  final case class InnerServiceMatrixCompose[
    TYPE,
    TAIL <: ServiceMatrix,
    TAIL_SERVICES_WITH_INPUT_TYPE <: ServicesWithInput[TYPE],
    TAIL_SERVICES_WITH_OUTPUT_TYPE <: ServicesWithOutput[TYPE]
  ](
    tail: TAIL,
    service: Service[TYPE, TYPE],
    conditions: Seq[Condition[TYPE]],
    serviceWithInput: TAIL_SERVICES_WITH_INPUT_TYPE,
    servicesWithOutput: TAIL_SERVICES_WITH_OUTPUT_TYPE)
      extends ServiceMatrix {
    type THIS                         = InnerServiceMatrixCompose[TYPE, TAIL, TAIL_SERVICES_WITH_INPUT_TYPE, TAIL_SERVICES_WITH_OUTPUT_TYPE]
    type DIAGONAL_TYPE                = TYPE
    type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputCompose[A, DIAGONAL_TYPE, tail.SERVICE_WITH_INPUT_TYPE[A]]
    type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputCompose[DIAGONAL_TYPE, A, tail.SERVICES_WITH_OUTPUT_TYPE[A]]

    def enlarge[NEXT_TYPE](
      nextservicesWithInput: this.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE],
      nextServicesWithOutput: this.SERVICES_WITH_OUTPUT_TYPE[NEXT_TYPE],
      services: Service[NEXT_TYPE, NEXT_TYPE],
      conditions: Seq[Condition[NEXT_TYPE]] = Seq.empty[Condition[NEXT_TYPE]]
    ) = {

      ServiceMatrixCompose(this)(services, conditions, nextservicesWithInput, nextServicesWithOutput)
    }
    def get[I, O](implicit get: MatrixGet[I, O, THIS]): Service[I, O] = get(this)

    // rows = гетерогенный список совариантных сервисов, у которых первый параметры имеет вид Service[H,A]
    /*   protected def appendRuntimeData[R<:HList, T<: HList](runtime: R, rows: Seq[CovServiceList[]] ) = {
              ???
       }*/

    def toRouterRuntime(): HList = {
      val services    = this.serviceWithInput.append(this.service)
      val runtimeData = TypeInfo[TYPE](Seq.empty[Variable[TYPE]], services, this.conditions)
      val runtime     = runtimeData :: HNil
      //Надо из serviceRows сделать HList контрвариантных списков
      // val columns = this.servicesRow.toHList
      (1 :: 232 :: "strtr" :: HNil)
      val runtimeConstructor = new RuntimeConstructor(runtime)
      ???
    }
  }
}

object ServiceMatrix {
  def apply[W](service: Service[W, W], condition: Condition[W]) = {
    ServiceMatrixCompose(NilServiceMatrix)(service, Seq(condition), ServicesWithInputNil[W], ServicesWithOutputNil[W])
  }
  def apply[W](service: Service[W, W]) = {
    ServiceMatrixCompose(NilServiceMatrix)(
      service,
      Seq.empty[Condition[W]],
      ServicesWithInputNil[W],
      ServicesWithOutputNil[W]
    )
  }
}
