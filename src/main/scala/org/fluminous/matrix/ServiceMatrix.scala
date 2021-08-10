package org.fluminous.matrix

import org.fluminous.runtime.{Condition, ExecutionRuntime, TypeInfo}

import scala.reflect.ClassTag

sealed trait ServiceMatrix {
  type THIS <: ServiceMatrix
  type DIAGONAL_TYPE
  type SERVICE_WITH_INPUT_TYPE[I] <: ServicesWithInput[I]
  type SERVICES_WITH_OUTPUT_TYPE[O] <: ServicesWithOutput[O]

  def toExecutionRuntime: ExecutionRuntime = {
    val matrixRuntime = toTypeInfoSeq(Seq.empty)
    new ExecutionRuntime {
      override val runtime: Seq[TypeInfo] = matrixRuntime
    }
  }

  def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo]
}

object NilServiceMatrix extends ServiceMatrix {
  type THIS                         = NilServiceMatrix.type
  type DIAGONAL_TYPE                = Nothing
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputNil[A]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputNil[A]
  override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = current
}

object ServiceMatrixCompose {
  def apply[TYPE: ClassTag, TAIL <: ServiceMatrix](
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
    TYPE : ClassTag,
    TAIL <: ServiceMatrix,
    TAIL_SERVICES_WITH_INPUT_TYPE <: ServicesWithInput[TYPE],
    TAIL_SERVICES_WITH_OUTPUT_TYPE <: ServicesWithOutput[TYPE]
  ](
    tail: TAIL,
    service: Service[TYPE, TYPE],
    conditions: Seq[Condition[TYPE]],
    servicesWithInput: TAIL_SERVICES_WITH_INPUT_TYPE,
    servicesWithOutput: TAIL_SERVICES_WITH_OUTPUT_TYPE)
      extends ServiceMatrix {
    type THIS                         = InnerServiceMatrixCompose[TYPE, TAIL, TAIL_SERVICES_WITH_INPUT_TYPE, TAIL_SERVICES_WITH_OUTPUT_TYPE]
    type DIAGONAL_TYPE                = TYPE
    type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputCompose[A, DIAGONAL_TYPE, tail.SERVICE_WITH_INPUT_TYPE[A]]
    type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputCompose[DIAGONAL_TYPE, A, tail.SERVICES_WITH_OUTPUT_TYPE[A]]

    def enlarge[NEXT_TYPE: ClassTag](
      nextservicesWithInput: this.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE],
      nextServicesWithOutput: this.SERVICES_WITH_OUTPUT_TYPE[NEXT_TYPE],
      services: Service[NEXT_TYPE, NEXT_TYPE],
      conditions: Seq[Condition[NEXT_TYPE]] = Seq.empty[Condition[NEXT_TYPE]]
    ) = {

      ServiceMatrixCompose(this)(services, conditions, nextservicesWithInput, nextServicesWithOutput)
    }
    def get[I, O](implicit get: MatrixGet[I, O, THIS]): Service[I, O] = get(this)

    override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = {
      //TODO Append services from servicesWithOutput
      val services = service +: servicesWithInput.toSeq
      val typeInfo = TypeInfo.forType[TYPE](services, conditions)
      tail.toTypeInfoSeq(typeInfo +: current)
    }
  }
}

object ServiceMatrix {
  def apply[W: ClassTag](service: Service[W, W], condition: Condition[W]) = {
    ServiceMatrixCompose(NilServiceMatrix)(service, Seq(condition), ServicesWithInputNil[W], ServicesWithOutputNil[W])
  }
  def apply[W:ClassTag](service: Service[W, W]) = {
    ServiceMatrixCompose(NilServiceMatrix)(
      service,
      Seq.empty[Condition[W]],
      ServicesWithInputNil[W],
      ServicesWithOutputNil[W]
    )
  }
}
