package org.fluminous.matrix

import org.fluminous.runtime.{ Condition, ExecutionRuntime, TypeInfo }

import scala.reflect.ClassTag

sealed trait ServiceMatrix {
  type THIS <: ServiceMatrix
  type L2D <: ServicesWithInput2DList
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
  type L2D                          = ServicesWithInput2DNil.type
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputNil[A]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputNil[A]
  override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = current
}

final case class ServiceMatrixCompose[
  SI[A] <: ServicesWithInput[A],
  SO[B] <: ServicesWithOutput[B],
  TYPE: ClassTag,
  TAIL <: ServiceMatrix
](
  tail: TAIL,
  service: Service[TYPE, TYPE],
  conditions: Seq[Condition[TYPE]],
  servicesWithInput: SI[TYPE],
  servicesWithOutput: SO[TYPE])
    extends ServiceMatrix {
  type THIS                         = ServiceMatrixCompose[SI, SO, TYPE, TAIL]
  type L2D                          = ServicesWithInput2DCompose[SI[TYPE], tail.L2D]
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputCompose[A, TYPE, tail.SERVICE_WITH_INPUT_TYPE[A]]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputCompose[TYPE, A, tail.SERVICES_WITH_OUTPUT_TYPE[A]]

  def appendType[NEXT_TYPE: ClassTag](
    nextservicesWithInput: this.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE],
    nextServicesWithOutput: this.SERVICES_WITH_OUTPUT_TYPE[NEXT_TYPE],
    services: Service[NEXT_TYPE, NEXT_TYPE],
    conditions: Seq[Condition[NEXT_TYPE]] = Seq.empty[Condition[NEXT_TYPE]]
  ): ServiceMatrixCompose[SERVICE_WITH_INPUT_TYPE, SERVICES_WITH_OUTPUT_TYPE, NEXT_TYPE, ServiceMatrixCompose[
    SI,
    SO,
    TYPE,
    TAIL
  ]] = {
    ServiceMatrixCompose(this, services, conditions, nextservicesWithInput, nextServicesWithOutput)
  }
  def get[I, O](implicit get: MatrixGet[I, O, THIS]): Service[I, O] = get(this)

  override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = {
    //TODO Append services from servicesWithOutput
    val services = service +: servicesWithInput.toSeq
    val typeInfo = TypeInfo.forType[TYPE](services, conditions)
    tail.toTypeInfoSeq(typeInfo +: current)
  }
}

object ServiceMatrix {
  type Aux[I[_] <: ServicesWithInput[_], O[_] <: ServicesWithOutput[_]] = ServiceMatrix {
    type SERVICE_WITH_INPUT_TYPE[A]   = I[A]
    type SERVICES_WITH_OUTPUT_TYPE[A] = O[A]
  }

  def apply[W: ClassTag](service: Service[W, W], condition: Condition[W]) = {
    ServiceMatrixCompose(NilServiceMatrix, service, Seq(condition), ServicesWithInputNil[W], ServicesWithOutputNil[W])
  }
  def apply[W: ClassTag](service: Service[W, W]) = {
    ServiceMatrixCompose(
      NilServiceMatrix,
      service,
      Seq.empty[Condition[W]],
      ServicesWithInputNil[W],
      ServicesWithOutputNil[W]
    )
  }
}
