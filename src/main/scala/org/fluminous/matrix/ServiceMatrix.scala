package org.fluminous.matrix

import org.fluminous.runtime.exception.{ ExecutionRuntimeException, NoServicesFoundException }
import org.fluminous.runtime.{ Condition, ExecutionRuntime, TypeInfo }

import scala.reflect.ClassTag

sealed trait ServiceMatrix {
  type THIS <: ServiceMatrix
  type L2D <: ServicesWithInput2DList
  type UPDATED_THIS[O] <: ServiceMatrix
  type SERVICE_WITH_INPUT_TYPE[I] <: ServicesWithInput[I]
  type SERVICES_WITH_OUTPUT_TYPE[O] <: ServicesWithOutput[O]
  def toExecutionRuntime: Either[ExecutionRuntimeException, ExecutionRuntime]
  def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo]
  def updateServicesWithInput[O](services: SERVICES_WITH_OUTPUT_TYPE[O]): UPDATED_THIS[O]
}

object NilServiceMatrix extends ServiceMatrix {
  type THIS                         = NilServiceMatrix.type
  type L2D                          = ServicesWithInput2DNil.type
  type UPDATED_THIS[O]              = NilServiceMatrix.type
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputNil[A]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputNil[A]
  override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = current
  override def toExecutionRuntime: Either[ExecutionRuntimeException, ExecutionRuntime] = {
    Left(new NoServicesFoundException())
  }
  override def updateServicesWithInput[O](services: SERVICES_WITH_OUTPUT_TYPE[O]): UPDATED_THIS[O] =
    NilServiceMatrix
}

final case class ServiceMatrixCompose[
  SI <: ServicesWithInput[TYPE],
  SO <: ServicesWithOutput[TYPE],
  TYPE: ClassTag,
  TAIL <: ServiceMatrix
](
  tail: TAIL,
  service: Service[TYPE, TYPE],
  conditions: Seq[Condition[TYPE]],
  servicesWithInput: SI,
  servicesWithOutput: SO)
    extends ServiceMatrix {
  type THIS                         = ServiceMatrixCompose[SI, SO, TYPE, TAIL]
  type L2D                          = ServicesWithInput2DCompose[TYPE, SI, tail.L2D]
  type UPDATED_THIS[O]              = ServiceMatrixCompose[ServicesWithInputCompose[TYPE, O, SI], SO, TYPE, tail.UPDATED_THIS[O]]
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputCompose[A, TYPE, tail.SERVICE_WITH_INPUT_TYPE[A]]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputCompose[TYPE, A, tail.SERVICES_WITH_OUTPUT_TYPE[A]]

  def appendType[NEXT_TYPE: ClassTag](
    nextServicesWithInput: this.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE],
    nextServicesWithOutput: this.SERVICES_WITH_OUTPUT_TYPE[NEXT_TYPE],
    services: Service[NEXT_TYPE, NEXT_TYPE],
    conditions: Seq[Condition[NEXT_TYPE]] = Seq.empty[Condition[NEXT_TYPE]]
  ): ServiceMatrixCompose[
    ServicesWithInputCompose[NEXT_TYPE, TYPE, tail.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE]],
    ServicesWithOutputCompose[TYPE, NEXT_TYPE, tail.SERVICES_WITH_OUTPUT_TYPE[NEXT_TYPE]],
    NEXT_TYPE,
    UPDATED_THIS[NEXT_TYPE]
  ] = {
    ServiceMatrixCompose(
      this.updateServicesWithInput(nextServicesWithOutput),
      services,
      conditions,
      nextServicesWithInput,
      nextServicesWithOutput
    )
  }
  //def get[I, O](implicit get: MatrixGet[I, O, THIS]): Service[I, O] = get(this)

  override def updateServicesWithInput[O](services: this.SERVICES_WITH_OUTPUT_TYPE[O]): UPDATED_THIS[O] = {
    val updatedTail = this.tail.updateServicesWithInput(services.tail)
    ServiceMatrixCompose(
      updatedTail,
      this.service,
      this.conditions,
      ServicesWithInputCompose(services.service, this.servicesWithInput),
      this.servicesWithOutput
    )
  }

  override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = {
    val services = service +: servicesWithInput.toSeq
    val typeInfo = TypeInfo.forType[TYPE](services, conditions)
    tail.toTypeInfoSeq(typeInfo +: current)
  }

  def toExecutionRuntime: Either[ExecutionRuntimeException, ExecutionRuntime] = {
    val matrixRuntime = toTypeInfoSeq(Seq.empty)
    Right(new ExecutionRuntime {
      override val runtime: Seq[TypeInfo] = matrixRuntime
    })
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
