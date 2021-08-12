package org.fluminous.matrix

import org.fluminous.runtime.exception.{ ExecutionRuntimeException, NoServicesFoundException }
import org.fluminous.runtime.{ Condition, ExecutionRuntime, TypeInfo }

import scala.collection.mutable
import scala.reflect.ClassTag

sealed trait ServiceMatrix {
  type THIS <: ServiceMatrix
  type UPDATED_THIS[O] <: ServiceMatrix
  type SERVICE_WITH_INPUT_TYPE[I] <: ServicesWithInput[I]
  type SERVICES_WITH_OUTPUT_TYPE[O] <: ServicesWithOutput[O]
  def toExecutionRuntime: Either[ExecutionRuntimeException, ExecutionRuntime]
  def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo]
  def updateServicesWithInput[O](services: SERVICES_WITH_OUTPUT_TYPE[O]): UPDATED_THIS[O]
}

object NilServiceMatrix extends ServiceMatrix {
  type THIS                         = NilServiceMatrix.type
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

final case class ServiceMatrixCompose[SI <: ServicesWithInput[TYPE], TYPE: ClassTag, TAIL <: ServiceMatrix](
  tail: TAIL,
  services: mutable.Buffer[Service[TYPE, TYPE]],
  conditions: Seq[Condition[TYPE]],
  servicesWithInput: SI)
    extends ServiceMatrix {
  type THIS                         = ServiceMatrixCompose[SI, TYPE, TAIL]
  type UPDATED_THIS[O]              = ServiceMatrixCompose[ServicesWithInputCompose[TYPE, O, SI], TYPE, tail.UPDATED_THIS[O]]
  type SERVICE_WITH_INPUT_TYPE[A]   = ServicesWithInputCompose[A, TYPE, tail.SERVICE_WITH_INPUT_TYPE[A]]
  type SERVICES_WITH_OUTPUT_TYPE[A] = ServicesWithOutputCompose[TYPE, A, tail.SERVICES_WITH_OUTPUT_TYPE[A]]

  def appendType[NEXT_TYPE: ClassTag](
    nextServicesWithInput: this.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE],
    nextServicesWithOutput: this.SERVICES_WITH_OUTPUT_TYPE[NEXT_TYPE],
    services: Seq[Service[NEXT_TYPE, NEXT_TYPE]],
    conditions: Seq[Condition[NEXT_TYPE]] = Seq.empty[Condition[NEXT_TYPE]]
  ): ServiceMatrixCompose[
    ServicesWithInputCompose[NEXT_TYPE, TYPE, tail.SERVICE_WITH_INPUT_TYPE[NEXT_TYPE]],
    NEXT_TYPE,
    UPDATED_THIS[NEXT_TYPE]
  ] = {
    ServiceMatrixCompose(
      this.updateServicesWithInput(nextServicesWithOutput),
      services.toBuffer,
      conditions,
      nextServicesWithInput
    )
  }
  //def get[I, O](implicit get: MatrixGet[I, O, THIS]): Service[I, O] = get(this)

  override def updateServicesWithInput[O](services: this.SERVICES_WITH_OUTPUT_TYPE[O]): UPDATED_THIS[O] = {
    val updatedTail = this.tail.updateServicesWithInput(services.tail)
    ServiceMatrixCompose(
      updatedTail,
      this.services,
      this.conditions,
      ServicesWithInputCompose(services.services, this.servicesWithInput)
    )
  }

  override def toTypeInfoSeq(current: Seq[TypeInfo]): Seq[TypeInfo] = {
    val services = servicesWithInput.toSeq
    val typeInfo = TypeInfo.forType[TYPE](services, conditions)
    tail.toTypeInfoSeq(typeInfo +: current)
  }

  def toExecutionRuntime: Either[ExecutionRuntimeException, ExecutionRuntime] = {
    val matrixRuntime = toTypeInfoSeq(Seq.empty)
    Right(new ExecutionRuntime {
      override val runtime: Map[String, TypeInfo] = matrixRuntime.groupBy(_.typeName).flatMap {
        case (name, typeInfo) => typeInfo.headOption.map(t => (name, t))
      }
    })
  }
}

object ServiceMatrix {

  def apply[W: ClassTag](service: Service[W, W], condition: Condition[W]) = {
    ServiceMatrixCompose(NilServiceMatrix, mutable.Buffer(service), Seq(condition), ServicesWithInputNil[W])
  }

  def apply[W: ClassTag](services: Seq[Service[W, W]], condition: Condition[W]) = {
    ServiceMatrixCompose(NilServiceMatrix, services.toBuffer, Seq(condition), ServicesWithInputNil[W])
  }

  def apply[W: ClassTag](service: Service[W, W]) = {
    ServiceMatrixCompose(
      NilServiceMatrix,
      mutable.Buffer(service),
      Seq.empty[Condition[W]],
      ServicesWithInputNil[W]
    )
  }

  def apply[W: ClassTag](services: Seq[Service[W, W]]) = {
    ServiceMatrixCompose(
      NilServiceMatrix,
      services.toBuffer,
      Seq.empty[Condition[W]],
      ServicesWithInputNil[W]
    )
  }
}
