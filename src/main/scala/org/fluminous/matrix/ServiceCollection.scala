package org.fluminous.matrix

import org.fluminous.runtime.exception.ExecutionRuntimeException
import org.fluminous.runtime.{ Condition, ExecutionRuntime, TypeInfo }
import shapeless._

import scala.reflect.ClassTag

case class ServiceCollection[L <: HList] private (private val tags: L, private val runtime: Map[String, TypeInfo]) {
  type HListType = L
  def addType[A: ClassTag](
    implicit ev: IsDistinct[ClassTag[A] :: HListType]
  ): ServiceCollection[ClassTag[A] :: HListType] = {
    val tag = implicitly[ClassTag[A]]
    val typeName = tag.runtimeClass.getTypeName
    new ServiceCollection(tag :: tags, this.runtime.updated(typeName, TypeInfo.forType[A]))
  }

  def addService[A: ClassTag, B: ClassTag](
    service: Service[A, B]
  )(implicit evA: Contains[HListType, ClassTag[A]],
    evB: Contains[HListType, ClassTag[B]]
  ): ServiceCollection[L] = {
    val inputClass          = implicitly[ClassTag[A]].runtimeClass.getTypeName
    val typeInfo            = this.runtime.getOrElse(inputClass, TypeInfo.forType[A])
    val updatedServicesList = (service.name, service.toRuntimeService) +: typeInfo.services.toSeq
    val updatedTypeInfo     = typeInfo.copy(services = updatedServicesList.toMap)
    ServiceCollection(tags, this.runtime.updated(inputClass, updatedTypeInfo))
  }

  def addCondition[A: ClassTag](
    condition: Condition[A]
  )(implicit evA: Contains[HListType, ClassTag[A]]
  ): ServiceCollection[L] = {
    val inputClass            = implicitly[ClassTag[A]].runtimeClass.getTypeName
    val typeInfo              = this.runtime.getOrElse(inputClass, TypeInfo.forType[A])
    val updatedConditionsList = (condition.conditionName, condition.toRuntimeCondition) +: typeInfo.conditions.toSeq
    val updatedTypeInfo       = typeInfo.copy(conditions = updatedConditionsList.toMap)
    ServiceCollection(tags, this.runtime.updated(inputClass, updatedTypeInfo))
  }

  def toExecutionRuntime: Either[ExecutionRuntimeException, ExecutionRuntime] = {
    val outer = this
    Right(
      new ExecutionRuntime {
        override protected val runtime: Map[String, TypeInfo] = outer.runtime
      }
    )
  }
}

object ServiceCollection {
  def apply(): ServiceCollection[HNil] = {
    ServiceCollection(HNil, Map.empty[String, TypeInfo])
  }
}
