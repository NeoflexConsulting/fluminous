package org.fluminous.services

import org.fluminous.runtime.exception.{ ExecutionRuntimeException, ServiceException, VariableNotFoundException }
import org.fluminous.runtime.{ Condition, ExecutionRuntime, Router, TypeInfo, UnpreparedExecutionRuntime }
import shapeless._

import scala.reflect.ClassTag

case class ServiceCollection[L <: HList] private (private val tags: L, private val runtime: Map[String, TypeInfo]) {
  type HListType = L
  def addType[A: ClassTag](
    implicit ev: IsDistinct[ClassTag[A] :: HListType]
  ): ServiceCollection[ClassTag[A] :: HListType] = {
    val tag      = implicitly[ClassTag[A]]
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

  def toRouter[Rq: ClassTag, Rs: ClassTag](
    implicit evRq: Contains[HListType, ClassTag[Rq]],
    evRs: Contains[HListType, ClassTag[Rs]]
  ): Router[Rq, Rs] = {
    val inputTypeName  = implicitly[ClassTag[Rq]].runtimeClass.getTypeName
    val outputTypeName = implicitly[ClassTag[Rs]].runtimeClass.getTypeName
    val setter: Rq => Map[String, TypeInfo] = { request =>
      val updatedTypeInfo = this.runtime.getOrElse(inputTypeName, TypeInfo.forType[Rq]).copy(inputValue = Some(request))
      this.runtime.updated(inputTypeName, updatedTypeInfo)
    }
    val getter: (String, Map[String, TypeInfo]) => Either[ExecutionRuntimeException, Rs] = { (variableName, rt) =>
      {
        rt.getOrElse(outputTypeName, TypeInfo.forType[Rs])
          .variables
          .get(variableName)
          .map(_.value.asInstanceOf[Rs])
          .toRight(new VariableNotFoundException(variableName, outputTypeName))

      }
    }
    new Router[Rq, Rs](new UnpreparedExecutionRuntime[Rq, Rs](setter, getter))
  }
}

object ServiceCollection {
  def apply(): ServiceCollection[HNil] = {
    ServiceCollection(HNil, Map.empty[String, TypeInfo])
  }
}
