package org.fluminous.services

import org.fluminous.runtime.exception.{ ExecutionRuntimeException, VariableNotFoundException }
import org.fluminous.runtime.{ ExecutionRuntimeTemplate, Router, TypeInfo }
import shapeless._
import cats.Monad
import scala.reflect.ClassTag

case class ServiceCollection[F[_]: Monad] private (private val runtime: Map[String, Service[F]]) {
  def addService(service: Service[F]): ServiceCollection[F] = {
    ServiceCollection(this.runtime.updated(service.name, service))
  }

  def toRouter[Rq: ClassTag, Rs: ClassTag]: Router[F, Rq, Rs] = {
    val inputTypeName  = implicitly[ClassTag[Rq]].runtimeClass.getTypeName
    val outputTypeName = implicitly[ClassTag[Rs]].runtimeClass.getTypeName
    val setter: Rq => Map[String, TypeInfo[F]] = { request =>
      val updatedTypeInfo =
        this.runtime.getOrElse(inputTypeName, TypeInfo.forType[F, Rq]).copy(inputValue = Some(request))
      this.runtime.updated(inputTypeName, updatedTypeInfo)
    }
    val getter: (String, Map[String, TypeInfo[F]]) => Either[ExecutionRuntimeException, Rs] = { (variableName, rt) =>
      {
        rt.getOrElse(outputTypeName, TypeInfo.forType[F, Rs])
          .variables
          .get(variableName)
          .map(_.value.asInstanceOf[Rs])
          .toRight(new VariableNotFoundException(variableName, outputTypeName))

      }
    }
    new Router[F, Rq, Rs](new ExecutionRuntimeTemplate[F, Rq, Rs](setter, getter))
  }
}

object ServiceCollection {
  def apply[F[_]: Monad](): ServiceCollection[F] = {
    ServiceCollection(Map.empty[String, Service[F]])
  }
}
