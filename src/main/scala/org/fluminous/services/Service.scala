package org.fluminous.services

import org.fluminous.runtime.Variable
import org.fluminous.runtime.exception.{ IncompatibleTypeException, ServiceException }

import scala.reflect.ClassTag

sealed abstract class Service[IN: ClassTag, OUT: ClassTag](
  val name: String,
  val inputTypeName: String,
  val outputTypeName: String) {
  def this(serviceName: String) = {
    this(
      serviceName,
      implicitly[ClassTag[IN]].runtimeClass.getClass.getTypeName,
      implicitly[ClassTag[OUT]].runtimeClass.getClass.getTypeName
    )
  }
  def invoke(request: IN): Either[ServiceException, OUT]
  def toRuntimeService: RuntimeService = {
    val outer = this
    def runtimeInvoke(request: Variable, outputVariableName: String): Either[ServiceException, Variable] = {
      if (request.typeName != this.inputTypeName)
        Left(new IncompatibleTypeException(this.name, this.inputTypeName, request.typeName))
      else
        outer.invoke(request.value.asInstanceOf[IN]).map(Variable(outputVariableName, outputTypeName, _))
    }
    new RuntimeService(runtimeInvoke)
  }
}

final class RuntimeService(val invokeOuter: (Variable, String) => Either[ServiceException, Variable]) {
  def invoke(request: Variable, outputVariableName: String): Either[ServiceException, Variable] = {
    invokeOuter(request, outputVariableName)
  }
}

object Service {
  def apply[IN: ClassTag, OUT: ClassTag](serviceName: String, func: IN => OUT): Service[IN, OUT] = {
    new Service[IN, OUT](serviceName) {
      override def invoke(request: IN): Either[ServiceException, OUT] = Right(func(request))
    }
  }
}
