package org.fluminous.services

import org.fluminous.runtime.Variable
import org.fluminous.runtime.exception.{ IncompatibleTypeException, ServiceException }

import scala.reflect.ClassTag

sealed abstract class Service[IN: ClassTag, OUT: ClassTag](val name: String) {
  protected val inputTypeName  = implicitly[ClassTag[IN]].runtimeClass.getTypeName
  protected val outputTypeName = implicitly[ClassTag[OUT]].runtimeClass.getTypeName

  def invoke(request: IN): Either[ServiceException, OUT]
  def toRuntimeService: RuntimeService = new RuntimeService(runtimeInvoke)
  private def runtimeInvoke(request: Variable, outputVariableName: String): Either[ServiceException, Variable] = {

    if (request.typeName != inputTypeName)
      Left(new IncompatibleTypeException(this.name, inputTypeName, request.typeName))
    else
      invoke(request.value.asInstanceOf[IN]).map(Variable(outputVariableName, outputTypeName, _))
  }
}

final class FunctionService[IN: ClassTag, OUT: ClassTag](val function: IN => OUT, name: String)
    extends Service[IN, OUT](name) {
  override def invoke(request: IN): Either[ServiceException, OUT] = Right(function(request))
}

final class RuntimeService(val invokeOuter: (Variable, String) => Either[ServiceException, Variable]) {
  def invoke(request: Variable, outputVariableName: String): Either[ServiceException, Variable] = {
    invokeOuter(request, outputVariableName)
  }
}

object Service {
  def apply[IN: ClassTag, OUT: ClassTag](serviceName: String, func: IN => OUT): Service[IN, OUT] = {
    new FunctionService[IN, OUT](func, serviceName)
  }
}
