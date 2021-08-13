package org.fluminous.services

import cats.{ Id, Monad }
import cats.data.EitherT
import cats.instances.future._
import org.fluminous.runtime.Variable
import org.fluminous.runtime.exception.{ IncompatibleTypeException, ServiceException }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag

sealed abstract class Service[F[_]: Monad, IN: ClassTag, OUT: ClassTag](val name: String) {
  protected val inputTypeName: String  = implicitly[ClassTag[IN]].runtimeClass.getTypeName
  protected val outputTypeName: String = implicitly[ClassTag[OUT]].runtimeClass.getTypeName

  def invoke(request: IN): EitherT[F, ServiceException, OUT]
  def toRuntimeService: RuntimeService[F] = new RuntimeService(runtimeInvoke)
  private def runtimeInvoke(request: Variable, outputVariableName: String): EitherT[F, ServiceException, Variable] = {

    if (request.typeName != inputTypeName)
      EitherT.leftT(new IncompatibleTypeException(this.name, inputTypeName, request.typeName))
    else
      invoke(request.value.asInstanceOf[IN]).map(Variable(outputVariableName, outputTypeName, _))
  }
}

final class FunctionService[F[_]: Monad, IN: ClassTag, OUT: ClassTag](val function: IN => F[OUT], name: String)
    extends Service[F, IN, OUT](name) {
  override def invoke(request: IN): EitherT[F, ServiceException, OUT] = { EitherT.liftF(function(request)) }
}

final class RuntimeService[F[_]: Monad](val invokeOuter: (Variable, String) => EitherT[F, ServiceException, Variable]) {
  def invoke(request: Variable, outputVariableName: String): EitherT[F, ServiceException, Variable] = {
    invokeOuter(request, outputVariableName)
  }
}

object Service {
  def apply[IN: ClassTag, OUT: ClassTag](serviceName: String, func: IN => OUT): Service[Id, IN, OUT] = {
    new FunctionService[Id, IN, OUT](func, serviceName)
  }
}

object AsyncService {
  def apply[IN: ClassTag, OUT: ClassTag](serviceName: String, func: IN => Future[OUT]): Service[Future, IN, OUT] = {
    new FunctionService[Future, IN, OUT](func, serviceName)
  }
}
