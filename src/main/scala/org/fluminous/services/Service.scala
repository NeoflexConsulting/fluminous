package org.fluminous.services

import cats.{Id, Monad}
import cats.data.EitherT
import io.circe.{Decoder, Encoder, Json}
import org.fluminous.runtime.exception.{DeserializationException, NotFoundInputParameter, ServiceException}
import org.fluminous.services.function.{FunctionService1, FunctionService2, FunctionService3}

import scala.concurrent.Future

abstract class Service[F[_]: Monad](val name: String) {
  def invoke(request: Map[String, Json]): EitherT[F, ServiceException, Json]
  protected def getValue[IN: Decoder](
    request: Map[String, Json],
    parameterName: String
  ): EitherT[F, ServiceException, IN] = {
    val result = for {
      parameterJson  <- request.get(parameterName).toRight(NotFoundInputParameter(name, parameterName))
      parameterValue <- Decoder[IN].decodeJson(parameterJson).left.map(DeserializationException(name, _))
    } yield parameterValue
    EitherT.fromEither(result)
  }

}

object Service {
  def apply[IN: Decoder, OUT: Encoder](serviceName: String, func: IN => OUT, parameterName: String): Service[Id] = {
    new FunctionService1[Id, IN, OUT](serviceName, func, parameterName)
  }
  def apply[IN1: Decoder, IN2: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2) => OUT,
    parameter1Name: String,
    parameter2Name: String
  ): Service[Id] = {
    new FunctionService2[Id, IN1, IN2, OUT](serviceName, func, parameter1Name, parameter2Name)
  }
  def apply[IN1: Decoder, IN2: Decoder, IN3: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2, IN3) => OUT,
    parameter1Name: String,
    parameter2Name: String,
    parameter3Name: String
  ): Service[Id] = {
    new FunctionService3[Id, IN1, IN2, IN3, OUT](serviceName, func, parameter1Name, parameter2Name, parameter3Name)
  }
}

object AsyncService {
  def apply[IN: Decoder, OUT: Encoder](
    serviceName: String,
    func: IN => Future[OUT],
    parameterName: String
  ): Service[Future] = {
    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global
    new FunctionService1[Future, IN, OUT](serviceName, func, parameterName)
  }

  def apply[IN1: Decoder, IN2: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2) => Future[OUT],
    parameter1Name: String,
    parameter2Name: String
  ): Service[Future] = {
    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global
    new FunctionService2[Future, IN1, IN2, OUT](serviceName, func, parameter1Name, parameter2Name)
  }
  def apply[IN1: Decoder, IN2: Decoder, IN3: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2, IN3) => Future[OUT],
    parameter1Name: String,
    parameter2Name: String,
    parameter3Name: String
  ): Service[Future] = {
    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global
    new FunctionService3[Future, IN1, IN2, IN3, OUT](serviceName, func, parameter1Name, parameter2Name, parameter3Name)
  }
}
