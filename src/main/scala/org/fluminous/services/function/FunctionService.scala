package org.fluminous.services.function

import cats.Monad
import cats.data.EitherT
import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Json}
import org.fluminous.runtime.exception.ServiceException
import org.fluminous.services.Service

final class FunctionService1[F[_]: Monad, IN: Decoder, OUT: Encoder](
    name: String,
    function: IN => F[OUT],
    parameterName: String)
    extends Service[F](name) {
  override def invoke(request: Map[String, Json]): EitherT[F, ServiceException, Json] = {
    for {
      parameterValue <- getValue[IN](request, parameterName)
      result         <- EitherT.right(function(parameterValue).map(Encoder[OUT].apply))
    } yield result
  }
}

final class FunctionService2[F[_]: Monad, IN1: Decoder, IN2: Decoder, OUT: Encoder](
    name: String,
    function: (IN1, IN2) => F[OUT],
    parameter1Name: String,
    parameter2Name: String)
    extends Service[F](name) {
  override def invoke(request: Map[String, Json]): EitherT[F, ServiceException, Json] = {
    import EitherT._
    for {
      value1 <- getValue[IN1](request, parameter1Name)
      value2 <- getValue[IN2](request, parameter2Name)
      result <- right(function(value1, value2).map(Encoder[OUT].apply))
    } yield result
  }
}

final class FunctionService3[F[_]: Monad, IN1: Decoder, IN2: Decoder, IN3: Decoder, OUT: Encoder](
    name: String,
    function: (IN1, IN2, IN3) => F[OUT],
    parameter1Name: String,
    parameter2Name: String,
    parameter3Name: String)
    extends Service[F](name) {
  override def invoke(request: Map[String, Json]): EitherT[F, ServiceException, Json] = {
    import EitherT._
    for {
      value1 <- getValue[IN1](request, parameter1Name)
      value2 <- getValue[IN2](request, parameter2Name)
      value3 <- getValue[IN3](request, parameter3Name)
      result <- right(function(value1, value2, value3).map(Encoder[OUT].apply))
    } yield result
  }
}
