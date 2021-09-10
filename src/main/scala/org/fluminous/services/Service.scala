package org.fluminous.services

import cats.MonadThrow
import io.circe.{ Decoder, DecodingFailure, Json }
import cats.syntax.flatMap._
import cats.syntax.functor._

abstract class Service[F[_]: MonadThrow](val name: String) {
  private val monadThrow = MonadThrow[F]
  import monadThrow._
  def invoke(request: Map[String, Json]): F[Json]
  protected def getValue[IN: Decoder](request: Map[String, Json], parameterName: String): F[IN] = {
    for {
      parameterJson <- fromOption(request.get(parameterName), NotFoundInputParameter(name, parameterName))
      parameterValue <- recoverWith(fromEither(Decoder[IN].decodeJson(parameterJson))) {
                         case e: DecodingFailure => raiseError(DeserializationException(name, e))
                       }
    } yield parameterValue
  }
}
