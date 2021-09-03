package org.fluminous.services

import org.fluminous.runtime.exception.{ ExecutionRuntimeException, ResponseDeserializationException }
import org.fluminous.runtime.{ ExecutionRuntime, Router }
import cats.Monad
import cats.data.EitherT
import io.circe.{ Decoder, Encoder, Json }
import org.fluminous.routing.{ ExecutionStep, Finish, Routing }

case class ServiceCollection[F[_]: Monad] private (private val services: Map[String, Service[F]]) {

  def addService(service: Service[F]): ServiceCollection[F] = {
    ServiceCollection(this.services.updated(service.name, service))
  }

  def toRouter[Rq: Encoder, Rs: Decoder](routing: Routing[F]): Router[F, Rq, Rs] = {
    new TypedRouter[F, Rq, Rs](new UntypedRouter[F](services, routing))
  }

  def toUntypedRouter(routing: Routing[F]): Router[F, Json, Json] = { new UntypedRouter[F](services, routing) }

  private class TypedRouter[F[_]: Monad, Rq: Encoder, Rs: Decoder](private val untypedRouter: UntypedRouter[F])
      extends Router[F, Rq, Rs] {

    override def routeRequest(input: Rq): F[Either[ExecutionRuntimeException, Rs]] = {
      (for {
        json   <- EitherT(untyped.routeRequest(Encoder[Rq].apply(input)))
        result <- decodeJson(json)
      } yield result).value
    }

    override def untyped: UntypedRouter[F] = untypedRouter

    private def decodeJson(json: Json): EitherT[F, ExecutionRuntimeException, Rs] = {
      EitherT.fromEither[F](Decoder[Rs].decodeJson(json).left.map(ResponseDeserializationException))
    }
  }

  private class UntypedRouter[F[_]: Monad](private val services: Map[String, Service[F]], routing: Routing[F])
      extends Router[F, Json, Json] {

    override def routeRequest(input: Json): F[Either[ExecutionRuntimeException, Json]] = {
      val rc = executeNextStep(
        EitherT.pure(ExecutionRuntime[F](services ++ routing.services, input, routing.firstStep))
      )
      rc.value
    }

    override def untyped: UntypedRouter[F] = this

    private def executeNextStep(
      runtime: EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F]]
    ): EitherT[F, ExecutionRuntimeException, Json] = {
      runtime.flatMap { r =>
        r.currentStep match {
          case Finish              => EitherT.pure(r.contextJson)
          case step: ExecutionStep => executeNextStep(step.executeInRuntime(r))
        }
      }
    }
  }
}

object ServiceCollection {
  def apply[F[_]: Monad](): ServiceCollection[F] = {
    ServiceCollection(Map.empty[String, Service[F]])
  }
}
