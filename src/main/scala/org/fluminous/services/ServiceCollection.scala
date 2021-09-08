package org.fluminous.services

import org.fluminous.runtime.{ ResponseDeserializationException, Router }
import cats.MonadThrow
import io.circe.{ Decoder, Encoder, Json }
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.serverlessworkflow.api.workflow.BaseWorkflow
import org.fluminous.Settings
import org.fluminous.routing.{ RoutingBuilder, WorkFlowBuildException }
import org.fluminous.services.function.{ FunctionService1, FunctionService2, FunctionService3 }

case class ServiceCollection[F[_]: MonadThrow] private (private val services: Map[String, Service[F]]) {

  def addSyncService[IN: Decoder, OUT: Encoder](
    serviceName: String,
    func: IN => OUT,
    parameterName: String
  ): ServiceCollection[F] =
    addService[IN, OUT](serviceName, i => MonadThrow[F].pure(func(i)), parameterName)

  def addSyncService[IN1: Decoder, IN2: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2) => OUT,
    parameter1Name: String,
    parameter2Name: String
  ): ServiceCollection[F] =
    addService[IN1, IN2, OUT](
      serviceName,
      (i, j) => MonadThrow[F].pure(func(i, j)),
      parameter1Name,
      parameter2Name
    )

  def addSyncService[IN1: Decoder, IN2: Decoder, IN3: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2, IN3) => OUT,
    parameter1Name: String,
    parameter2Name: String,
    parameter3Name: String
  ): ServiceCollection[F] =
    addService[IN1, IN2, IN3, OUT](
      serviceName,
      (i, j, k) => MonadThrow[F].pure(func(i, j, k)),
      parameter1Name,
      parameter2Name,
      parameter3Name
    )

  def addService[IN: Decoder, OUT: Encoder](
    serviceName: String,
    func: IN => F[OUT],
    parameterName: String
  ): ServiceCollection[F] = {
    ServiceCollection(
      this.services.updated(
        serviceName,
        new FunctionService1[F, IN, OUT](serviceName, func, parameterName)
      )
    )
  }

  def addService[IN1: Decoder, IN2: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2) => F[OUT],
    parameter1Name: String,
    parameter2Name: String
  ): ServiceCollection[F] = {
    ServiceCollection(
      this.services.updated(
        serviceName,
        new FunctionService2[F, IN1, IN2, OUT](serviceName, func, parameter1Name, parameter2Name)
      )
    )
  }

  def addService[IN1: Decoder, IN2: Decoder, IN3: Decoder, OUT: Encoder](
    serviceName: String,
    func: (IN1, IN2, IN3) => F[OUT],
    parameter1Name: String,
    parameter2Name: String,
    parameter3Name: String
  ): ServiceCollection[F] = {
    ServiceCollection(
      this.services.updated(
        serviceName,
        new FunctionService3[F, IN1, IN2, IN3, OUT](serviceName, func, parameter1Name, parameter2Name, parameter3Name)
      )
    )
  }

  def toRouter[Rq: Encoder, Rs: Decoder](
    routingJson: String,
    settings: Settings
  ): Either[WorkFlowBuildException, Router[F, Rq, Rs]] = {
    withRouting(routingJson, settings)(r => new TypedRouter[F, Rq, Rs](new UntypedRouter[F](r)))
  }

  def toUntypedRouter(
    routingJson: String,
    settings: Settings
  ): Either[WorkFlowBuildException, Router[F, Json, Json]] = {
    withRouting(routingJson, settings)(new UntypedRouter[F](_))
  }

  private def withRouting[IN, OUT](
    routingJson: String,
    settings: Settings
  )(
    f: (Json => F[Json]) => Router[F, IN, OUT]
  ): Either[WorkFlowBuildException, Router[F, IN, OUT]] = {
    val workflow = BaseWorkflow.fromSource(routingJson)
    new RoutingBuilder(this.services, settings).fromWorkflow(workflow).map(f)
  }

  private class TypedRouter[F[_]: MonadThrow, Rq: Encoder, Rs: Decoder](private val untypedRouter: UntypedRouter[F])
      extends Router[F, Rq, Rs] {

    override def routeRequest(input: Rq): F[Rs] = {
      for {
        json   <- untyped.routeRequest(Encoder[Rq].apply(input))
        result <- decodeJson(json)
      } yield result
    }

    override def untyped: UntypedRouter[F] = untypedRouter

    private def decodeJson(json: Json): F[Rs] = {
      MonadThrow[F].fromEither(Decoder[Rs].decodeJson(json).left.map(ResponseDeserializationException))
    }
  }

  private class UntypedRouter[F[_]: MonadThrow](routing: Json => F[Json]) extends Router[F, Json, Json] {
    override def routeRequest(input: Json): F[Json] = routing(input)
    override def untyped: UntypedRouter[F]          = this
  }
}

object ServiceCollection {
  def apply[F[_]: MonadThrow](): ServiceCollection[F] = {
    ServiceCollection(Map.empty[String, Service[F]])
  }
}
