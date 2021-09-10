package org.fluminous.runtime

import cats.MonadThrow
import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.services.{ Service, ServiceException }
import cats.syntax.flatMap._
import cats.syntax.functor._

case class ActionExecutor[F[_]: MonadThrow](
  arguments: Map[String, Filter],
  fromStateDataFilter: Filter,
  resultsFilter: Filter,
  toStateDataFilter: Filter) {
  private val monadThrow = MonadThrow[F]
  import monadThrow._

  def execute(service: Service[F])(input: Json): F[Option[Json]] = {
    val argumentsAsJson = arguments.flatMap { case (k, v) => v.transform(input).map(j => (k, j)) }
    for {
      serviceOutput <- recoverWith(service.invoke(argumentsAsJson)) {
                 case e: ServiceException => raiseError(ServiceExecutionException(e))
               }
    } yield resultsFilter.transform(serviceOutput).flatMap(j => toStateDataFilter.transform(j))
  }
}
