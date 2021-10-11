package org.fluminous.runtime

import cats.MonadThrow
import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.services.{ Service, ServiceException }
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

case class ActionExecutor[F[_]: MonadThrow](
  stateName: String,
  operationName: String,
  arguments: List[(String, Filter)],
  fromStateDataFilter: Filter,
  resultsFilter: Filter,
  toStateDataFilter: Filter)
    extends ExecutorFunctions {
  private val monadThrow = MonadThrow[F]
  import monadThrow._

  def execute(service: Service[F])(input: Json): F[Json] = {
    for {
      argumentsAsJson <- fromEither(arguments.traverse { case (k, v) => v.transform(List(input)).map(j => (k, j)) })
      argumentsAsSingleJson <- fromEither(
                                argumentsAsJson
                                  .map(getUniqueValue(NonUniqueArgumentValue(stateName, operationName)))
                                  .sequence
                              )
      serviceOutput <- recoverWith(service.invoke(argumentsAsSingleJson.toMap)) {
                        case e: ServiceException => raiseError(ServiceExecutionException(e))
                      }
      r            <- fromEither(resultsFilter.transform(List(serviceOutput)).flatMap(j => toStateDataFilter.transform(j)))
      uniqueResult <- fromEither(getUnique(NonUniqueArgumentValue(stateName, operationName), r))
    } yield uniqueResult
  }

}
