package org.fluminous.services

import cats.Monad
import cats.data.EitherT
import io.circe.Json
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.PathItem.HttpMethod
import org.fluminous.runtime.exception.ServiceException

class RestService[F[_]: Monad](server: String, path: String, method: HttpMethod, operation: Operation)
    extends Service[F](operation.getOperationId) {
  override def invoke(request: Map[String, Json]): EitherT[F, ServiceException, Json] = ???
}
