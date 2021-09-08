package org.fluminous.services.rest

import cats.Monad
import cats.data.EitherT
import io.circe.Json
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.PathItem.HttpMethod
import org.fluminous.runtime.exception.ServiceException
import org.fluminous.services.Service
import sttp.client3.UriContext
import sttp.model.Uri

import scala.collection.JavaConverters._

class RestService[F[_]: Monad](server: String, path: String, method: HttpMethod, operation: Operation)
    extends Service[F](operation.getOperationId) {
  println("!!!!!!!!!!!!!")
  println(server)
  println(path)
  operation.getParameters.asScala.map(_.getSchema.get$ref()).foreach(println)
  override def invoke(request: Map[String, Json]): EitherT[F, ServiceException, Json] = {
    operation.getRequestBody.getRequired
    operation.getParameters.asScala.map(_.getRequired)
    operation.getParameters.asScala.map(_.getName)
    operation.getParameters.asScala.map(_.getIn)
    operation.getParameters.asScala.map(_.getSchema)
    val user          = "Mary Smith"
    val filter        = "programming languages"
    val endpoint: Uri = uri"http://$server/$path?$request"
    ???
  }
}
