package org.fluminous.services.rest

import cats.MonadThrow
import io.circe.Json
import io.swagger.v3.oas.models.parameters.Parameter
import sttp.model.Uri

class SubstitutablePath[F[_]: MonadThrow](
  path: String,
  pathParameters: Seq[Parameter],
  queryParameters: Seq[Parameter]) {
  def getUri(parameters: Map[String, Json]): F[(Map[String, Json], Uri)] = {
    ???
  }
  val endpoint: Uri            = uri"http://$server/$path?$request"
  private val parameterPattern = """{(.+)}""".r
}

object Test {
  def main(args: Array[String]): Unit = {
    val parameterPattern = """{(.+)}""".r
  }
}
