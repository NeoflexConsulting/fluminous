package org.fluminous.services.rest

import cats.MonadThrow
import io.circe.Json
import org.fluminous.services.{
  HttpInvocationError,
  MultipleBodyParameters,
  ParsingResponseError,
  Service,
  UnsuccessfulHttpStatusCode
}
import sttp.client3.{ basicRequest, Response }
import sttp.model.{ Header, Method, Uri }
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.circe.parser._
import org.fluminous.routing.ParameterFunctions

sealed abstract class RestService[F[_]: MonadThrow: HttpBackend](
  val operationId: String,
  val endpointTemplate: EndpointTemplate,
  val headerParameters: List[(String, Boolean)])
    extends Service[F](operationId)
    with ParameterFunctions {
  private val monadThrow = MonadThrow[F]
  import monadThrow._

  final override def invoke(request: Map[String, Json]): F[Json] = {
    distributeParameters(request).flatMap {
      case (uri, body, headers) =>
        val requestWithBody    = body.map(b => basicRequest.body(b.toString)).getOrElse(basicRequest)
        val requestWithHeaders = headers.foldLeft(requestWithBody)(_.header(_))
        processResponse(uri, httpMethod, requestWithHeaders.method(httpMethod, uri).send(HttpBackend[F].backend))
    }
  }
  protected val httpMethod: Method

  private def distributeParameters(input: Map[String, Json]): F[(Uri, Option[Json], List[Header])] = {
    for {
      endpoint         <- fromEither(endpointTemplate.endpoint(input))
      headerParameters <- fromEither(validateRequiredParameters(operationId, headerParameters, input))
      headers          = headerParameters.map(p => Header(p._1, p._2))
      remainingInput   = input.filterKeys(endpointTemplate.usedParameters ++ headerParameters.map(_._1).toSet)
      body             <- getBody(remainingInput.toList)
    } yield (endpoint, body, headers)
  }

  private def getBody(input: List[(String, Json)]): F[Option[Json]] = {
    input match {
      case Nil         => pure(None)
      case json :: Nil => pure(Some(json._2))
      case l @ _       => raiseError(MultipleBodyParameters(operationId, l.map(_._1)))
    }
  }

  private def processResponse(endpoint: Uri, method: Method, response: F[Response[Either[String, String]]]): F[Json] = {
    handleErrorWith(response) { e =>
      raiseError(HttpInvocationError(operationId, e))
    }.flatMap(extractBody(endpoint, method)).flatMap(parseToJson(endpoint, method))
  }

  private def extractBody(url: Uri, method: Method)(response: Response[Either[String, String]]): F[String] = {
    response.body
      .fold(
        code => raiseError(UnsuccessfulHttpStatusCode(operationId, url.toString, method.toString(), code)),
        pure
      )
  }

  private def parseToJson(url: Uri, method: Method)(text: String): F[Json] = {
    fromEither(parse(text).left.map(ParsingResponseError(operationId, url.toString, method.toString(), _)))
  }
}

class GetService[F[_]: MonadThrow: HttpBackend](
  operationId: String,
  endpointTemplate: EndpointTemplate,
  headerParameters: List[(String, Boolean)])
    extends RestService(operationId, endpointTemplate, headerParameters) {
  protected val httpMethod: Method = Method.GET
}

class PostService[F[_]: MonadThrow: HttpBackend](
  operationId: String,
  endpointTemplate: EndpointTemplate,
  headerParameters: List[(String, Boolean)])
    extends RestService(operationId, endpointTemplate, headerParameters) {
  protected val httpMethod: Method = Method.POST
}

class PutService[F[_]: MonadThrow: HttpBackend](
  operationId: String,
  endpointTemplate: EndpointTemplate,
  headerParameters: List[(String, Boolean)])
    extends RestService(operationId, endpointTemplate, headerParameters) {
  protected val httpMethod: Method = Method.PUT
}

class DeleteService[F[_]: MonadThrow: HttpBackend](
  operationId: String,
  endpointTemplate: EndpointTemplate,
  headerParameters: List[(String, Boolean)])
    extends RestService(operationId, endpointTemplate, headerParameters) {
  protected val httpMethod: Method = Method.DELETE
}
