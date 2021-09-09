package org.fluminous.services.rest

import cats.{ Applicative, MonadThrow }
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import io.circe.Json
import io.swagger.v3.oas.models.Operation
import org.fluminous.services.{
  HttpInvocationError,
  JSONInputParameterType,
  ParsingResponseError,
  RequiredInputParameterIsMissing,
  Service,
  ServiceException,
  UnsuccessfulHttpStatusCode,
  ValidationError,
  ValidationFailure
}
import sttp.client3.{ basicRequest, Response, UriContext }
import sttp.model.{ Header, Method, Uri }
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.list._

import scala.collection.JavaConverters._
import io.circe.parser._
import io.swagger.v3.oas.models.parameters.Parameter

sealed abstract class RestService[F[_]: MonadThrow: HttpBackend](
  val server: String,
  val path: String,
  val operationId: String,
  val substitutablePath: SubstitutablePath[F],
  val headerParameters: List[(String, Boolean)])
    extends Service[F](operationId) {
  val monadThrow = MonadThrow[F]
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

  private def distributeParameters(request: Map[String, Json]): F[(Uri, Option[Json], List[Header])] = {
    for {
      substitutionResult         <- substitutablePath.getUri(request)
      (remainingParameters, uri) = substitutionResult
      validatedParameters        = headerParameters.traverse(validateParameter(remainingParameters))
      headers                    <- fromValidated(validatedParameters.leftMap(ValidationError(operationId, _)).map(_.flatten))
    } yield (uri, headers)
  }

  private def validateParameter(
    inputParameters: Map[String, Json]
  )(
    validatedParameter: (String, Boolean)
  ): Validated[List[ValidationFailure], Option[(Map[String, Json], Header)]] = {
    val name           = validatedParameter._1
    val isOptional     = validatedParameter._2
    val inputParameter = inputParameters.get(name)
    if (inputParameter.isEmpty && !isOptional)
      Invalid(List(RequiredInputParameterIsMissing(name)))
    else if (inputParameter.exists(nonPrimitive))
      Invalid(
        List(
          JSONInputParameterType(
            name,
            inputParameter.exists(_.isArray),
            inputParameter.exists(_.isObject),
            inputParameter.exists(_.isNull)
          )
        )
      )
    else Valid(inputParameter.map(p => Header(name, p.toString())))
  }

  private def nonPrimitive(j: Json): Boolean = j.isArray || j.isObject || j.isNull

  private def processResponse(endpoint: Uri, method: Method, response: F[Response[Either[String, String]]]): F[Json] = {
    handleErrorWith(response) {
      case e: Throwable => raiseError(HttpInvocationError(operationId, e))
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
  server: String,
  path: String,
  operationId: String,
  queryParameters: Seq[Parameter],
  pathParameters: Seq[Parameter],
  headerParameters: Seq[Parameter])
    extends RestService(server, path, operationId, queryParameters, pathParameters, headerParameters) {
  protected val httpMethod: Method = Method.GET
}

class PostService[F[_]: MonadThrow: HttpBackend](
  server: String,
  path: String,
  operationId: String,
  substitutablePath: SubstitutablePath,
  headerParameters: Seq[Parameter])
    extends RestService(server, path, operationId, substitutablePath, headerParameters) {
  protected val httpMethod: Method = Method.POST
}

class PutService[F[_]: MonadThrow: HttpBackend](
  server: String,
  path: String,
  operationId: String,
  substitutablePath: SubstitutablePath,
  headerParameters: Seq[Parameter])
    extends RestService(server, path, operationId, substitutablePath, headerParameters) {
  protected val httpMethod: Method = Method.PUT
}

class DeleteService[F[_]: MonadThrow: HttpBackend](
  server: String,
  path: String,
  operationId: String,
  substitutablePath: SubstitutablePath,
  headerParameters: Seq[Parameter])
    extends RestService(server, path, operationId, substitutablePath, headerParameters) {
  protected val httpMethod: Method = Method.DELETE
}
