package org.fluminous.services.rest

import io.circe.Json
import io.swagger.v3.oas.models.parameters.Parameter
import org.fluminous.routing.{
  InvalidRestPath,
  ParameterFunctions,
  PathContainsNotDeclaredParameters,
  PathParametersAreMissingFromPath,
  PathParametersAreOptional,
  PathValidationError,
  ValidationPathFailure,
  WorkFlowBuildException
}
import sttp.model.Uri
import cats.syntax.list._
import org.fluminous.services.ValidationServiceError
import sttp.client3.UriContext

import scala.annotation.tailrec

case class EndpointTemplate private (
  serverName: String,
  operationId: String,
  path: Seq[EndpointTemplate.PathItemOrParameter],
  pathParameters: List[(String, Boolean)],
  queryParameters: List[(String, Boolean)])
    extends ParameterFunctions {
  def endpoint(input: Map[String, Json]): Either[ValidationServiceError, Uri] = {
    for {
      query           <- validateRequiredParameters(operationId, queryParameters, input).map(_.toMap)
      pathParameters  <- validateRequiredParameters(operationId, pathParameters, input).map(_.toMap)
      substitutedPath = path.flatMap(_.fold(Some(_), pathParameters.get)).mkString
    } yield uri"http://$serverName/$substitutedPath?$query"
  }
  val usedParameters: Set[String] = (path.flatMap(_.right.toOption) ++ queryParameters.map(_._1)).toSet

}

object EndpointTemplate extends ParameterFunctions {
  type PathItemOrParameter = Either[String, String]
  def apply(
    serverName: String,
    operationId: String,
    path: String,
    pathParameters: List[Parameter],
    queryParameters: List[Parameter]
  ): Either[WorkFlowBuildException, EndpointTemplate] = {

    parsePath(operationId, path)
      .flatMap(checkByPath(operationId, pathParameters, path, _))
      .map(
        EndpointTemplate(
          serverName,
          operationId,
          _,
          pathParameters.map(p => (p.getName, isRequired(p))),
          queryParameters.map(p => (p.getName, isRequired(p)))
        )
      )
  }
  private def checkByPath(
    operationId: String,
    requiredParameters: List[Parameter],
    path: String,
    parsedPath: List[PathItemOrParameter]
  ): Either[WorkFlowBuildException, List[PathItemOrParameter]] = {
    val parsedParameterNames   = parsedPath.flatMap(_.right.toOption).toSet
    val requiredParameterNames = requiredParameters.map(_.getName).toSet
    List[Option[ValidationPathFailure]](
      requiredParameters.filter(isOptional).map(_.getName).toNel.map(PathParametersAreOptional),
      parsedParameterNames.diff(requiredParameterNames).toList.toNel.map(PathContainsNotDeclaredParameters),
      requiredParameterNames.diff(parsedParameterNames).toList.toNel.map(PathParametersAreMissingFromPath)
    ).flatten.toNel.map(PathValidationError(operationId, path, _)).toLeft(parsedPath)
  }

  @tailrec
  private def parsePath(
    operationId: String,
    path: String,
    readySeq: List[PathItemOrParameter] = List.empty
  ): Either[InvalidRestPath, List[PathItemOrParameter]] = {
    path.split(startVarSymbol, 2).toList match {
      case Nil         => Right(readySeq.reverse)
      case path :: Nil => Right((Left(path) +: readySeq).reverse)
      case path :: afterPath :: _ =>
        afterPath.split(endVarSymbol, 2).toList match {
          case _ :: Nil => Left(InvalidRestPath(operationId, path))
          case variable :: afterVariable :: _ =>
            parsePath(operationId, afterVariable, readySeq :+ Right(variable) :+ Left(path))
        }
    }
  }
  private val startVarSymbol = "{"
  private val endVarSymbol   = "{"
}
