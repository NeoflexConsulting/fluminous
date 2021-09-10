package org.fluminous.routing

import cats.data.NonEmptyList
import org.fluminous.jq.ParserException

sealed abstract class WorkFlowBuildException(message: String, cause: Throwable) extends Exception(message, cause) {
  def this(message: String) = {
    this(s"Error during workflow building: $message", null)
  }
  def this(throwable: Throwable) = {
    this(s"Error during workflow building", throwable)
  }
}

case class ExpressionNotFound(foundExpression: String)
    extends WorkFlowBuildException(
      s"""Jq expression should start with $${ and end with }, but found: "$foundExpression""""
    )

case class JqParserError(expression: String, exception: ParserException)
    extends WorkFlowBuildException(s"Error parsing Jq expression: $expression", exception)

case class UnsupportedStateType(stateType: String) extends WorkFlowBuildException(s"Unsupported state type: $stateType")

case class ConditionNotFound(stateName: String)
    extends WorkFlowBuildException(s"Condition in state $stateName not found")

case class InitialStateNotFound() extends WorkFlowBuildException("Initial state not found")

case class InvalidStateReference(state: String, referencedState: String)
    extends WorkFlowBuildException(s"State $state references to invalid next state $referencedState")

case class OperationMissing(functionDefinition: String)
    extends WorkFlowBuildException(s"Operation definition is missing in function definition $functionDefinition")

case class InvalidOperation(functionDefinition: String)
    extends WorkFlowBuildException(
      s"""Invalid operation in function definition $functionDefinition. """ +
        s"""Operation should be in format "document#operationId""""
    )

case class OpenAPIParsingError(document: String, errors: List[String])
    extends WorkFlowBuildException(
      s"""Open API document $document could not be parsed. Parsing errors: \n${errors.mkString("\n")}"""
    )

case class DuplicatedOperationId(document: String, operationId: String)
    extends WorkFlowBuildException(s"Operation with operationId $operationId is duplicated in document $document")

case class OperationNotFoundInOpenAPI(document: String, operationId: String)
    extends WorkFlowBuildException(s"Operation with operationId $operationId not found in document $document")

case class ServerNotFoundForDocument(document: String)
    extends WorkFlowBuildException(s"Server name is not defined in settings for document $document")

final case class ServiceNotFoundException(serviceName: String)
    extends WorkFlowBuildException(s"Service with name $serviceName not found")

final case class UnsupportedHttpMethod(serviceName: String, method: String)
    extends WorkFlowBuildException(s"Http method $method used for operation $serviceName is unsupported")

final case class InvalidRestPath(serviceName: String, path: String)
    extends WorkFlowBuildException(s"Invalid path $path for operation: $serviceName")

sealed abstract class ValidationPathFailure(val message: String)

final case class PathParametersAreMissingFromPath(parameterNames: NonEmptyList[String])
    extends ValidationPathFailure(s"Declared path parameters ${parameterNames.toList.mkString(",")} are missing in path")

final case class PathContainsNotDeclaredParameters(parameterNames: NonEmptyList[String])
    extends ValidationPathFailure(s"Path parameters ${parameterNames.toList.mkString(",")} are not declared")

final case class PathParametersAreOptional(parameterNames: NonEmptyList[String])
    extends ValidationPathFailure(s"Path parameters: ${parameterNames.toList.mkString(",")} could not be optional")

final case class PathValidationError(
  serviceName: String,
  path: String,
  validationFailure: NonEmptyList[ValidationPathFailure])
    extends WorkFlowBuildException(
      s"Declaration of $serviceName with path $path is invalid: ${validationFailure.map(_.message).toList.mkString("\n")}"
    )

object WorkFlowBuildException {
  implicit def toEither[A](e: WorkFlowBuildException): Either[WorkFlowBuildException, A] = Left(e)
}
