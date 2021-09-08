package org.fluminous.runtime.exception

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
      s"""Jq expression should start with $${ and end with }, but found: "${foundExpression}""""
    )

case class JqParserError(exception: ParserException) extends WorkFlowBuildException(exception)

case class UnsupportedStateType(stateType: String)
    extends WorkFlowBuildException(s"""Unsupported state type: $stateType"""")

case class ConditionNotFound(stateName: String)
    extends WorkFlowBuildException(s"Condition in state ${stateName} not found")

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

object WorkFlowBuildException {
  implicit def toEither[A](e: WorkFlowBuildException): Either[WorkFlowBuildException, A] = Left(e)
}
