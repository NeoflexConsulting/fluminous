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

case class ActionNotFound(stateName: String) extends WorkFlowBuildException(s"Action in state ${stateName} not found")

case class ConditionNotFound(stateName: String)
    extends WorkFlowBuildException(s"Condition in state ${stateName} not found")

case class InitialStateNotFound() extends WorkFlowBuildException("Initial state not found")

case class InvalidStateReference(state: String, referencedState: String)
    extends WorkFlowBuildException(s"state $state references to invalid next state $referencedState")

object WorkFlowBuildException {
  implicit def toEither[A](e: WorkFlowBuildException): Either[WorkFlowBuildException, A] = Left(e)
}
