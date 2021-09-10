package org.fluminous.runtime

import io.circe.{DecodingFailure, Json}
import org.fluminous.services.ServiceException

sealed class ExecutionRuntimeException(val message: String, val cause: Option[Exception] = Option.empty)
    extends Exception(message, cause.orNull)

case class ResponseDeserializationException(deserializationError: DecodingFailure)
    extends ExecutionRuntimeException("Final output JSON is not suitable.", Some(deserializationError))

final case class NoServicesFoundException() extends ExecutionRuntimeException("No services were found")

final case class InputStateFilterEvaluatedToNull(stateName: String)
    extends ExecutionRuntimeException(s"Input state filter for state ${stateName} evaluated to null")

final case class OutputStateFilterEvaluatedToNull(stateName: String)
    extends ExecutionRuntimeException(s"Output state filter for state ${stateName} evaluated to null")

final case class ActionFilterEvaluatedToNull(stateName: String)
    extends ExecutionRuntimeException(s"FromState action filter for state ${stateName} evaluated to null")

final case class ConditionEvaluatedToNonBoolean(stateName: String, value: Json)
    extends ExecutionRuntimeException(s"Condition in state ${stateName} evaluated to non-boolean value: $value")

final case class ServiceExecutionException(exception: ServiceException)
    extends ExecutionRuntimeException(
      s"Error occurred during invocation of service ${exception.serviceName}",
      Some(exception)
    )
