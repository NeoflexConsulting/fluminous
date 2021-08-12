package org.fluminous.runtime.exception

sealed class ExecutionRuntimeException(val message: String, val cause: Option[Exception] = Option.empty)
    extends Exception(message, cause.orNull)

final class NoServicesFoundException extends ExecutionRuntimeException("No services were found")

final class VariableNotFoundException(variableName: String, expectedType: String)
    extends ExecutionRuntimeException(s"Variable with name ${variableName} of type ${expectedType} not found")

final class ConditionNotFoundException(conditionName: String)
    extends ExecutionRuntimeException(s"Condition with name ${conditionName} not found")

final class InputValueNotFoundException(expectedType: String)
    extends ExecutionRuntimeException(s"Input value of type ${expectedType} not found")

final class ServiceNotFoundException(serviceName: String)
    extends ExecutionRuntimeException(s"Service with name ${serviceName} not found")

final class ServiceExecutionException(exception: ServiceException)
    extends ExecutionRuntimeException(s"Error occurred during invocation of service ${exception.serviceName}", Some(exception))
