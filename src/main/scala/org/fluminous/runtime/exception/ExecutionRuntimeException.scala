package org.fluminous.runtime.exception

sealed class ExecutionRuntimeException(val message: String) extends Exception(message)

final class NoServicesFoundException extends ExecutionRuntimeException("No services were found")

final class VariableNotFoundException(variableName: String, expectedType: String)
    extends ExecutionRuntimeException(s"Variable with name ${variableName} of type ${expectedType} not found")
