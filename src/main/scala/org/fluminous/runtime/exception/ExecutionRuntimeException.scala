package org.fluminous.runtime.exception

sealed class ExecutionRuntimeException(val message: String) extends Exception(message)

final class NoServicesFoundException extends ExecutionRuntimeException("No services were found")
