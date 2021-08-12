package org.fluminous.runtime.exception

sealed abstract class ServiceException private (val serviceName: String, message: String, cause: Throwable)
    extends Exception(message, cause) {
  def this(serviceName: String, message: String) = {
    this(serviceName, s"Error during service $serviceName invocation: $message", null)
  }
  def this(serviceName: String, throwable: Throwable) = {
    this(serviceName, s"Error during service $serviceName invocation", throwable)
  }
}

class IncompatibleTypeException(serviceName: String, expectedType: String, actualType: String)
    extends ServiceException(serviceName, s"Incompatible input type. Expected: $expectedType, actual: $actualType")
