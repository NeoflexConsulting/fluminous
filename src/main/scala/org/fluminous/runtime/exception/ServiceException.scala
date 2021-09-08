package org.fluminous.runtime.exception

import io.circe.DecodingFailure

sealed abstract class ServiceException private (val serviceName: String, message: String, cause: Throwable)
    extends Exception(message, cause) {
  def this(serviceName: String, message: String) = {
    this(serviceName, s"Error during service $serviceName invocation: $message", null)
  }
  def this(serviceName: String, throwable: Throwable) = {
    this(serviceName, s"Error during service $serviceName invocation", throwable)
  }
}

case class DeserializationException(override val serviceName: String, deserializationError: DecodingFailure)
    extends ServiceException(serviceName, deserializationError.message)

case class NotFoundInputParameter(override val serviceName: String, expectedParameter: String)
    extends ServiceException(serviceName, s"Expected input parameter $expectedParameter not found")

class IncompatibleTypeException(serviceName: String, expectedType: String, actualType: String)
    extends ServiceException(serviceName, s"Incompatible input type. Expected: $expectedType, actual: $actualType")

class RequiredInputParameterIsMissing(serviceName: String, parameterName: String)
    extends ServiceException(serviceName, s"Required input parameter $parameterName is missing")

class JSONInputParameterType(serviceName: String, parameterName: String)
    extends ServiceException(serviceName, s"Parameter $parameterName is JSON Object, while JSON value expected")
