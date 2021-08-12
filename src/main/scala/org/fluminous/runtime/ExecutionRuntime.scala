package org.fluminous.runtime

import org.fluminous.runtime.exception.{ ExecutionRuntimeException, ServiceException }

class ExecutionRuntime[Rs](
  private val runtime: Map[String, TypeInfo],
  private val outputGetter: (String, Map[String, TypeInfo]) => Either[ExecutionRuntimeException, Rs]) {
  type OK = Unit
  def executeFirstService(serviceName: String, outputVariableName: String): Either[ServiceException, OK] = ???
  def executeFirstCondition(conditionName: String): Either[ServiceException, Boolean]                    = ???
  def executeService(
    serviceName: String,
    inputVariableName: String,
    outputVariableName: String
  ): Either[ServiceException, OK]                                                                           = ???
  def executeCondition(conditionName: String, inputVariableName: String): Either[ServiceException, Boolean] = ???
  def getOutput(variableName: String): Either[ExecutionRuntimeException, Rs]                                = this.outputGetter(variableName, runtime)
}
