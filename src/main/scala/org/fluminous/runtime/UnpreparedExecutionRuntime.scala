package org.fluminous.runtime

import org.fluminous.runtime.exception.{ExecutionRuntimeException, ServiceException}

class UnpreparedExecutionRuntime[Rq, Rs](
  private val inputSetter: Rq => Map[String, TypeInfo],
  private val outputGetter: (String, Map[String, TypeInfo]) => Either[ExecutionRuntimeException, Rs]) {
  def setInput(input: Rq): ExecutionRuntime[Rs] =
    new ExecutionRuntime[Rs](this.inputSetter(input), this.outputGetter)
}
