package org.fluminous.runtime

import cats.Monad
import org.fluminous.runtime.exception.{ ExecutionRuntimeException, ServiceException }

class UnpreparedExecutionRuntime[F[_]: Monad, Rq, Rs](
  private val inputSetter: Rq => Map[String, TypeInfo[F]],
  private val outputGetter: (String, Map[String, TypeInfo[F]]) => Either[ExecutionRuntimeException, Rs]) {
  def setInput(input: Rq): ExecutionRuntime[F, Rs] =
    new ExecutionRuntime[F, Rs](this.inputSetter(input), this.outputGetter)
}
