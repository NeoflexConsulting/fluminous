package org.fluminous.runtime

import org.fluminous.routing.{ ExecuteFirstCondition, ExecuteFirstService, FirstStep }
import org.fluminous.runtime.exception.ServiceException

class Router[Rq, Rs](private val initialRuntime: UnpreparedExecutionRuntime[Rq, Rs]) {
  def routeRequest(input: Rq, routing: FirstStep): Either[ServiceException, Rs] = {
  /*  for {
      er <- runRouting(routing, initialRuntime.setInput(input))
    } yield {
      er.getOutput()
    }*/
    ???
  }

  private def runRouting(
    routing: FirstStep,
    er: ExecutionRuntime[Rs]
  ): Either[ServiceException, ExecutionRuntime[Rs]] = {
    ???
  }
}
