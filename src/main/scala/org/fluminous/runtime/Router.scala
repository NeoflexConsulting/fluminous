package org.fluminous.runtime

import org.fluminous.routing.{
  ExecuteCondition,
  ExecuteFirstCondition,
  ExecuteFirstService,
  ExecuteService,
  Finish,
  FirstStep,
  IntermediateStep
}
import org.fluminous.runtime.exception.{ ExecutionRuntimeException, ServiceException }

import scala.annotation.tailrec

class Router[Rq, Rs](private val initialRuntime: UnpreparedExecutionRuntime[Rq, Rs]) {
  def routeRequest(input: Rq, routing: FirstStep): Either[ExecutionRuntimeException, Rs] = {
    val rc = executeFirstStep(routing, initialRuntime.setInput(input))
    executeNextStep(rc)
  }

  private def executeFirstStep(
    routing: FirstStep,
    er: ExecutionRuntime[Rs]
  ): Either[ExecutionRuntimeException, RouterContext[Rs]] = {
    routing match {
      case ExecuteFirstCondition(conditionName, ifTrueStep, ifFalseStep) =>
        for {
          conditionIsMet <- er.executeFirstCondition(conditionName)
        } yield {
          if (conditionIsMet) RouterContext(ifTrueStep, er) else RouterContext(ifFalseStep, er)
        }
      case ExecuteFirstService(serviceName, outputVariableName, nextStep) =>
        er.executeFirstService(serviceName, outputVariableName).map(er => RouterContext(nextStep, er))

    }
  }

  @tailrec
  private def executeNextStep(
    routeContext: Either[ExecutionRuntimeException, RouterContext[Rs]]
  ): Either[ExecutionRuntimeException, Rs] = {
    routeContext match {
      case Left(ex) => Left(ex)
      case Right(rc) =>
        rc.nextStep match {
          case Finish(outputVariableName) => rc.er.getOutput(outputVariableName)
          case _                          => executeNextStep(routeContext.flatMap(executeAction))
        }
    }
  }

  private def executeAction(rc: RouterContext[Rs]): Either[ExecutionRuntimeException, RouterContext[Rs]] = {
    rc.nextStep match {
      case ExecuteCondition(conditionName, inputVariableName, ifTrueStep, ifFalseStep) =>
        for {
          conditionIsMet <- rc.er.executeCondition(conditionName, inputVariableName)
        } yield {
          if (conditionIsMet) RouterContext(ifTrueStep, rc.er) else RouterContext(ifFalseStep, rc.er)
        }
      case ExecuteService(serviceName, inputVariableName, outputVariableName, nextStep) =>
        rc.er.executeService(serviceName, inputVariableName, outputVariableName).map(er => RouterContext(nextStep, er))
    }
  }

}
