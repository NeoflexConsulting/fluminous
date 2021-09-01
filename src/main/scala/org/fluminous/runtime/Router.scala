package org.fluminous.runtime

import cats.Monad
import cats.data.EitherT
import org.fluminous.routing.{
  Switch,
  ExecuteFirstCondition,
  ExecuteFirstService,
  Operation,
  Finish,
  FirstStep
}
import org.fluminous.runtime.exception.ExecutionRuntimeException

class Router[F[_]: Monad, Rq, Rs](private val initialRuntime: ExecutionRuntimeTemplate[F, Rq, Rs]) {
  def routeRequest(input: Rq, routing: FirstStep): F[Either[ExecutionRuntimeException, Rs]] = {
    val rc = executeFirstStep(routing, initialRuntime.setInput(input))
    executeNextStep(rc).value
  }

  private def executeFirstStep(
    routing: FirstStep,
    er: ExecutionRuntime[F, Rs]
  ): EitherT[F, ExecutionRuntimeException, RoutingContext[F, Rs]] = {
    routing match {
      case ExecuteFirstCondition(conditionName, ifTrueStep, ifFalseStep) =>
        for {
          conditionIsMet <- EitherT.fromEither[F](er.executeFirstCondition(conditionName))
        } yield {
          if (conditionIsMet) RoutingContext(ifTrueStep, er) else RoutingContext(ifFalseStep, er)
        }
      case ExecuteFirstService(serviceName, outputVariableName, nextStep) =>
        er.executeFirstService(serviceName, outputVariableName).map(er => RoutingContext(nextStep, er))

    }
  }

  private def executeNextStep(
    routeContext: EitherT[F, ExecutionRuntimeException, RoutingContext[F, Rs]]
  ): EitherT[F, ExecutionRuntimeException, Rs] = {
    routeContext.flatMap { rc =>
      rc.nextStep match {
        case Finish(outputVariableName) => EitherT.fromEither(rc.er.getOutput(outputVariableName))
        case _                          => executeNextStep(routeContext.flatMap(executeAction))
      }
    }
  }

  private def executeAction(rc: RoutingContext[F, Rs]): EitherT[F, ExecutionRuntimeException, RoutingContext[F, Rs]] = {
    rc.nextStep match {
      case Switch(conditionName, inputVariableName, ifTrueStep, ifFalseStep) =>
        for {
          conditionIsMet <- EitherT.fromEither[F](rc.er.executeCondition(conditionName, inputVariableName))
        } yield {
          if (conditionIsMet) RoutingContext(ifTrueStep, rc.er) else RoutingContext(ifFalseStep, rc.er)
        }
      case Operation(serviceName, inputVariableName, outputVariableName, nextStep) =>
        rc.er.executeService(serviceName, inputVariableName, outputVariableName).map(er => RoutingContext(nextStep, er))
    }
  }

}
