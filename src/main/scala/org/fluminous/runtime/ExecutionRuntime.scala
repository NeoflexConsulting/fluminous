package org.fluminous.runtime

import cats.Monad
import cats.data.EitherT
import io.circe.{ Json, JsonObject }
import io.circe.Json.{ fromJsonObject, fromValues, Null }
import org.fluminous.jq.filter.Filter
import org.fluminous.routing.{ Operation, RoutingStep, Switch }
import org.fluminous.runtime.exception.{
  ActionFilterEvaluatedToNull,
  ConditionEvaluatedToNonBoolean,
  ExecutionRuntimeException,
  InputStateFilterEvaluatedToNull,
  OutputStateFilterEvaluatedToNull,
  ServiceExecutionException,
  ServiceNotFoundException
}
import org.fluminous.services.Service

case class ExecutionRuntime[F[_]: Monad](
  services: Map[String, Service[F]],
  contextJson: Json,
  currentStep: RoutingStep) {

  def executeOperation(operation: Operation): EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F]] = {
    import EitherT._
    val serviceName = operation.functionName
    for {
      service       <- fromEither[F](services.get(serviceName).toRight(ServiceNotFoundException(serviceName)))
      argumentsJson <- fromEither[F](getArguments(operation, contextJson))
      result        <- service.invoke(argumentsJson).leftMap(ServiceExecutionException)
      updatedJson   <- fromEither[F](updateContextJson(operation, contextJson, result))
    } yield this.copy(contextJson = updatedJson, currentStep = operation.nextStep)
  }

  def executeSwitch(switch: Switch): Either[ExecutionRuntimeException, ExecutionRuntime[F]] = {
    for {
      stateJson <- switch.inputFilter.transform(contextJson).toRight(InputStateFilterEvaluatedToNull(switch.stateName))
      result    <- evaluateCondition(switch.stateName, switch.condition, stateJson)
      updatedJson <- switch.outputFilter
                      .transform(stateJson)
                      .toRight(OutputStateFilterEvaluatedToNull(switch.stateName))
    } yield {
      if (result) {
        this.copy(contextJson = updatedJson, currentStep = switch.ifTrueStep)
      } else {
        this.copy(contextJson = updatedJson, currentStep = switch.ifFalseStep)
      }
    }
  }

  private def evaluateCondition(
    stateName: String,
    condition: Filter,
    input: Json
  ): Either[ExecutionRuntimeException, Boolean] = {
    val result = condition.transform(input)
    result.flatMap(_.asBoolean).toRight(ConditionEvaluatedToNonBoolean(stateName, result.getOrElse(Null)))
  }

  private def getArguments(
    operation: Operation,
    contextJson: Json
  ): Either[ExecutionRuntimeException, Map[String, Json]] = {
    val stateName = operation.stateName
    for {
      stateJson     <- operation.inputFilter.transform(contextJson).toRight(InputStateFilterEvaluatedToNull(stateName))
      actionJson    <- operation.fromStateDataFilter.transform(stateJson).toRight(ActionFilterEvaluatedToNull(stateName))
      argumentsJson = operation.arguments.flatMap { case (k, v) => v.transform(actionJson).map(j => (k, j)) }
    } yield argumentsJson
  }

  private def updateContextJson(
    operation: Operation,
    contextJson: Json,
    result: Json
  ): Either[ExecutionRuntimeException, Json] = {
    val mergingJson = operation.resultsFilter.transform(result).flatMap(j => operation.toStateDataFilter.transform(j))
    val mergedJson  = mergingJson.map(j => merge(j, contextJson)).getOrElse(contextJson)
    operation.outputFilter.transform(mergedJson).toRight(OutputStateFilterEvaluatedToNull(operation.stateName))
  }

  private def merge(left: Json, right: Json): Json =
    left.arrayOrObject(right, mergeArrays(_, right), mergeObjects(_, right))

  private def mergeObjects(left: JsonObject, right: Json): Json = right.asObject match {
    case Some(rhs) =>
      fromJsonObject(
        left.toIterable.foldLeft(rhs) {
          case (acc, (key, value)) =>
            rhs(key).fold(acc.add(key, value)) { r =>
              acc.add(key, merge(value, r))
            }
        }
      )
    case _ => right
  }

  private def mergeArrays(left: Vector[Json], right: Json): Json = right.asArray match {
    case Some(rhs) => fromValues(rhs ++ left)
    case _         => right
  }
}
