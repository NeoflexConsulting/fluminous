package org.fluminous.runtime

import cats.MonadThrow
import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import cats.syntax.flatMap._
import cats.syntax.functor._

//TODO replace MonadThrow for MonadError[F, ExecutionRuntimeException] and MonadError[F, ServiceException]
final case class SwitchExecutor[F[_]: MonadThrow](
  stateName: String,
  inputFilter: Filter,
  outputFilter: Filter,
  condition: Filter) {
  private val monadError = MonadThrow[F]
  import monadError._
  def execute(ifTrue: Json => F[Json], ifFalse: Json => F[Json])(input: Json): F[Json] = {
    for {
      stateJson       <- fromOption(inputFilter.transform(input), InputStateFilterEvaluatedToNull(stateName))
      conditionResult <- evaluateCondition(stateName, condition, stateJson)
      updatedJson     <- fromOption(outputFilter.transform(stateJson), OutputStateFilterEvaluatedToNull(stateName))
      result          <- if (conditionResult) ifTrue(updatedJson) else ifFalse(updatedJson)
    } yield result
  }

  private def evaluateCondition(stateName: String, condition: Filter, input: Json): F[Boolean] = {
    val result = condition.transform(input)
    fromOption(result.flatMap(_.asBoolean), ConditionEvaluatedToNonBoolean(stateName, result.getOrElse(Null)))
  }
}
