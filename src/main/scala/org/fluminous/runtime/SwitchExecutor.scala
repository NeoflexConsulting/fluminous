package org.fluminous.runtime

import cats.MonadThrow
import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._

//TODO replace MonadThrow for MonadError[F, ExecutionRuntimeException] and MonadError[F, ServiceException]
final case class SwitchExecutor[F[_]: MonadThrow](
  stateName: String,
  inputFilter: Filter,
  outputFilter: Filter,
  condition: Filter)
    extends ExecutorFunctions {
  private val monadError = MonadThrow[F]
  import monadError._
  def execute(ifTrue: Json => F[Json], ifFalse: Json => F[Json])(input: Json): F[Json] = {
    for {
      stateJson <- fromEither(inputFilter.transform(List(input)).flatMap(getUnique(NonUniqueInputState(stateName), _)))
                    .ensure(InputStateFilterEvaluatedToNull(stateName))(v => !v.isNull)
      conditionResult <- evaluateCondition(stateName, condition, stateJson)
      updatedJson <- fromEither(
                      outputFilter.transform(List(stateJson)).flatMap(getUnique(NonUniqueOutputState(stateName), _))
                    ).ensure(OutputStateFilterEvaluatedToNull(stateName))(v => !v.isNull)
      result <- if (conditionResult) ifTrue(updatedJson) else ifFalse(updatedJson)
    } yield result
  }

  private def evaluateCondition(stateName: String, condition: Filter, input: Json): F[Boolean] = {
    val result = condition.transform(List(input)).flatMap(getUnique(NonUniqueCondition(stateName), _))
    for {
      json <- fromEither(result.map(_.asBoolean))
      bool <- fromOption(json, ConditionEvaluatedToNonBoolean(stateName, result.getOrElse(Null)))
    } yield bool
  }
}
