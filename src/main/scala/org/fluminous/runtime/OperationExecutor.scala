package org.fluminous.runtime

import cats.MonadThrow
import io.circe.Json.{ fromJsonObject, fromValues }
import io.circe.{ Json, JsonObject }
import org.fluminous.jq.filter.Filter
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monadError._

final case class OperationExecutor[F[_]: MonadThrow](stateName: String, inputFilter: Filter, outputFilter: Filter)
    extends ExecutorFunctions {
  private val monadError = MonadThrow[F]
  import monadError._
  def execute(actions: List[Json => F[Json]], nextStep: Json => F[Json])(input: Json): F[Json] = {
    for {
      stateJson <- fromEither(
                    inputFilter
                      .transform(input)
                      .flatMap(getUnique(NonUniqueInputState(stateName), _))
                  ).ensure(InputStateFilterEvaluatedToNull(stateName))(v => !v.isNull)
      mergedJson <- actions.foldM(stateJson) {
                     case (js, a) => a(js).map(r => asNonNull(r).map(merge(_, js)).getOrElse(js))
                   }
      updatedJson <- fromEither(
                      outputFilter.transform(mergedJson).flatMap(getUnique(NonUniqueOutputState(stateName), _))
                    ).ensure(OutputStateFilterEvaluatedToNull(stateName))(v => !v.isNull)
      result <- nextStep(updatedJson)
    } yield result
  }
  private def asNonNull(json: Json): Option[Json] = {
    if (json.isNull)
      None
    else
      Some(json)
  }

  private def merge(to: Json, from: Json): Json =
    from.arrayOrObject(to, mergeArrays(_, to), mergeObjects(_, to))

  private def mergeObjects(fromObject: JsonObject, toJson: Json): Json = toJson.asObject match {
    case Some(toObject) =>
      fromJsonObject(
        fromObject.toIterable.foldLeft(toObject) {
          case (to, (fromKey, fromValue)) =>
            toObject(fromKey).fold(to.add(fromKey, fromValue)) { toValue =>
              to.add(fromKey, merge(fromValue, toValue))
            }
        }
      )
    case _ => Json.fromJsonObject(fromObject)
  }

  private def mergeArrays(fromArray: Vector[Json], to: Json): Json = to.asArray match {
    case Some(toArray) => fromValues(toArray ++ fromArray)
    case _             => Json.fromValues(fromArray)
  }
}
