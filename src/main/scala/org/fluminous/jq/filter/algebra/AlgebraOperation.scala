package org.fluminous.jq.filter.algebra

import io.circe.{ Json, JsonNumber }
import org.fluminous.jq.filter.algebra.AlgebraOperation.Zero
import org.fluminous.jq.{ EvaluationException, Expression }

trait AlgebraOperation extends Expression {
  val priority: Int
  def execute(
    left: Json,
    isRightSingleValued: Boolean,
    right: => Either[EvaluationException, List[Json]]
  ): Either[EvaluationException, List[Json]]
  protected def asBoolean(json: Json): Boolean = {
    json.asBoolean.getOrElse {
      json.asNull.fold(true)(_ => false)
    }
  }
  protected def asNumber(json: Json): Option[JsonNumber] = {
    json.asNull.fold(json)(_ => Zero).asNumber
  }
}

object AlgebraOperation {
  val Zero: Json = Json.fromInt(0)
}
