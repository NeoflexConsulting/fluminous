package org.fluminous.jq.filter.algebra

import io.circe.{ Json, JsonNumber }
import org.fluminous.jq.{ EvaluationException, Expression }
import org.fluminous.jq.filter.algebra.bool.Or
import org.fluminous.jq.filter.algebra.bool.And

trait AlgebraOperation extends Expression {
  val priority: Int
  def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json]
  protected def asBoolean(json: Json): Boolean = {
    json.asBoolean.getOrElse {
      json.asNull.fold(true)(_ => false)
    }
  }
  protected def asNumber(json: Json): Option[JsonNumber] = {
    json.asNull.fold(json)(_ => json).asNumber
  }
}

object AlgebraOperation {
  val Zero: Json = Json.fromInt(0)
}
