package org.fluminous.jq.filter.algebra.bool

import io.circe.Json
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.algebra.AlgebraOperation
import io.circe.syntax._

final case class And(override val position: Int) extends AlgebraOperation {
  override val priority: Int       = 2
  override val description: String = "and"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    val leftBoolean = asBoolean(left)
    if (!leftBoolean) {
      Right(Json.False)
    } else {
      for {
        evaluated <- right
      } yield {
        (leftBoolean && asBoolean(evaluated)).asJson
      }
    }
  }
}
