package org.fluminous.jq.filter.algebra.bool

import io.circe.Json
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.algebra.AlgebraOperation
import io.circe.syntax._

final case class Or(override val position: Int) extends AlgebraOperation {
  override val priority: Int       = 1
  override val description: String = "or"
  override def execute(
    left: Json,
    isRightSingleValued: Boolean,
    right: => Either[EvaluationException, List[Json]]
  ): Either[EvaluationException, List[Json]] = {
    val leftBoolean = asBoolean(left)
    if (leftBoolean && isRightSingleValued) {
      Right(List(Json.True))
    } else {
      for {
        evaluated <- right
      } yield {
        evaluated.map(b => (leftBoolean || asBoolean(b)).asJson)
      }
    }
  }
}
