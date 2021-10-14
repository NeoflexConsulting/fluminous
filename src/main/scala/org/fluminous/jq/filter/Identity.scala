package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.EvaluationException

case object Identity extends Filter {
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] =
    Right(List(input))
  override val position: Int       = 0
  override val description: String = "identity"
}
