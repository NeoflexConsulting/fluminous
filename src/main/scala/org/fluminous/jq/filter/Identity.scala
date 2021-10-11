package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.EvaluationException

case object Identity extends Filter {
  override def transformSingle(input: Json): Either[EvaluationException, Json] =
    Right(input)
  override val position: Int       = 0
  override val description: String = "identity"
}
