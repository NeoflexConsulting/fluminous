package org.fluminous.jq.filter.boolean

import io.circe.Json
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.Filter
import io.circe.syntax._

final case class BooleanConstant(override val position: Int, value: Boolean) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = Right(value.asJson)
  override val description: String                                       = value.toString
}
