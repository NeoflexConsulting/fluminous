package org.fluminous.jq.filter.algebra

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import io.circe.syntax._

final case class BooleanConstant(override val position: Int, value: Boolean) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = Right(value.asJson)
  override val description: String                                       = BooleanConstant.typeDescription.description
}

object BooleanConstant {
  implicit def typeDescription: Description[BooleanConstant] =
    new Description[BooleanConstant] {
      override val description: String = "boolean constant"
    }
}
