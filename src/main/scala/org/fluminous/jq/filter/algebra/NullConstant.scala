package org.fluminous.jq.filter.algebra

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class NullConstant(override val position: Int) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = Right(Null)
  override val description: String                                       = NullConstant.typeDescription.description
}

object NullConstant {
  implicit def typeDescription: Description[NullConstant] =
    new Description[NullConstant] {
      override val description: String = "null"
    }
}
