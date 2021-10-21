package org.fluminous.jq.filter.algebra

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

case class IntegerNumber(override val position: Int, value: Int) extends Filter {
  override val isSingleValued: Boolean                                         = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = Right(List(Json.fromInt(value)))
  override val description: String                                             = IntegerNumber.typeDescription.description
}

object IntegerNumber {
  implicit def typeDescription: Description[IntegerNumber] = new Description[IntegerNumber] {
    override val description: String = "integer number"
  }
}
