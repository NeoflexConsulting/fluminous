package org.fluminous.jq.filter.algebra

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class NegatedFilter(override val position: Int, filter: Filter) extends Filter {
  override def transformSingle(input: Json): Either[EvaluationException, Json] = {
    input.asNumber
      .flatMap(_.toBigDecimal.map(d => -d))
      .map(Json.fromBigDecimal)
      .toRight(EvaluationException(position, "Could not be negated. Not a number"))
  }

  override val description: String = NegatedFilter.typeDescription.description
}

object NegatedFilter {

  implicit def typeDescription: Description[NegatedFilter] =
    new Description[NegatedFilter] {
      override val description: String = "negation"
    }
}
