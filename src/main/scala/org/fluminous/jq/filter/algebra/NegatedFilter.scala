package org.fluminous.jq.filter.algebra

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import cats.syntax.traverse._

final case class NegatedFilter(override val position: Int, filter: Filter) extends Filter {
  override val isSingleValued: Boolean = filter.isSingleValued
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    filter
      .transform(input)
      .flatMap(r =>
        r.map(
            _.asNumber
              .flatMap(_.toBigDecimal.map(d => -d))
              .map(Json.fromBigDecimal)
              .toRight(EvaluationException(position, "Could not be negated. Not a number"))
          )
          .sequence
      )
  }

  override val description: String = NegatedFilter.typeDescription.description
}

object NegatedFilter {

  implicit def typeDescription: Description[NegatedFilter] =
    new Description[NegatedFilter] {
      override val description: String = "negation"
    }
}
