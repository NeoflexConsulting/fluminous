package org.fluminous.jq.filter.sequence

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import cats.syntax.traverse._

final case class FilterSequence(override val position: Int, filters: List[Filter]) extends Filter {
  override val isSingleValued: Boolean = false
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    filters.map(_.transform(input)).flatSequence
  }
  override val description: String = FilterSequence.typeDescription.description

}

object FilterSequence {
  implicit def typeDescription: Description[FilterSequence] =
    new Description[FilterSequence] {
      override val description: String = "filters sequence"
    }
}
