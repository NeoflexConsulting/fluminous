package org.fluminous.jq.filter.sequence

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.{ Filter, SequenceFilter }
import cats.syntax.traverse._

final case class FilterSequence(override val position: Int, filters: List[Filter]) extends SequenceFilter {
  override def transform(input: List[Json]): Either[EvaluationException, List[Json]] = {
    input.flatMap(json => filters.map(_.transformSingle(json))).sequence
  }
  override val description: String = FilterSequence.typeDescription.description

}

object FilterSequence {
  implicit def typeDescription: Description[FilterSequence] =
    new Description[FilterSequence] {
      override val description: String = "filters sequence"
    }
}
