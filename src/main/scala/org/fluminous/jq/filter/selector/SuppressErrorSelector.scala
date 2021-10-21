package org.fluminous.jq.filter.selector

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class SuppressErrorSelector(override val position: Int, filter: Filter) extends Filter {
  override val isSingleValued = filter.isSingleValued
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    Right(filter.transform(input).toOption.getOrElse(List(Null)))
  }
  override val description: String = SuppressErrorSelector.typeDescription.description
}

object SuppressErrorSelector {
  implicit def typeDescription: Description[SuppressErrorSelector] = new Description[SuppressErrorSelector] {
    override val description: String = "suppress error filter"
  }
}
