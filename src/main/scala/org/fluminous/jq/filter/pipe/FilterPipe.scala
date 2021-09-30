package org.fluminous.jq.filter.pipe

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, EvaluationException}

final case class FilterPipe(override val position: Int, filters: Seq[Filter]) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    val initial: Either[EvaluationException, Json] = Right(input)
    filters.foldLeft(initial) { (json, filter) =>
      json match {
        case err @ Left(_) => err
        case Right(j)      => filter.transform(j)
      }
    }
  }

  override val description: String = FilterPipe.typeDescription.description
}

object FilterPipe {
  implicit def typeDescription: Description[FilterPipe] =
    new Description[FilterPipe] {
      override val description: String = "filters pipe"
    }
}
