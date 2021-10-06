package org.fluminous.jq.filter.pipe

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }
import cats.syntax.foldable._

final case class Pipe(override val position: Int, filters: List[Filter]) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    filters.foldLeftM(input) { (json, filter) =>
      filter.transform(json)
    }
  }
  override val description: String = Pipe.typeDescription.description
}

object Pipe {
  implicit def typeDescription: Description[Pipe] =
    new Description[Pipe] {
      override val description: String = "filters pipe"
    }
}
