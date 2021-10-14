package org.fluminous.jq.filter.pipe

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }
import cats.syntax.foldable._
import cats.syntax.traverse._

final case class Pipe(override val position: Int, filters: List[Filter]) extends Filter {
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    filters.foldLeftM(List(input)) { (jsonList, filter) =>
      jsonList.map(filter.transform).flatSequence
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
