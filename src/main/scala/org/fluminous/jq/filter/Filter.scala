package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException, Expression }
import cats.syntax.traverse._

trait Filter extends Expression {
  def transform(input: List[Json]): Either[EvaluationException, List[Json]] = input.map(transformSingle).sequence
  def transformSingle(input: Json): Either[EvaluationException, Json]
}

object Filter {
  implicit def typeDescription: Description[Filter] = new Description[Filter] {
    override val description: String = "filter"
  }
}
