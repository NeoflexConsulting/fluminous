package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException, Expression }

trait Filter extends Expression {
  def transform(input: Json): Either[EvaluationException, Json]
}

object Filter {
  implicit def typeDescription: Description[Filter] = new Description[Filter] {
    override val description: String = "filter"
  }
}
