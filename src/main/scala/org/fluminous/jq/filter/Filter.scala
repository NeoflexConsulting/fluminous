package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException, Expression }

trait Filter extends Expression {
  val isSingleValued: Boolean
  def transform(input: Json): Either[EvaluationException, List[Json]]
}

object Filter {
  implicit def typeDescription: Description[Filter] = new Description[Filter] {
    override val description: String = "filter"
  }
}
