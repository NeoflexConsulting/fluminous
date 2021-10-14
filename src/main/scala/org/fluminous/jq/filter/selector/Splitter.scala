package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

case class Splitter(override val position: Int) extends Filter {
  override val isSingleValued: Boolean = false

  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    input.asArray
      .map(_.toList)
      .orElse(input.asObject.map(_.toList.map(_._2)))
      .toRight(
        EvaluationException(
          position,
          s"Trying to split by elements json of type ${input.name}"
        )
      )
  }
  override val description: String = s"splitter"
}

object Splitter {
  implicit def typeDescription: Description[Splitter] = new Description[Splitter] {
    override val description: String = "splitter"
  }
}
