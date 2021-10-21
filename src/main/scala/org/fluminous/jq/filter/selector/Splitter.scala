package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

case class Splitter(override val position: Int, parentFieldName: Option[String] = None)
    extends Filter
    with SelectorFunctions {
  override val isSingleValued: Boolean = false

  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        childJson <- jsonByFieldName(input, parentFieldName)
        result <- childJson.asArray
                   .map(_.toList)
                   .orElse(input.asObject.map(_.toList.map(_._2)))
                   .toRight(
                     EvaluationException(
                       position,
                       s"Trying to split by elements json of type ${input.name}"
                     )
                   )
      } yield result
    }
  }
  override val description: String = s"splitter"
}

object Splitter {
  implicit def typeDescription: Description[Splitter] = new Description[Splitter] {
    override val description: String = "splitter"
  }
}
