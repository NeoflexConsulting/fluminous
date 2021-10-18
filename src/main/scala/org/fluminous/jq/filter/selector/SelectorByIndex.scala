package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class SelectorByIndex(override val position: Int, index: Int, parentFieldName: Option[String] = None)
    extends Filter
    with SelectorFunctions {
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        childJson <- jsonByFieldName(input, parentFieldName)
        jsonArray <- childJson.asArray.toRight(
                      EvaluationException(position, s"Trying to read $index element from json of type ${input.name}")
                    )
      } yield List(getElementByIndex(jsonArray, index))
    }
  }

  override val description: String = s"selector for index: $index"
}

object SelectorByIndex {
  implicit def typeDescription: Description[SelectorByIndex] = new Description[SelectorByIndex] {
    override val description: String = "selector by index"
  }
}
