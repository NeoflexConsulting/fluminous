package org.fluminous.jq.filter.selector

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class SelectorByName(override val position: Int, field: String, parentFieldName: Option[String] = None)
    extends Filter with SelectorFunctions{
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        childJson <- jsonByFieldName(input, parentFieldName)
        jsonObject <- childJson.asObject.toRight(
                       EvaluationException(position, s"Trying to read field $field from json of type ${input.name}")
                     )
      } yield jsonObject(field).map(List(_)).getOrElse(List(Null))
    }
  }
  override val description: String = s"selector for field: $field"
}

object SelectorByName {
  implicit def typeDescription: Description[SelectorByName] = new Description[SelectorByName] {
    override val description: String = "selector by name"
  }
}
