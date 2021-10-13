package org.fluminous.jq.filter.selector

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class SelectorByName(override val position: Int, field: String) extends Filter {
  override def transformSingle(input: Json): Either[EvaluationException, Json] = {
    if (input.isNull) {
      Right(input)
    } else {
      for {
        jsonObject <- input.asObject.toRight(
                       EvaluationException(position, s"Trying to read field $field from json of type ${input.name}")
                     )
      } yield jsonObject(field).getOrElse(Null)
    }
  }
  override val description: String = s"selector for field: $field"
}

object SelectorByName {
  implicit def typeDescription: Description[SelectorByName] = new Description[SelectorByName] {
    override val description: String = "selector by name"
  }
}
