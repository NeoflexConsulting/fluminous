package org.fluminous.jq.filter.selector

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class Selector(override val position: Int, field: String) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
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

object Selector {
  implicit def typeDescription: Description[Selector] = new Description[Selector] {
    override val description: String = "field selector"
  }
  implicit def identityDescription: Description[IdentitySelector] = new Description[IdentitySelector] {
    override val description: String = "field selector"
  }
}
