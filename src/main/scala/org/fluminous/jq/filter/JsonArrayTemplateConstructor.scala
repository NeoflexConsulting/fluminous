package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, Expression }

final case class JsonArrayTemplateConstructor(values: Seq[Either[Json, Filter]]) extends Expression {
  override val description: String = JsonArrayTemplateConstructor.typeDescription.description
}

object JsonArrayTemplateConstructor {
  implicit def typeDescription: Description[JsonArrayTemplateConstructor] =
    new Description[JsonArrayTemplateConstructor] {
      override val description: String = "json array"
    }
}
