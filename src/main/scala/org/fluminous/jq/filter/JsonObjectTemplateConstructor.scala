package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, Expression }

final case class JsonObjectTemplateConstructor(values: Map[String, Either[Json, Filter]]) extends Expression {
  override val description: String = JsonObjectTemplateConstructor.typeDescription.description
}

object JsonObjectTemplateConstructor {
  implicit def typeDescription: Description[JsonObjectTemplateConstructor] =
    new Description[JsonObjectTemplateConstructor] {
      override val description: String = "json object"
    }
}
