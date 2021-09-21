package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, Expression }

final case class JsonObjectTemplateConstructor(override val position: Int, values: Map[String, Filter])
    extends Expression {
  override val description: String = JsonObjectTemplateConstructor.typeDescription.description
}

object JsonObjectTemplateConstructor {
  implicit def typeDescription: Description[JsonObjectTemplateConstructor] =
    new Description[JsonObjectTemplateConstructor] {
      override val description: String = "json object"
    }
}
