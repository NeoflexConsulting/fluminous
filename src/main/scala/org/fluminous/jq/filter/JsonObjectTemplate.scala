package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Description

case class JsonObjectTemplate(override val position: Int, values: Map[String, Filter]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromFields(values.toSeq.flatMap { case (n, v) => v.transform(input).map(j => (n, j)) }))
  }
  override val description: String = JsonObjectTemplate.typeDescription.description
}

object JsonObjectTemplate {
  implicit def typeDescription: Description[JsonObjectTemplate] = new Description[JsonObjectTemplate] {
    override val description: String = "end of json object"
  }
}
