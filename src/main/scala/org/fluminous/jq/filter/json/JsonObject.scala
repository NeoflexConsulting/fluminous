package org.fluminous.jq.filter.json

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.Description

case class JsonObject(override val position: Int, values: Map[String, Filter]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromFields(values.toSeq.flatMap { case (n, v) => v.transform(input).map(j => (n, j)) }))
  }
  override val description: String = JsonObject.typeDescription.description
}

object JsonObject {
  implicit def typeDescription: Description[JsonObject] = new Description[JsonObject] {
    override val description: String = "end of json object"
  }
}
