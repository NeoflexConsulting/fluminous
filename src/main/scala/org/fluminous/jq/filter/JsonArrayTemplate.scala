package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Description

final case class JsonArrayTemplate(values: Seq[Either[Json, Filter]]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromValues(values.flatMap(valueToJson(input))))
  }

  private def valueToJson(input: Json)(value: Either[Json, Filter]): Option[Json] = {
    value.fold(Option(_), _.transform(input))
  }
  override val description: String = JsonArrayTemplate.typeDescription.description
}

object JsonArrayTemplate {
  implicit def typeDescription: Description[JsonArrayTemplate] = new Description[JsonArrayTemplate] {
    override val description: String = "json array"
  }
}
