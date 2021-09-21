package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Description

final case class JsonArrayTemplate(override val position: Int, values: Seq[Filter]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromValues(values.flatMap(_.transform(input))))
  }

  override val description: String = JsonArrayTemplate.typeDescription.description
}

object JsonArrayTemplate {
  implicit def typeDescription: Description[JsonArrayTemplate] = new Description[JsonArrayTemplate] {
    override val description: String = "end of json array"
  }
}
