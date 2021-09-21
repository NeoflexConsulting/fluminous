package org.fluminous.jq.filter.json

import io.circe.Json
import org.fluminous.jq.Description
import org.fluminous.jq.filter.Filter

final case class JsonArray(override val position: Int, values: Seq[Filter]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromValues(values.flatMap(_.transform(input))))
  }

  override val description: String = JsonArray.typeDescription.description
}

object JsonArray {
  implicit def typeDescription: Description[JsonArray] = new Description[JsonArray] {
    override val description: String = "end of json array"
  }
}
