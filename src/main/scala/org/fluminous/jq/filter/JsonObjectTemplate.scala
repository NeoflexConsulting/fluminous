package org.fluminous.jq.filter

import io.circe.Json

case class JsonObjectTemplate(values: Map[String, Either[Json, Filter]]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromFields(values.mapValues(valueToJson(input))))
  }
  private def valueToJson(input: Json)(value: Either[Json, Filter]): Json = {
    value.fold(Option(_), _.transform(input)).getOrElse(Json.Null)
  }
}
