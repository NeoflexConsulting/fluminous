package org.fluminous.jq.filter

import io.circe.Json

case class JsonObjectTemplate(values: Map[String, Either[Json, Filter]]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    Option(Json.fromFields(values.toSeq.flatMap(valueToJson(input))))
  }
  private def valueToJson(input: Json)(pair: (String, Either[Json, Filter])): Option[(String, Json)] = {
    pair._2.fold(Option(_), _.transform(input)).map(v => (pair._1, v))
  }
}
