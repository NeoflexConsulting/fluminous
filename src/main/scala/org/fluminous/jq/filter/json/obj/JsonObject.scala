package org.fluminous.jq.filter.json.obj

import cats.syntax.traverse._
import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, EvaluationException}

case class JsonObject(override val position: Int, values: Map[String, Filter]) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    values.toList.traverse { case (n, v) => v.transform(input).map(j => (n, j)) }.map(Json.fromFields)
  }
  override val description: String = JsonObject.typeDescription.description
}

object JsonObject {
  implicit def typeDescription: Description[JsonObject] = new Description[JsonObject] {
    override val description: String = "end of json object"
  }
}
