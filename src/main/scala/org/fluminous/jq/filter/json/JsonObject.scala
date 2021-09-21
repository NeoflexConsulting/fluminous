package org.fluminous.jq.filter.json

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }
import io.circe.syntax._
import cats.syntax.traverse._
import cats.syntax.option._

case class JsonObject(override val position: Int, values: Map[String, Filter]) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    values.toList.traverse { case (n, v) => v.transform(input).map(j => (n, j)) }.map(_.asJson)
  }
  override val description: String = JsonObject.typeDescription.description
}

object JsonObject {
  implicit def typeDescription: Description[JsonObject] = new Description[JsonObject] {
    override val description: String = "end of json object"
  }
}
