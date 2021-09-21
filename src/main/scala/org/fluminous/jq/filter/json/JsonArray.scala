package org.fluminous.jq.filter.json

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import io.circe.syntax._
import cats.syntax.traverse._

final case class JsonArray(override val position: Int, values: List[Filter]) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    values.traverse(_.transform(input)).map(_.asJson)
  }
  override val description: String = JsonArray.typeDescription.description
}

object JsonArray {
  implicit def typeDescription: Description[JsonArray] = new Description[JsonArray] {
    override val description: String = "end of json array"
  }
}
