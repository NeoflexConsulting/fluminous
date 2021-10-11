package org.fluminous.jq.filter.json.array

import cats.syntax.traverse._
import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class JsonArray(override val position: Int, values: List[Filter]) extends Filter {
  override def transformSingle(input: Json): Either[EvaluationException, Json] = {
    values.traverse(_.transformSingle(input)).map(a => Json.fromValues(a.filterNot(_.isNull)))
  }
  override val description: String = JsonArray.typeDescription.description
}

object JsonArray {
  implicit def typeDescription: Description[JsonArray] = new Description[JsonArray] {
    override val description: String = "end of json array"
  }
}
