package org.fluminous.jq.filter.json.array

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class JsonArray(override val position: Int, values: Filter) extends Filter {
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    values.transform(input).map(a => List(Json.fromValues(a.filterNot(_.isNull))))
  }
  override val description: String = JsonArray.typeDescription.description
}

object JsonArray {
  implicit def typeDescription: Description[JsonArray] = new Description[JsonArray] {
    override val description: String = "end of json array"
  }
}
