package org.fluminous.jq.filter.json.array

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

final case class JsonArrayStart(override val position: Int, values: List[Filter])
    extends Expression {
  override val description: String = JsonArrayStart.typeDescription.description
}

object JsonArrayStart {
  implicit def typeDescription: Description[JsonArrayStart] =
    new Description[JsonArrayStart] {
      override val description: String = "json array"
    }
}
