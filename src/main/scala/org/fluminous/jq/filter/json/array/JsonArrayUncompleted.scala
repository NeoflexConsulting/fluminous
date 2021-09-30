package org.fluminous.jq.filter.json.array

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

final case class JsonArrayUncompleted(override val position: Int, values: List[Filter])
    extends Expression {
  override val description: String = JsonArrayUncompleted.typeDescription.description
}

object JsonArrayUncompleted {
  implicit def typeDescription: Description[JsonArrayUncompleted] =
    new Description[JsonArrayUncompleted] {
      override val description: String = "json array"
    }
}