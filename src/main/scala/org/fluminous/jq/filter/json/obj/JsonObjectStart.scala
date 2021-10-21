package org.fluminous.jq.filter.json.obj

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

final case class JsonObjectStart(override val position: Int, values: Map[Filter, Filter])
    extends Expression {
  override val description: String = JsonObjectStart.typeDescription.description
}

object JsonObjectStart {
  implicit def typeDescription: Description[JsonObjectStart] =
    new Description[JsonObjectStart] {
      override val description: String = "json object"
    }
}