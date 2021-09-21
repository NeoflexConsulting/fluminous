package org.fluminous.jq.filter.json

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, Expression }

final case class JsonObjectUncompleted(override val position: Int, values: Map[String, Filter])
    extends Expression {
  override val description: String = JsonObjectUncompleted.typeDescription.description
}

object JsonObjectUncompleted {
  implicit def typeDescription: Description[JsonObjectUncompleted] =
    new Description[JsonObjectUncompleted] {
      override val description: String = "json object"
    }
}