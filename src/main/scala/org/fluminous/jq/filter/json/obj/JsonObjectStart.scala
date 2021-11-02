package org.fluminous.jq.filter.json.obj

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

import scala.collection.immutable.ListMap

final case class JsonObjectStart(override val position: Int, values: ListMap[Filter, Filter])
    extends Expression {
  override val description: String = JsonObjectStart.typeDescription.description
}

object JsonObjectStart {
  implicit def typeDescription: Description[JsonObjectStart] =
    new Description[JsonObjectStart] {
      override val description: String = "json object"
    }
}