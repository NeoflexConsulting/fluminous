package org.fluminous.jq.filter.json.tuple

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

final case class JsonTupleHeader(override val position: Int, fieldName: Filter) extends Expression {
  override val description: String = JsonTupleHeader.typeDescription.description
}

object JsonTupleHeader {
  implicit def typeDescription: Description[JsonTupleHeader] = new Description[JsonTupleHeader] {
    override val description: String = "name of json field"
  }
}
