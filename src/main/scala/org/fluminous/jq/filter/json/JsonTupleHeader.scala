package org.fluminous.jq.filter.json

import org.fluminous.jq.{ Description, Expression }

final case class JsonTupleHeader(override val position: Int, fieldName: String) extends Expression {
  override val description: String = JsonTupleHeader.typeDescription.description
}

object JsonTupleHeader {
  implicit def typeDescription: Description[JsonTupleHeader] = new Description[JsonTupleHeader] {
    override val description: String = "name of json field"
  }
}
