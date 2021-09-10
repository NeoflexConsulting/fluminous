package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Expression

final case class JsonTupleHeader(fieldName: String) extends Expression
