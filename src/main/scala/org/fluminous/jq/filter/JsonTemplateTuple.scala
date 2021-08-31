package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Expression

final case class JsonTemplateTuple(fieldName: String, fieldValue: Either[Json, Filter]) extends Expression
