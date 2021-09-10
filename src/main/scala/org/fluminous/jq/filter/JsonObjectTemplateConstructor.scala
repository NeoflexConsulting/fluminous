package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Expression

final case class JsonObjectTemplateConstructor(values: Map[String, Either[Json, Filter]]) extends Expression
