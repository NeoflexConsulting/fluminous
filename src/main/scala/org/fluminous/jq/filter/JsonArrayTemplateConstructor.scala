package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Expression

final case class JsonArrayTemplateConstructor(values: Seq[Either[Json, Filter]]) extends Expression
