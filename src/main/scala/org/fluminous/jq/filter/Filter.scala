package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Expression

trait Filter extends Expression {
  def transform(input: Json): Option[Json]
}
