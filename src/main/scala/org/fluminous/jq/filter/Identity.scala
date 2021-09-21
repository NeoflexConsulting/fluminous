package org.fluminous.jq.filter

import io.circe.Json

case object Identity extends Filter {
  override def transform(input: Json): Option[Json] = Some(input)
  override val position: Int                        = 0
  override val description: String                  = "identity"
}
