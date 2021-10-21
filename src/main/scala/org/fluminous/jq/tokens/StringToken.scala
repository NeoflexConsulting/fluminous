package org.fluminous.jq.tokens

import org.fluminous.jq.Expression

trait StringToken extends Expression {
  val value: String
}
