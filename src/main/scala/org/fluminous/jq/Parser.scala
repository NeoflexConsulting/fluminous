package org.fluminous.jq

import org.fluminous.jq.Expression

case class Parser(tokenizer: Tokenizer) {
  private val stack = List.empty[Expression]
}
