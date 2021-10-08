package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.tokens.Token

trait SymbolToken extends Token {
  val char: Char
  override def toString: String = char.toString
}
