package org.fluminous.jq.tokens

trait BasicToken extends Token {
  val char: Char
  override def toString: String = char.toString
}
