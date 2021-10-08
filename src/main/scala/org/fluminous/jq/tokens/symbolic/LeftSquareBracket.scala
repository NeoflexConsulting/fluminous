package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class LeftSquareBracket(override val position: Int) extends AtomicToken {
  val char                         = LeftSquareBracket.char
  override val description: String = LeftSquareBracket.typeDescription.description

}
object LeftSquareBracket {
  val char = '['
  implicit def typeDescription: Description[LeftSquareBracket] = new Description[LeftSquareBracket] {
    override val description: String = char.toString
  }
}
