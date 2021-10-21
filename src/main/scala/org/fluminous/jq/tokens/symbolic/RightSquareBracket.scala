package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class RightSquareBracket(override val position: Int) extends AtomicToken {
  val char                         = RightSquareBracket.char
  override val description: String = RightSquareBracket.typeDescription.description

}
object RightSquareBracket {
  val char = ']'
  implicit def typeDescription: Description[RightSquareBracket] = new Description[RightSquareBracket] {
    override val description: String = char.toString
  }
}
