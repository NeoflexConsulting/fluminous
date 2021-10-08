package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class RightBracket(override val position: Int) extends AtomicToken {
  val char                         = RightBracket.char
  override val description: String = RightBracket.typeDescription.description

}
object RightBracket {
  val char = ')'
  implicit def typeDescription: Description[RightBracket] = new Description[RightBracket] {
    override val description: String = char.toString
  }
}
