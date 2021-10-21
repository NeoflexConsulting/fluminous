package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class LeftBracket(override val position: Int) extends AtomicToken {
  val char                         = LeftBracket.char
  override val description: String = LeftBracket.typeDescription.description

}
object LeftBracket {
  val char = '('
  implicit def typeDescription: Description[LeftBracket] = new Description[LeftBracket] {
    override val description: String = char.toString
  }
}
