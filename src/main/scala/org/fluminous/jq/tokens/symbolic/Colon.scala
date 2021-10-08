package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class Colon(override val position: Int) extends AtomicToken {
  val char                         = Colon.char
  override val description: String = Colon.typeDescription.description

}
object Colon {
  val char = ':'
  implicit def typeDescription: Description[Colon] = new Description[Colon] {
    override val description: String = char.toString
  }
}
