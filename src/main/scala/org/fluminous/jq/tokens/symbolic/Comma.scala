package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class Comma(override val position: Int) extends AtomicToken {
  val char                         = Comma.char
  override val description: String = Comma.typeDescription.description

}
object Comma {
  val char = ','
  implicit def typeDescription: Description[Comma] = new Description[Comma] {
    override val description: String = char.toString
  }
}
