package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class OptionalSign(override val position: Int) extends AtomicToken {
  val char                         = OptionalSign.char
  override val description: String = OptionalSign.typeDescription.description

}
object OptionalSign {
  val char = '?'
  implicit def typeDescription: Description[OptionalSign] = new Description[OptionalSign] {
    override val description: String = char.toString
  }
}
