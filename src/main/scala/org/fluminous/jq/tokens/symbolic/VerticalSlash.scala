package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class VerticalSlash(override val position: Int) extends AtomicToken {
  val char                         = VerticalSlash.char
  override val description: String = VerticalSlash.typeDescription.description
}
object VerticalSlash {
  val char = '|'
  implicit def typeDescription: Description[VerticalSlash] = new Description[VerticalSlash] {
    override val description: String = char.toString
  }
}
