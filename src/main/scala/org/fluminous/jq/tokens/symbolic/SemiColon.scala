package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class SemiColon(override val position: Int) extends AtomicToken {
  val char                         = SemiColon.char
  override val description: String = SemiColon.typeDescription.description
}

object SemiColon {
  val char = ';'
  implicit def typeDescription: Description[SemiColon] = new Description[SemiColon] {
    override val description: String = char.toString
  }
}
