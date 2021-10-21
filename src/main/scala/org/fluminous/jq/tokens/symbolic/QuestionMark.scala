package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class QuestionMark(override val position: Int) extends AtomicToken {
  val char                         = QuestionMark.char
  override val description: String = QuestionMark.typeDescription.description
}

object QuestionMark {
  val char = '?'
  implicit def typeDescription: Description[QuestionMark] = new Description[QuestionMark] {
    override val description: String = char.toString
  }
}
