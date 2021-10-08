package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class LeftFigureBracket(override val position: Int) extends AtomicToken {
  val char                         = LeftFigureBracket.char
  override val description: String = LeftFigureBracket.typeDescription.description

}
object LeftFigureBracket {
  val char = '{'
  implicit def typeDescription: Description[LeftFigureBracket] = new Description[LeftFigureBracket] {
    override val description: String = char.toString
  }
}
