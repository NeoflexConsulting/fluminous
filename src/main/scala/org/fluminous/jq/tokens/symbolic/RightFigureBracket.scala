package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.Description

case class RightFigureBracket(override val position: Int) extends AtomicToken {
  val char                         = RightFigureBracket.char
  override val description: String = RightFigureBracket.typeDescription.description

}
object RightFigureBracket {
  val char = '}'
  implicit def typeDescription: Description[RightFigureBracket] = new Description[RightFigureBracket] {
    override val description: String = char.toString
  }
}
