package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class Root(override val position: Int) extends BasicToken {
  override val char = Root.char
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case EOF            => Right(TokenConstructed)
      case Character('.') => Right(TokenUpdated(RecursiveDescent(position)))
      case _              => Right(TokenConstructed)
    }
  }
  override val description: String = Root.typeDescription.description
}

object Root {
  val char = '.'
  implicit def typeDescription: Description[Root] = new Description[Root] {
    override val description: String = char.toString
  }
}
