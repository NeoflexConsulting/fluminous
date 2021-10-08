package org.fluminous.jq.tokens.symbolic

import org.fluminous.jq.input.{Character, EOF}
import org.fluminous.jq.tokens.{AppendResult, RecursiveDescent, TokenConstructed, TokenUpdated}
import org.fluminous.jq.{Description, ParserException, input}

case class Root(override val position: Int) extends SymbolToken {
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
