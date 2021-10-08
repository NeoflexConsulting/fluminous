package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class Identifier(override val position: Int, value: String) extends Token {
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case EOF =>
        Right(TokenConstructed)
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) || c == Root.char =>
        Right(TokenConstructed)
      case Character(c) =>
        Right(TokenUpdated(Identifier(position, value :+ c)))
    }
  }
  override def toString: String    = value
  override val description: String = toString
}

object Identifier {
  implicit def typeDescription: Description[Identifier] = new Description[Identifier] {
    override val description: String = "identifier"
  }
}
