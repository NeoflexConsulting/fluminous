package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }
import org.fluminous.jq.tokens.symbolic.{ AtomicToken, Root }

case class Identifier(override val position: Int, override val value: String) extends Token with StringToken {
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case EOF =>
        Right(TokenConstructed)
      case Character(c) if Token.whitespaceSymbols.contains(c) || AtomicToken.symbols.contains(c) || c == Root.char =>
        Right(TokenConstructed)
      case Character(c) =>
        Right(TokenUpdated(Identifier(position, value :+ c)))
    }
  }
  override val description: String = value
}

object Identifier {
  implicit def typeDescription: Description[Identifier] = new Description[Identifier] {
    override val description: String = "identifier"
  }
}
