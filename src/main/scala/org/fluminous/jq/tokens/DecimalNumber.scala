package org.fluminous.jq.tokens

import org.fluminous.jq.{ParserException, input}
import org.fluminous.jq.input.{Character, EOF}

case class DecimalNumber(value: String) extends Token {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case Character(c) if c.isDigit =>
        Right(
          Some(DecimalNumber(value :+ c))
        )
      case EOF =>
        Right(None)
      case _ =>
        Left(ParserException(position, "Identifier could not start with number. Try to surround it by quotes"))
    }
  }
  def toDecimal: BigDecimal = BigDecimal(value)
}
