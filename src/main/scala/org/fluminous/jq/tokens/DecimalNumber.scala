package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class DecimalNumber(override val position: Int, value: String) extends Token {
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case Character(c) if c.isDigit =>
        Right(
          Some(DecimalNumber(position, value :+ c))
        )
      case EOF =>
        Right(None)
      case _ =>
        Left(ParserException(symbolPosition, "Identifier could not start with number. Try to surround it by quotes"))
    }
  }
  def asDecimal: BigDecimal        = BigDecimal(value)
  override def toString: String    = value
  override val description: String = toString
}

object DecimalNumber {
  implicit def typeDescription: Description[DecimalNumber] = new Description[DecimalNumber] {
    override val description: String = "decimal number"
  }
}
