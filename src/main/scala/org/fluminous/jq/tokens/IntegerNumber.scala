package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class IntegerNumber(override val position: Int, value: String) extends Token {
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if c.isDigit =>
        Right(Some(IntegerNumber(position, value :+ c)))
      case Character(c) if !c.isDigit && value == "-" =>
        Left(ParserException(symbolPosition, "Symbol - at invalid position"))
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case c @ Character('.') =>
        Right(Some(DecimalNumber(position, value :+ c.c)))
      case EOF =>
        Right(None)
      case _ =>
        Left(ParserException(symbolPosition, "Identifier could not start with number. Try to surround it by quotes"))

    }
  }
  def asInt: Int                   = value.toInt
  override def toString: String    = value
  override val description: String = toString
}

object IntegerNumber {
  implicit def typeDescription: Description[IntegerNumber] = new Description[IntegerNumber] {
    override val description: String = "integer number"
  }
}
