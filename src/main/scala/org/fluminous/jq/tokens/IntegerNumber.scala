package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class IntegerNumber(value: String) extends Token {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if c.isDigit =>
        Right(Some(IntegerNumber(value :+ c)))
      case Character(c) if !c.isDigit && value == "-" =>
        Left(ParserException(position, "Symbol - at invalid position"))
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case c @ Character('.') =>
        Right(Some(DecimalNumber(value :+ c.c)))
      case EOF =>
        Right(None)
      case _ =>
        Left(ParserException(position, "Identifier could not start with number. Try to surround it by quotes"))

    }
  }
  def asInt: Int = value.toInt
}
