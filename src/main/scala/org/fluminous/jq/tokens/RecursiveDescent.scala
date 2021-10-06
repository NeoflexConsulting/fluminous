package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class RecursiveDescent(override val position: Int) extends Token {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || c == VerticalSlash.char =>
        Right(None)
      case EOF =>
        Right(None)
      case Character(c) =>
        Left(ParserException(position, s"""Invalid sequence "..$c""""))
    }
  }
  override def toString: String    = raw"""\\"""
  override val description: String = toString
  implicit def typeDescription: Description[RecursiveDescent] = new Description[RecursiveDescent] {
    override val description: String = toString
  }
}
