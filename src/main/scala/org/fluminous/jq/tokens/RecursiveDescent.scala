package org.fluminous.jq.tokens

import org.fluminous.jq.{Description, ParserException, input}
import org.fluminous.jq.input.{Character, EOF}
import org.fluminous.jq.tokens.symbolic.VerticalSlash

case class RecursiveDescent(override val position: Int) extends Token {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || c == VerticalSlash.char =>
        Right(TokenConstructed)
      case EOF =>
        Right(TokenConstructed)
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
