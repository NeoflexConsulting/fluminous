package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case object RecursiveDescent extends Token {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || c == Pipe.char =>
        Right(None)
      case EOF =>
        Right(None)
      case Character(c) =>
        Left(ParserException(position, s"""Invalid sequence "..$c""""))
    }
  }
  override def toString: String = raw"""\\"""
}
