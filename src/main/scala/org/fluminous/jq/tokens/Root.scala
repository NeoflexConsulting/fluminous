package org.fluminous.jq.tokens

import org.fluminous.jq.{ParserException, input}
import org.fluminous.jq.input.{Character, EOF}

case object Root extends BasicToken {
  override val char = '.'
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF            => Right(None)
      case Character('.') => Right(Some(RecursiveDescent))
      case _              => Right(None)
    }
  }
}
