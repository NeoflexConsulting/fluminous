package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ input, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case object Root extends BasicToken with Filter {
  override val char = '.'
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF            => Right(None)
      case Character('.') => Right(Some(RecursiveDescent))
      case _              => Right(None)
    }
  }

  override def transform(input: Json): Option[Json] = Option(input)
}
