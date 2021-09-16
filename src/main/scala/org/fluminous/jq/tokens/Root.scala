package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, ParserException, input}
import org.fluminous.jq.input.{Character, EOF}

trait Root extends BasicToken with Filter

case object Root extends Root {
  override val char = '.'
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF            => Right(None)
      case Character('.') => Right(Some(RecursiveDescent))
      case _              => Right(None)
    }
  }

  override def transform(input: Json): Option[Json] = Option(input)

  override val description: String = typeDescription.description
  implicit def typeDescription: Description[Root] = new Description[Root] {
    override val description: String = char.toString
  }
}
