package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class Identifier(override val position: Int, value: String) extends Token with Filter {
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF =>
        Right(None)
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) || c == Root.char =>
        Right(None)
      case Character(c) =>
        Right(Some(Identifier(position, value :+ c)))
    }
  }
  override def toString: String    = value
  override val description: String = toString

  override def transform(input: Json): Option[Json] = Some(Json.fromString(value))
}

object Identifier {
  implicit def typeDescription: Description[Identifier] = new Description[Identifier] {
    override val description: String = "identifier"
  }
}
