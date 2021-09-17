package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class Root(override val position: Int) extends BasicToken with Filter {
  override val char = Root.char
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF            => Right(None)
      case Character('.') => Right(Some(RecursiveDescent(position)))
      case _              => Right(None)
    }
  }

  override def transform(input: Json): Option[Json] = Option(input)
  override val description: String                  = Root.typeDescription.description
}

object Root {
  val char = '.'
  implicit def typeDescription: Description[Root] = new Description[Root] {
    override val description: String = char.toString
  }
}
