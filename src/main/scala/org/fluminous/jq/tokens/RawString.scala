package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class RawString(override val position: Int, value: String, finished: Boolean = true) extends Token with Filter {
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, AppendResult] = {
    if (finished) {
      Right(TokenConstructed)
    } else {
      symbol match {
        case Character('"') => Right(TokenUpdated(RawString(position, value)))
        case Character(c)   => Right(TokenUpdated(RawString(position, value :+ c, false)))
        case EOF            => Left(ParserException(symbolPosition, s"String $value doesn't end with quote"))
      }
    }
  }
  override def toString: String    = s""""${value}""""
  override val description: String = toString

  override def transform(input: Json): Either[EvaluationException, Json] = Right(Json.fromString(value))
}

object RawString {
  implicit def typeDescription: Description[RawString] = new Description[RawString] {
    override val description: String = "quoted string"
  }
}
