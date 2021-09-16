package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }
import org.fluminous.jq.input.{ Character, EOF }

case class RawString(value: String, finished: Boolean = true) extends Token {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    if (finished) {
      Right(None)
    } else {
      symbol match {
        case Character('"') => Right(Some(RawString(value)))
        case Character(c)   => Right(Some(RawString(value :+ c, false)))
        case EOF            => Left(ParserException(position, s"String $value doesn't end with quote"))
      }
    }
  }
  override def toString: String    = s""""$value""""
  override val description: String = toString
}

object RawString {
  implicit def typeDescription: Description[RawString] = new Description[RawString] {
    override val description: String = "quoted string"
  }
}
