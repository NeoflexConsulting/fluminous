package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import org.fluminous.jq.input.{ Character, EOF }
import org.fluminous.jq.tokens.symbolic.AtomicToken

case class NaturalNumber(override val position: Int, value: String) extends Token with Filter {
  override val isSingleValued: Boolean = true
  def tryAppend(symbol: input.Symbol, symbolPosition: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case Character(c) if c.isDigit =>
        Right(TokenUpdated(NaturalNumber(position, value :+ c)))
      case Character(c) if Token.whitespaceSymbols.contains(c) || AtomicToken.symbols.contains(c) =>
        Right(TokenConstructed)
      case c @ Character('.') =>
        Right(TokenUpdated(DecimalNumber(position, value :+ c.c)))
      case EOF =>
        Right(TokenConstructed)
      case _ =>
        Left(ParserException(symbolPosition, "Identifier could not start with number. Try to surround it by quotes"))

    }
  }
  override val description: String = toString

  def intValue: Int = value.toInt

  override def transform(input: Json): Either[EvaluationException, List[Json]] = Right(List(Json.fromInt(intValue)))
}

object NaturalNumber {
  implicit def typeDescription: Description[NaturalNumber] = new Description[NaturalNumber] {
    override val description: String = "natural number"
  }
}
