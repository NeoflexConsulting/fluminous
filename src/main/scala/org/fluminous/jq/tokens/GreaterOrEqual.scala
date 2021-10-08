package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.{Description, EvaluationException, ParserException, input}
import io.circe.syntax._

case class GreaterOrEqual(override val position: Int) extends Token with AlgebraOperation{
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    Right(TokenConstructed)
  }
  override def toString: String    = """>="""
  override val description: String = toString
  implicit def typeDescription: Description[GreaterOrEqual] = new Description[GreaterOrEqual] {
    override val description: String = toString
  }

  override val priority: Int       = 0
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(greaterOrEqual(left, _))
  }
  private def greaterOrEqual(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal >= rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}
