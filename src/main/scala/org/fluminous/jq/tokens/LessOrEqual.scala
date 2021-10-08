package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import io.circe.syntax._

case class LessOrEqual(override val position: Int) extends Token with AlgebraOperation {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    Right(TokenConstructed)
  }
  override def toString: String    = """<="""
  override val description: String = toString
  implicit def typeDescription: Description[LessOrEqual] = new Description[LessOrEqual] {
    override val description: String = toString
  }
  override val priority: Int = 5
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(lessOrEqual(left, _))
  }
  private def lessOrEqual(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal <= rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}
