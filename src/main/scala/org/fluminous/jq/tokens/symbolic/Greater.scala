package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.input.Character
import org.fluminous.jq.tokens._
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import io.circe.syntax._

case class Greater(override val position: Int) extends Token with AlgebraOperation {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case Character(c) if c == '=' =>
        Right(TokenUpdated(GreaterOrEqual(position)))
      case _ =>
        Right(TokenConstructed)
    }
  }
  override def toString: String    = """>"""
  override val description: String = toString
  implicit def typeDescription: Description[Greater] = new Description[Greater] {
    override val description: String = toString
  }
  override val priority: Int = 0
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(greater(left, _))
  }
  private def greater(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal > rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}
