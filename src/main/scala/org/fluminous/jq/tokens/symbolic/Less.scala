package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.input.Character
import org.fluminous.jq.tokens._
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import io.circe.syntax._

case class Less(override val position: Int) extends Token with AlgebraOperation {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case Character(c) if c == '=' =>
        Right(TokenUpdated(LessOrEqual(position)))
      case _ =>
        Right(TokenConstructed)
    }
  }
  override def toString: String    = """<"""
  override val description: String = toString
  implicit def typeDescription: Description[Less] = new Description[Less] {
    override val description: String = toString
  }
  override val priority: Int = 5
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(less(left, _))
  }
  private def less(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal < rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}
