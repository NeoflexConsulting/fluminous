package org.fluminous.jq.tokens

import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import org.fluminous.jq.input.{ Character, EOF }
import io.circe.syntax._

case class Equal(override val position: Int, isCompleted: Boolean = false) extends Token with AlgebraOperation {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    if (isCompleted) {
      Right(TokenConstructed)
    } else {
      symbol match {
        case Character(c) if c == '=' => Right(TokenUpdated(this.copy(isCompleted = true)))
        case EOF =>
          Left(ParserException(position, s"""Invalid symbol '='"""))
      }
    }
  }
  override def toString: String    = """=="""
  override val description: String = toString
  implicit def typeDescription: Description[Equal] = new Description[Equal] {
    override val description: String = toString
  }

  override val priority: Int = 5
  override def execute(left: Json,  isRightSingleValued: Boolean, right: => Either[EvaluationException, List[Json]]): Either[EvaluationException, List[Json]] = {
    right.map(_.map(isEqual(left, _)))
  }
  private def isEqual(left: Json, right: Json): Json = {
    (left == right).asJson
  }
}
