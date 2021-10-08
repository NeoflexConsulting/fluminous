package org.fluminous.jq.tokens
import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import org.fluminous.jq.input.{ Character, EOF }
import io.circe.syntax._

case class NotEqual(override val position: Int, isCompleted: Boolean = false) extends Token with AlgebraOperation {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    if (isCompleted) {
      Right(TokenConstructed)
    } else {
      symbol match {
        case Character(c) if c == '=' => Right(TokenUpdated(this.copy(isCompleted = true)))
        case EOF =>
          Left(ParserException(position, s"""Invalid symbol '!'"""))
      }
    }
  }
  override def toString: String    = """!="""
  override val description: String = toString
  implicit def typeDescription: Description[NotEqual] = new Description[NotEqual] {
    override val description: String = toString
  }

  override val priority: Int = 5
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.map(notEqual(left, _))
  }
  private def notEqual(left: Json, right: Json): Json = {
    (left != right).asJson
  }
}
