package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.algebra.AlgebraOperation
import cats.syntax.traverse._

import java.util.regex.Pattern

case class Divide(override val position: Int) extends AtomicToken with AlgebraOperation {
  val char                         = Divide.char
  override val description: String = Divide.typeDescription.description
  override val priority: Int       = 4
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(divide(left, _))
  }
  private def divide(left: Json, right: Json): Either[EvaluationException, Json] = {
    divideNumbers(left, right).flatMap {
      _.orElse(divideStrings(left, right))
        .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
    }
  }
  private def divideNumbers(left: Json, right: Json): Either[EvaluationException, Option[Json]] = {
    (for {
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
    } yield {
      if (rightBigDecimal == 0)
        Left(EvaluationException(position, "Division by zero"))
      else
        Right(Json.fromBigDecimal(leftBigDecimal / rightBigDecimal))
    }).sequence
  }

  private def divideStrings(left: Json, right: Json): Option[Json] = {
    for {
      leftString  <- left.asString
      rightString <- right.asString
    } yield Json.fromValues(leftString.split(Pattern.quote(rightString)).map(Json.fromString))
  }
}
object Divide {
  val char = '/'
  implicit def typeDescription: Description[Divide] = new Description[Divide] {
    override val description: String = char.toString
  }
}
