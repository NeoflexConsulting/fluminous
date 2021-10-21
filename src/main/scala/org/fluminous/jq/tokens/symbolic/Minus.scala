package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.{ Description, EvaluationException }
import cats.syntax.traverse._

case class Minus(override val position: Int) extends AtomicToken with AlgebraOperation {
  val char                         = Minus.char
  override val description: String = Minus.typeDescription.description
  override val priority: Int       = 3
  override def execute(
    left: Json,
    isRightSingleValued: Boolean,
    right: => Either[EvaluationException, List[Json]]
  ): Either[EvaluationException, List[Json]] = {
    right.flatMap(_.map(subtract(left, _)).sequence)
  }
  private def subtract(left: Json, right: Json): Either[EvaluationException, Json] = {
    subtractNumbers(left, right)
      .orElse(subtractArrays(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
  private def subtractNumbers(left: Json, right: Json): Option[Json] = {
    for {
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
    } yield Json.fromBigDecimal(leftBigDecimal - rightBigDecimal)
  }
  private def subtractArrays(left: Json, right: Json): Option[Json] = {
    for {
      leftArray  <- left.asArray
      rightArray <- right.asArray
    } yield Json.fromValues(leftArray.filterNot(rightArray.toSet.contains))
  }
}
object Minus {
  val char = '-'
  implicit def typeDescription: Description[Minus] = new Description[Minus] {
    override val description: String = char.toString
  }
}
