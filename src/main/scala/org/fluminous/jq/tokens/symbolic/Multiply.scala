package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.algebra.AlgebraOperation
import cats.syntax.traverse._

case class Multiply(override val position: Int) extends AtomicToken with AlgebraOperation {
  val char                         = Multiply.char
  override val description: String = Multiply.typeDescription.description
  override val priority: Int       = 4
  override def execute(
    left: Json,
    isRightSingleValued: Boolean,
    right: => Either[EvaluationException, List[Json]]
  ): Either[EvaluationException, List[Json]] = {
    right.flatMap(_.map(multiply(left, _)).sequence)
  }

  private def multiply(left: Json, right: Json): Either[EvaluationException, Json] = {
    multiplyNumbers(left, right)
      .orElse(multiplyStringByNumber(left, right))
      .orElse(multiplyObjects(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }

  private def multiplyStringByNumber(left: Json, right: Json): Option[Json] = {
    for {
      rightNumber <- asNumber(right)
      rightInt    <- rightNumber.toInt
      leftString  <- left.asString
    } yield if (rightInt == 0) Null else Json.fromString(Array.fill(rightInt)(leftString).mkString)
  }

  private def multiplyNumbers(left: Json, right: Json): Option[Json] = {
    for {
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
    } yield Json.fromBigDecimal(leftBigDecimal * rightBigDecimal)
  }
  private def multiplyObjects(left: Json, right: Json): Option[Json] = {
    for {
      rightObject <- right.asObject
      leftObject  <- left.asObject
    } yield Json.fromJsonObject(leftObject.deepMerge(rightObject))
  }
}

object Multiply {
  val char = '*'
  implicit def typeDescription: Description[Multiply] = new Description[Multiply] {
    override val description: String = char.toString
  }
}
