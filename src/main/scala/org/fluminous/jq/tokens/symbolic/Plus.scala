package org.fluminous.jq.tokens.symbolic

import io.circe.{ Json, JsonObject }
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.algebra.AlgebraOperation
import cats.syntax.traverse._

case class Plus(override val position: Int) extends AtomicToken with AlgebraOperation {
  val char                         = Plus.char
  override val description: String = Plus.typeDescription.description
  override val priority: Int       = 3
  override def execute(
    left: Json,
    isRightSingleValued: Boolean,
    right: => Either[EvaluationException, List[Json]]
  ): Either[EvaluationException, List[Json]] = {
    right.flatMap(_.map(add(left, _)).sequence)
  }

  private def add(left: Json, right: Json): Either[EvaluationException, Json] = {
    addNumbers(left, right)
      .orElse(addArrays(left, right))
      .orElse(addStrings(left, right))
      .orElse(addObjects(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }

  private def addNumbers(left: Json, right: Json): Option[Json] = {
    for {
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
    } yield Json.fromBigDecimal(leftBigDecimal + rightBigDecimal)
  }

  private def addArrays(left: Json, right: Json): Option[Json] = {
    for {
      leftArray  <- left.asArray
      rightArray <- right.asArray
    } yield Json.fromValues(leftArray ++ rightArray)
  }
  private def addStrings(left: Json, right: Json): Option[Json] = {
    for {
      leftString  <- left.asString
      rightString <- right.asString
    } yield Json.fromString(leftString ++ rightString)
  }
  private def addObjects(left: Json, right: Json): Option[Json] = {
    for {
      leftObject  <- left.asObject
      rightObject <- right.asObject
    } yield Json.fromJsonObject(mergeObjects(rightObject, leftObject))
  }

  private def mergeObjects(fromObject: JsonObject, toObject: JsonObject): JsonObject =
    fromObject.toIterable.foldLeft(toObject) {
      case (to, (fromKey, fromValue)) =>
        toObject(fromKey).fold(to.add(fromKey, fromValue)) { _ =>
          to.add(fromKey, fromValue)
        }
    }
}
object Plus {
  val char = '+'
  implicit def typeDescription: Description[Plus] = new Description[Plus] {
    override val description: String = char.toString
  }
}
