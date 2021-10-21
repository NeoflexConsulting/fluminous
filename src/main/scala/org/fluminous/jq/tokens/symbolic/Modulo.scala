package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.algebra.AlgebraOperation
import cats.syntax.traverse._

case class Modulo(override val position: Int) extends AtomicToken with AlgebraOperation {
  val char                         = Modulo.char
  override val description: String = Modulo.typeDescription.description
  override val priority: Int       = 4

  override def execute(
    left: Json,
    isRightSingleValued: Boolean,
    right: => Either[EvaluationException, List[Json]]
  ): Either[EvaluationException, List[Json]] = {
    right.flatMap(_.map(moduloNumbers(left, _)).sequence)
  }

  private def moduloNumbers(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber  <- asNumber(left)
      leftInt     <- leftNumber.toInt
      rightNumber <- asNumber(right)
      rightInt    <- rightNumber.toInt
    } yield {
      if (rightInt == 0)
        Left(EvaluationException(position, "Modulo by zero"))
      else
        Right(Json.fromBigDecimal(leftInt % rightInt))
    }).sequence.flatMap(
      _.toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
    )
  }
}
object Modulo {
  val char = '%'
  implicit def typeDescription: Description[Modulo] = new Description[Modulo] {
    override val description: String = char.toString
  }
}
