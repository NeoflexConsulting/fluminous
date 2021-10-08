package org.fluminous.jq.tokens.symbolic

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.algebra.AlgebraOperation
import cats.syntax.traverse._

case class Modulo(override val position: Int) extends AtomicToken with AlgebraOperation {
  val char                         = Modulo.char
  override val description: String = Modulo.typeDescription.description
  override val priority: Int       = 4

  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(moduloNumbers(left, _))
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
