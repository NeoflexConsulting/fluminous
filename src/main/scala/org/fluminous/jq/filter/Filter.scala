package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException, Expression }

trait Filter extends Expression {
  val isSingleValued: Boolean
  def transform(input: Json): Either[EvaluationException, List[Json]]
  def transformToSingleJson(input: Json): Either[EvaluationException, Json] = {
    transform(input).flatMap(makeUnique)
  }
  private def makeUnique[T](list: List[T]): Either[EvaluationException, T] = {
    list match {
      case Nil         => Left(EvaluationException(position, s"Expected single evaluated value, but null found"))
      case head :: Nil => Right(head)
      case _           => Left(EvaluationException(position, s"Expected single evaluated value, but found ${list.size}"))
    }

  }
}

object Filter {
  implicit def typeDescription: Description[Filter] = new Description[Filter] {
    override val description: String = "filter"
  }
}
