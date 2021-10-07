package org.fluminous.jq.filter.algebra

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class AlgebraExpression(
  override val position: Int,
  operationSign: OperationSign,
  left: Filter,
  right: Filter)
    extends Filter {

  def addFilter(termOperationSign: OperationSign, term: Filter): AlgebraExpression = {
    if (this.operationSign.priority >= termOperationSign.priority) {
      AlgebraExpression(position, termOperationSign, AlgebraExpression(position, operationSign, left, right), term)
    } else {
      val updatedRight = right match {
        case expr: AlgebraExpression => expr.addFilter(termOperationSign, term)
        case _                       => AlgebraExpression(right.position, termOperationSign, right, term)
      }
      AlgebraExpression(position, operationSign, left, updatedRight)
    }
  }

  override val description: String = AlgebraExpression.typeDescription.description

  override def transform(input: Json): Either[EvaluationException, Json] = {
    for {
      leftResult <- left.transform(input)
      result     <- operationSign.execute(leftResult, right.transform(input))
    } yield result
  }
}

object AlgebraExpression {

  implicit def typeDescription: Description[AlgebraExpression] =
    new Description[AlgebraExpression] {
      override val description: String = "algebra expression"
    }
}
