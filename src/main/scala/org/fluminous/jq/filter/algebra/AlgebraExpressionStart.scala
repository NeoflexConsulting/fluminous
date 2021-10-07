package org.fluminous.jq.filter.algebra

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, Expression }
final case class AlgebraExpressionStart(
  override val position: Int,
  operationSign: OperationSign,
  left: Filter,
  right: Filter)
    extends Expression {

  def addFilter(termOperationSign: OperationSign, term: Filter): AlgebraExpressionStart = {
    if (this.operationSign.priority >= termOperationSign.priority) {
      AlgebraExpressionStart(position, termOperationSign, AlgebraExpression(position, operationSign, left, right), term)
    } else {
      val updatedRight = right match {
        case expr: AlgebraExpression => expr.addFilter(termOperationSign, term)
        case _                       => AlgebraExpression(right.position, termOperationSign, right, term)
      }
      AlgebraExpressionStart(position, operationSign, left, updatedRight)
    }
  }
  override val description: String = AlgebraExpressionStart.typeDescription.description
}

object AlgebraExpressionStart {

  implicit def typeDescription: Description[AlgebraExpressionStart] =
    new Description[AlgebraExpressionStart] {
      override val description: String = "algebra expression"
    }
}
