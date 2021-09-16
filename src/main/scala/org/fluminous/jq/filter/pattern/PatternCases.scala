package org.fluminous.jq.filter.pattern

import cats.data.NonEmptyList
import org.fluminous.jq.{ Description, Expression }
import org.fluminous.jq.filter.pattern.dsl.InconsistencyMeasure

case class PatternCases(
  name: String,
  cases: List[NonEmptyList[Expression] => Either[InconsistencyMeasure, List[Expression]]])

object PatternCases {
  def apply[T <: Expression: Description](
    patternCases: NonEmptyList[Expression] => Either[InconsistencyMeasure, List[Expression]]*
  ): PatternCases = PatternCases(implicitly[Description[T]].description, patternCases.toList)
}
