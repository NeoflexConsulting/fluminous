package org.fluminous.jq.filter.pattern

import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.{ Description, Expression }
import org.fluminous.jq.filter.pattern.dsl.MatchFailure

case class PatternCases(name: String, cases: List[PatternCase])

case class PatternCase(length: Int, patternCase: NonEmptyList[Expression] => Validated[MatchFailure, List[Expression]])

object PatternCases {
  def apply[T <: Expression: Description](patternCases: PatternCase*): PatternCases =
    PatternCases(implicitly[Description[T]].description, patternCases.toList)
}
