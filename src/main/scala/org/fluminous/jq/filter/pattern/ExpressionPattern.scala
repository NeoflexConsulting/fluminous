package org.fluminous.jq.filter.pattern

import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.{ Expression, FoldFunctions }
import org.fluminous.jq.filter.pattern.dsl.{ MatchFailure, PositionedMatchFailure }

trait ExpressionPattern extends FoldFunctions {
  def instantiateOnStack(stack: NonEmptyList[Expression]): Validated[List[PatternFailure], List[Expression]] = {
    firstValidOrAllInvalids(ExpressionCases.cases)(p => p.patternCase(stack).leftMap(f => (p, f)))
      .leftMap(getPatternFailure)
  }

  private def getPatternFailure(failures: List[(PatternCase, MatchFailure)]): List[PatternFailure] = {
    failures.flatMap {
      case (patternCase, p @ PositionedMatchFailure(_, _, _, overallMismatchesQty))
          if overallMismatchesQty < patternCase.length =>
        Some(
          PatternFailure(
            ExpressionCases.name,
            p.patternStartPosition,
            p.failurePosition,
            p.actualExpression,
            p.overallMismatchesQty
          )
        )
      case _ =>
        None
    }
  }
  protected val ExpressionCases: PatternCases
}

object ExpressionPattern {
  val patterns: List[ExpressionPattern] = List(
    SelectorPattern,
    JsonTupleHeaderPattern,
    JsonObjectTemplatePattern,
    JsonObjectTemplateConstructorPattern,
    JsonArrayTemplatePattern,
    JsonArrayTemplateConstructorPattern
  )
}
