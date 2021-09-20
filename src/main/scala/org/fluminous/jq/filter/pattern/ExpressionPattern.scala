package org.fluminous.jq.filter.pattern

import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.{ Expression, FoldFunctions }
import cats.syntax.foldable._
import org.fluminous.jq.filter.pattern.dsl.{ MatchFailure, PositionedMatchFailure }

trait ExpressionPattern extends FoldFunctions {
  def instantiateOnStack(stack: NonEmptyList[Expression]): Validated[Option[PatternFailure], List[Expression]] = {
    firstValidOrAllInvalids(ExpressionCases.cases)(p => p.patternCase(stack).leftMap(f => (p, f)))
      .leftMap(getRelevant)
  }

  private def getRelevant(failures: List[(PatternCase, MatchFailure)]): Option[PatternFailure] = {
    failures.flatMap {
      case (patternCase, p @ PositionedMatchFailure(_, _, _, _, overallMismatchesQty))
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
    }.maximumByOption(p => (p.failurePosition, -p.mismatchQty))
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
