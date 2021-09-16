package org.fluminous.jq.filter.pattern

import cats.Order
import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.Expression
import cats.syntax.foldable._
import org.fluminous.jq.filter.pattern.dsl.{ MatchFailure, MismatchesQty }
trait ExpressionPattern {
  def instantiateOnStack(stack: NonEmptyList[Expression]): Validated[PatternFailure, List[Expression]] = {
    ExpressionCases.cases.foldMapA(p => p.patternCase(stack).leftMap(f => List((p.length, f)))).leftMap(filterRelevant)
  }

  private def filterRelevant(failures: List[(Int, MatchFailure)]): PatternFailure = {
    val relevantFailures = failures.minimumList
    val position         = relevantFailures.map(positionFromStart).headOption.getOrElse(0)
    val relevantPatternCaseFailures = relevantFailures.map {
      case (_, matchFailure) => PatternCaseFailure(matchFailure.actualExpression, matchFailure.expectedExpression)
    }
    PatternFailure(ExpressionCases.name, position, relevantPatternCaseFailures)
  }

  implicit def positionFromStartAndMismatches: Order[(Int, MatchFailure)] = new Order[(Int, MatchFailure)] {
    override def compare(x: (Int, MatchFailure), y: (Int, MatchFailure)): Int = {
      val positionResult = implicitly[Order[Int]].compare(positionFromStart(x), positionFromStart(y))
      if (positionResult != 0)
        positionResult
      else
        implicitly[Order[MismatchesQty]].compare(x._2.overallMismatchesQty, y._2.overallMismatchesQty)
    }
  }

  private def positionFromStart(failure: (Int, MatchFailure)): Int = {
    failure._1 - failure._2.firstMismatchPositionFromEnd + 1
  }

  protected val ExpressionCases: PatternCases
}

object ExpressionPattern {
  val patterns = List(
    SelectorPattern,
    JsonTupleHeaderPattern,
    JsonObjectTemplatePattern,
    JsonObjectTemplateConstructorPattern,
    JsonArrayTemplatePattern,
    JsonArrayTemplateConstructorPattern
  )
}
