package org.fluminous.jq.filter.pattern

import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.json.obj.{ JsonObjectPattern, JsonObjectUncompletedPattern }
import org.fluminous.jq.filter.json.array.{ JsonArrayPattern, JsonArrayUncompletedPattern }
import org.fluminous.jq.filter.json.tuple.JsonTupleHeaderPattern
import org.fluminous.jq.{ Expression, FoldFunctions, Tokenizer }
import org.fluminous.jq.filter.pattern.dsl.{ MatchFailure, PositionedMatchFailure }
//import org.fluminous.jq.filter.selector.SelectorPattern

trait ExpressionPattern extends FoldFunctions {

  def instantiateOnStack(
    tokenizer: Tokenizer,
    stack: NonEmptyList[Expression]
  ): (Tokenizer, Validated[List[PatternFailure], List[Expression]]) = {
    val (finalTokenizer, res) = firstValidOrAllInvalids(ExpressionCases.cases, tokenizer) {
      case (p, nextTokenizer) =>
        val res = p.patternCase(MatcherInput(nextTokenizer, stack))
        (res.tokenizer, res.result.leftMap(f => (p, f)))
    }
    (finalTokenizer, res.leftMap(getPatternFailure))
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
    //SelectorPattern,
    JsonTupleHeaderPattern,
    JsonObjectPattern,
    JsonObjectUncompletedPattern,
    JsonArrayPattern,
    JsonArrayUncompletedPattern,
    Algebra,
    BooleanAlgebra
  )
}
