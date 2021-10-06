package org.fluminous.jq.filter.pattern

import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.json.obj.JsonObjectPattern
import org.fluminous.jq.filter.json.array.JsonArrayPattern
import org.fluminous.jq.filter.json.tuple.JsonTupleHeaderPattern
import org.fluminous.jq.{ Expression, FoldFunctions, ParserException, Tokenizer }
import org.fluminous.jq.filter.pattern.dsl.{ MatchFailure, PositionedMatchFailure }
import org.fluminous.jq.filter.pipe.PipePattern
import org.fluminous.jq.filter.selector.SelectorPattern
//import org.fluminous.jq.filter.selector.SelectorPattern

trait ExpressionPattern extends FoldFunctions {

  def instantiateOnStack(
    tokenizer: Tokenizer,
    stack: NonEmptyList[Expression]
  ): Either[ParserException, (Tokenizer, Validated[List[PatternFailure], List[Expression]])] = {
    for {
      (finalTokenizer, res) <- firstValidOrAllInvalidsWithEither(ExpressionCases.cases, tokenizer) {
                                case (p, nextTokenizer) =>
                                  p.patternCase(MatcherInput(nextTokenizer, stack))
                                    .map(output => (output.tokenizer, output.result.leftMap(f => (p, f))))
                              }
    } yield (finalTokenizer, res.leftMap(getPatternFailure))
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
    PipePattern,
    JsonTupleHeaderPattern,
    JsonObjectPattern,
    JsonArrayPattern,
    Algebra,
    BooleanAlgebra
  )
}
