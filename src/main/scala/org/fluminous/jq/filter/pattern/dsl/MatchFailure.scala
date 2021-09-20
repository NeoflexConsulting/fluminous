package org.fluminous.jq.filter.pattern.dsl

trait MatchFailure

case class PositionedMatchFailure(
  patternStartPosition: Int,
  failurePosition: Int,
  actualExpression: String,
  expectedExpression: String,
  overallMismatchesQty: Int)
    extends MatchFailure

case object StackIsNotEnough extends MatchFailure
