package org.fluminous.jq.filter.pattern.dsl

case class MatchFailure(
  position: Int,
  actualExpression: String,
  expectedExpression: String,
  overallMismatchesQty: MismatchesQty)

object MatchFailure {
  def StackIsNotEnough: MatchFailure = MatchFailure(0, "", "", CompleteMismatch)
}
