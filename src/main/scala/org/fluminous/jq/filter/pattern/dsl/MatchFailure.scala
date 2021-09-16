package org.fluminous.jq.filter.pattern.dsl

case class MatchFailure(
  firstMismatchPositionFromEnd: Int,
  actualExpression: String,
  expectedExpression: String,
  overallMismatchesQty: MismatchesQty)

object MatchFailure {
  def StackIsNotEnough(position: Int): MatchFailure = MatchFailure(position, "", "", CompleteMismatch)
}
