package org.fluminous.jq.filter.pattern
case class PatternFailure(
  patternName: String,
  startPosition: Int,
  failurePosition: Int,
  failedExpression: String,
  mismatchQty: Int) {
  def formatError: String = {
    s"Error occurred while parsing $patternName at position $failurePosition. Unexpected $failedExpression."
  }
}
