package org.fluminous.jq.filter.pattern

case class PatternCaseFailure(actualExpression: String, expectedExpression: String)

case class PatternFailure(patternName: String, firstMismatchPositionInStack: Int, failures: List[PatternCaseFailure])
