package org.fluminous.jq.filter.pattern

case class PatternCaseFailure(actualExpression: String, expectedExpression: String)

case class PatternFailure(patternName: String, position: Int, failures: List[PatternCaseFailure])
