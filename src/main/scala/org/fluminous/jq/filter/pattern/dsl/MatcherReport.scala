package org.fluminous.jq.filter.pattern.dsl

import org.fluminous.jq.Expression

trait MatcherReport {
  def matchError(actual: List[Expression]): String
}
