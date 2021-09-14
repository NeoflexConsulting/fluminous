package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression

case class PatternCase private (input: List[Expression], output: List[Expression])

object PatternCase {
  def apply(matcher: (List[Expression], List[Expression])) = {
    PatternCase(matcher._1, matcher._2)
  }
}
