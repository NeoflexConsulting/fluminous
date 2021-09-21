package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.boolean.Conjunction
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, check}
import org.fluminous.jq.tokens.RightSquareBracket
import shapeless.{::, HNil}

case object BooleanAlgebra extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Conjunction]()
}
