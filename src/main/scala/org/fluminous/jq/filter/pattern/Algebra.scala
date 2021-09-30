package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.tokens.{ LeftBracket, RightBracket }
import shapeless.{ ::, HNil }

case object Algebra extends ExpressionPattern {
  override protected val ExpressionCases: PatternCases = PatternCases[Filter](
    (test[RightBracket] ->: capture[Filter] ->: test[LeftBracket]).ifValidReplaceBy {
      case filter :: HNil => _ => filter
    }
  )
}
