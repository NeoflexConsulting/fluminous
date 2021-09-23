package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, testAndDrop }
import org.fluminous.jq.tokens.{ LeftBracket, RightBracket }
import shapeless.{ ::, HNil }

case object Algebra extends ExpressionPattern {
  override protected val ExpressionCases: PatternCases = PatternCases[Filter](
    (testAndDrop[RightBracket] ->: capture[Filter] ->: testAndDrop[LeftBracket]).ifValidReplaceBy {
      case filter :: HNil => _ => filter
    }
  )
}
