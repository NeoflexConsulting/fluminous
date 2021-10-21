package org.fluminous.jq.filter.json.array

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.{ LeftSquareBracket, RightSquareBracket }
import shapeless.{ ::, HNil }

case object JsonArrayPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonArray](
    (test[RightSquareBracket] ->: capture[Filter] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case f :: HNil => JsonArray(_, f)
    }
  )
}
