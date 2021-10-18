package org.fluminous.jq.filter.json.tuple

import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.Colon
import org.fluminous.jq.tokens.StringToken
import shapeless.{ ::, HNil }

case object JsonTupleHeaderPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonTupleHeader](
    (test[Colon] ->: capture[StringToken]).ifValidReplaceBy {
      case s :: HNil => JsonTupleHeader(_, s.value)
    }
  )
}
