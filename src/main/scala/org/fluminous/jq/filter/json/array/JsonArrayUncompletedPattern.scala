package org.fluminous.jq.filter.json.array

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.{ Comma, LeftSquareBracket }
import shapeless.{ ::, HNil }

case object JsonArrayUncompletedPattern extends ExpressionPattern {

  override val ExpressionCases: PatternCases = PatternCases[JsonArrayUncompleted](
    (test[Comma] ->: capture[Filter] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayUncompleted(_, List(filter))
    },
    (test[Comma] ->: capture[Filter] ->: capture[JsonArrayUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayUncompleted(_, s +: js.values)
    }
  )
}
