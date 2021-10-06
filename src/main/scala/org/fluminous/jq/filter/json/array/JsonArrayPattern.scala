package org.fluminous.jq.filter.json.array

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.{ Comma, LeftSquareBracket, RightSquareBracket }
import shapeless.{ ::, HNil }

case object JsonArrayPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonArray](
    (test[Comma] ->: capture[Filter] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayStart(_, List(filter))
    },
    (test[Comma] ->: capture[Filter] ->: capture[JsonArrayStart]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayStart(_, s +: js.values)
    },
    (test[RightSquareBracket] ->: capture[Filter] ->: capture[JsonArrayStart]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArray(_, (s +: js.values).reverse)
    },
    (test[RightSquareBracket] ->: capture[Filter] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArray(_, List(s))
    }
  )
}
