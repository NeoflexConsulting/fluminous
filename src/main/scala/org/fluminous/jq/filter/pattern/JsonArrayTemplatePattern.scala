package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.{JsonArray, JsonArrayUncompleted}
import org.fluminous.jq.tokens.{LeftSquareBracket, RightSquareBracket}
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, testAndDrop}
import shapeless.HNil
import shapeless.::

case object JsonArrayTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonArray](
    (testAndDrop[RightSquareBracket] ->: capture[Filter] ->: capture[JsonArrayUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArray(_, (s +: js.values).reverse)
    },
    (testAndDrop[RightSquareBracket] ->: capture[Filter] ->: testAndDrop[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArray(_, List(s))
    }
  )
}
