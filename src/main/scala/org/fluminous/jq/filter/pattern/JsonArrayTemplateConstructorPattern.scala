package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, testAndDrop}
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.JsonArrayUncompleted
import org.fluminous.jq.tokens.{Comma, LeftSquareBracket}
import shapeless.{::, HNil}

case object JsonArrayTemplateConstructorPattern extends ExpressionPattern {

  override val ExpressionCases: PatternCases = PatternCases[JsonArrayUncompleted](
    (testAndDrop[Comma] ->: capture[Filter] ->: testAndDrop[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayUncompleted(_, List(filter))
    },
    (testAndDrop[Comma] ->: capture[Filter] ->: capture[JsonArrayUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayUncompleted(_, s +: js.values)
    }
  )
}
