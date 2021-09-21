package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import org.fluminous.jq.filter.{ Filter, JsonArrayTemplateConstructor }
import org.fluminous.jq.tokens.{ Comma, LeftSquareBracket }
import shapeless.{ ::, HNil }

case object JsonArrayTemplateConstructorPattern extends ExpressionPattern {

  override val ExpressionCases: PatternCases = PatternCases[JsonArrayTemplateConstructor](
    (check[Comma] ->: capture[Filter] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplateConstructor(_, Seq(filter))
    },
    (check[Comma] ->: capture[Filter] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplateConstructor(_, s +: js.values)
    }
  )
}
