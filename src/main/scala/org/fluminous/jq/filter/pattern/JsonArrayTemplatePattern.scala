package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.{ Filter, JsonArrayTemplate, JsonArrayTemplateConstructor}
import org.fluminous.jq.tokens.{ LeftSquareBracket, RightSquareBracket }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonArrayTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonArrayTemplate](
    (check[RightSquareBracket] ->: capture[Filter] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplate(_, (s +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[Filter] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplate(_, Seq(s))
    }
  )
}
