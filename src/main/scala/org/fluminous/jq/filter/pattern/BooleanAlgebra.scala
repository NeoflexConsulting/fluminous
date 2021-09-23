package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.boolean.Conjunction
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, testAndDropIf }
import org.fluminous.jq.tokens.Identifier
import shapeless.{ ::, HNil }

case object BooleanAlgebra extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Conjunction](
    (capture[Filter] ->: testAndDropIf[Identifier](_.value == "and") ->: capture[Filter]).ifValidReplaceBy {
      case left :: right :: HNil => Conjunction(_, List(left, right))
    }
  )
}
