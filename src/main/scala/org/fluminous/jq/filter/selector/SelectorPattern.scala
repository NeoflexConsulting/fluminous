package org.fluminous.jq.filter.selector

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.{ Identifier, RawString, Root }
import shapeless.{ ::, HNil }

case object SelectorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Selector](
    (capture[Identifier] ->: test[Root]).ifValidReplaceBy {
      case id :: HNil => Selector(_, id.value)
    },
    (capture[RawString] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => Selector(_, s.value)
    },
    (lookup[Expression].notInstance[Identifier].notInstance[RawString] ->: test[Root]).ifValidReplaceBy { HNil =>
      IdentitySelector
    }
  )
}
