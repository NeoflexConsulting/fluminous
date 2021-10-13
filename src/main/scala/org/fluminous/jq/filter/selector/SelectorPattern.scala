package org.fluminous.jq.filter.selector

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.{ LeftSquareBracket, Root }
import org.fluminous.jq.tokens.{ Identifier, RawString }
import shapeless.{ ::, HNil }

case object SelectorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[SelectorByName](
    (capture[Identifier] ->: test[Root]).ifValidReplaceBy {
      case id :: HNil => SelectorByName(_, id.value)
    },
    (capture[RawString] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByName(_, s.value)
    },
    (lookup[Expression]
      .notInstance[Identifier]
      .notInstance[RawString]
      .notInstance[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy { HNil =>
      IdentitySelector
    }
  )
}
