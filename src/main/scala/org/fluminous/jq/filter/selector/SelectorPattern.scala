package org.fluminous.jq.filter.selector

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.{ LeftSquareBracket, RightSquareBracket, Root }
import org.fluminous.jq.tokens.{ Identifier, NaturalNumber, RawString }
import org.fluminous.jq.filter.range.Range
import shapeless.{ ::, HNil }

case object SelectorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[SelectorByName](
    (capture[Identifier] ->: test[Root]).ifValidReplaceBy {
      case id :: HNil => SelectorByName(_, id.value)
    },
    (capture[RawString] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByName(_, s.value)
    },
    (test[RightSquareBracket] ->: capture[Identifier] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByName(_, s.value)
    },
    (test[RightSquareBracket] ->: capture[RawString] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByName(_, s.value)
    },
    (test[RightSquareBracket] ->: capture[NaturalNumber] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByIndex(_, s.intValue)
    },
    (test[RightSquareBracket] ->: capture[Range] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case r :: HNil => SelectorByRange(_, r)
    },
    (lookup[Expression]
      .notInstance[Identifier]
      .notInstance[RawString]
      .notInstance[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy { _ =>
      IdentitySelector
    }
  )
}
