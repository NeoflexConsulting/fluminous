package org.fluminous.jq.filter.selector

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.algebra.IntegerNumber
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.{ LeftSquareBracket, QuestionMark, RightSquareBracket, Root }
import org.fluminous.jq.tokens.{ Identifier, RawString }
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
    (test[RightSquareBracket] ->: capture[IntegerNumber] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByIndex(_, s.value)
    },
    (test[RightSquareBracket] ->: capture[Range] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case r :: HNil => SelectorByRange(_, r)
    },
    (test[QuestionMark] ->: capture[SelectorByName]).ifValidReplaceBy {
      case r :: HNil => SuppressErrorSelector(_, r)
    },
    (test[QuestionMark] ->: capture[SelectorByIndex]).ifValidReplaceBy {
      case r :: HNil => SuppressErrorSelector(_, r)
    },
    (test[QuestionMark] ->: capture[SelectorByRange]).ifValidReplaceBy {
      case r :: HNil => SuppressErrorSelector(_, r)
    },
    (lookup[Expression]
      .notInstance[Identifier]
      .notInstance[RawString]
      .notInstance[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy { _ =>
      IdentitySelector
    }
  )
}
