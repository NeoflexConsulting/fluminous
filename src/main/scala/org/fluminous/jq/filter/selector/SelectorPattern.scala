package org.fluminous.jq.filter.selector

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.algebra.IntegerNumber
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.{ LeftSquareBracket, QuestionMark, RightSquareBracket, Root }
import org.fluminous.jq.tokens.{ Identifier, RawString, StringToken }
import org.fluminous.jq.filter.range.Range
import shapeless.{ ::, HNil }
import cats.syntax.option._
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.array.JsonArray

case object SelectorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[SelectorByName](
    (capture[StringToken] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByName(_, s.value)
    },
    (test[RightSquareBracket] ->: capture[StringToken] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByName(_, s.value)
    },
    (test[RightSquareBracket] ->: capture[IntegerNumber] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => SelectorByIndex(_, s.value)
    },
    (capture[JsonArray] ->: test[Root]).ifValidReplaceBy {
      case a :: HNil => SelectorByIndexArray(_, a.values)
    },
    (test[RightSquareBracket] ->: test[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy { _ =>
      Splitter(_)
    },
    (capture[Range] ->: test[Root]).ifValidReplaceBy {
      case r :: HNil => SelectorByRange(_, r)
    },
    (test[RightSquareBracket] ->: capture[StringToken] ->: test[LeftSquareBracket] ->: capture[SelectorByName]).ifValidReplaceBy {
      case s :: parent :: HNil => SelectorByName(_, s.value, parent.field.some)
    },
    (test[RightSquareBracket] ->: capture[IntegerNumber] ->: test[LeftSquareBracket] ->: capture[SelectorByName]).ifValidReplaceBy {
      case s :: parent :: HNil => SelectorByIndex(_, s.value, parent.field.some)
    },
    (capture[JsonArray] ->: capture[SelectorByName]).ifValidReplaceBy {
      case a :: parent :: HNil => SelectorByIndexArray(_, a.values, parent.field.some)
    },
    (test[RightSquareBracket] ->: test[LeftSquareBracket] ->: capture[SelectorByName]).ifValidReplaceBy {
      case parent :: HNil =>
        Splitter(_, parent.field.some)
    },
    (capture[Range] ->: capture[SelectorByName]).ifValidReplaceBy {
      case r :: parent :: HNil => SelectorByRange(_, r, parent.field.some)
    },
    (test[QuestionMark] ->: capture[Filter]).ifValidReplaceBy {
      case f :: HNil => SuppressErrorSelector(_, f)
    },
    (lookup[Expression]
      .notInstance[StringToken]
      .notInstance[LeftSquareBracket] ->: test[Root]).ifValidReplaceBy { _ =>
      IdentitySelector
    }
  )
}
