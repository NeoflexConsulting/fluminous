package org.fluminous.jq.filter.json.tuple

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.{ RawString, StringToken }
import org.fluminous.jq.tokens.symbolic.{ Colon, Comma, LeftFigureBracket }
import shapeless.{ ::, HNil }

case object JsonTupleHeaderPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonTupleHeader](
    (test[Colon] ->: capture[Filter] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case f :: HNil => JsonTupleHeader(_, f)
    },
    (test[Colon] ->: capture[Filter] ->: test[Comma]).ifValidReplaceBy {
      case f :: HNil => JsonTupleHeader(_, f)
    },
    (test[Colon] ->: capture[StringToken] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonTupleHeader(_, RawString(s.position, s.value))
    },
    (test[Colon] ->: capture[StringToken] ->: test[Comma]).ifValidReplaceBy {
      case s :: HNil => JsonTupleHeader(_, RawString(s.position, s.value))
    }
  )
}
