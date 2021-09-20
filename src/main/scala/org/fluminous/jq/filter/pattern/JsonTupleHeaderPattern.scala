package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.JsonTupleHeader
import org.fluminous.jq.tokens.{ Colon, Identifier, RawString }

import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonTupleHeaderPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonTupleHeader](
    (check[Colon] ->: capture[Identifier]).ifValidReplaceBy {
      case id :: HNil => JsonTupleHeader(_, id.value)
    },
    (check[Colon] ->: capture[RawString]).ifValidReplaceBy {
      case s :: HNil => JsonTupleHeader(_, s.value)
    }
  )
}
