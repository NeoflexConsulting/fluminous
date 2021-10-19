package org.fluminous.jq.filter.json.obj

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.tuple.JsonTupleHeader
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, lookup, test}
import org.fluminous.jq.filter.pattern.{ExpressionPattern, PatternCases}
import org.fluminous.jq.filter.selector.SelectorByName
import org.fluminous.jq.tokens.symbolic.{Comma, LeftFigureBracket, RightFigureBracket}
import org.fluminous.jq.tokens.{RawString, StringToken}
import shapeless.{::, HNil}

case object JsonObjectPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObject](
    (lookup[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectStart(_, Map(jh.fieldName -> s))
    },
    (lookup[Comma] ->: capture[StringToken] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectStart(_, Map(RawString(s.position,s.value) -> SelectorByName(s.position, s.value)))
    },
    (lookup[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectStart(_, js.values + (jh.fieldName -> s))
    },
    (lookup[Comma] ->: capture[StringToken] ->: test[Comma] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case s :: js :: HNil =>
        JsonObjectStart(_, js.values + (RawString(s.position,s.value) -> SelectorByName(s.position, s.value)))
    },
    (test[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectStart
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObject(_, js.values + (jh.fieldName -> s))
    },
    (test[RightFigureBracket] ->: capture[StringToken] ->: test[Comma] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case s :: js :: HNil =>
        JsonObject(_, js.values + (RawString(s.position,s.value) -> SelectorByName(s.position, s.value)))
    },
    (test[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader]).ifValidReplaceBy {
      case f :: jh :: HNil => JsonObject(_, Map(jh.fieldName -> f))
    },
    (test[RightFigureBracket] ->: capture[StringToken] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObject(_, Map(RawString(s.position,s.value) -> SelectorByName(s.position, s.value)))
    }
  )
}
