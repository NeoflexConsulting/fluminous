package org.fluminous.jq.filter.json.obj

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.tuple.JsonTupleHeader
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, test}
import org.fluminous.jq.filter.pattern.{ExpressionPattern, PatternCases}
import org.fluminous.jq.filter.selector.SelectorByName
import org.fluminous.jq.tokens.symbolic.{Comma, LeftFigureBracket, RightFigureBracket}
import org.fluminous.jq.tokens.{Identifier, RawString}
import shapeless.{::, HNil}

case object JsonObjectPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObject](
    (test[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectStart(_, Map(jh.fieldName -> s))
    },
    (test[Comma] ->: capture[Identifier] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectStart(_, Map(id.value -> SelectorByName(id.position, id.value)))
    },
    (test[Comma] ->: capture[RawString] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectStart(_, Map(s.value -> SelectorByName(s.position, s.value)))
    },
    (test[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectStart(_, js.values + (jh.fieldName -> s))
    },
    (test[Comma] ->: capture[Identifier] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObjectStart(_, js.values + (id.value -> SelectorByName(id.position, id.value)))
    },
    (test[Comma] ->: capture[RawString] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case s :: js :: HNil =>
        JsonObjectStart(_, js.values + (s.value -> SelectorByName(s.position, s.value)))
    },
    (test[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectStart
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObject(_, js.values + (jh.fieldName -> s))
    },
    (test[RightFigureBracket] ->: capture[Identifier] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObject(_, js.values + (id.value -> SelectorByName(id.position, id.value)))
    },
    (test[RightFigureBracket] ->: capture[RawString] ->: capture[JsonObjectStart]).ifValidReplaceBy {
      case s :: js :: HNil => JsonObject(_, js.values + (s.value -> SelectorByName(s.position, s.value)))
    },
    (test[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: test[
      LeftFigureBracket
    ]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObject(_, Map(jh.fieldName -> s))
    },
    (test[RightFigureBracket] ->: capture[Identifier] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObject(_, Map(id.value -> SelectorByName(id.position, id.value)))
    },
    (test[RightFigureBracket] ->: capture[RawString] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObject(_, Map(s.value -> SelectorByName(s.position, s.value)))
    }
  )
}
