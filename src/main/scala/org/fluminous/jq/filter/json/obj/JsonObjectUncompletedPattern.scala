package org.fluminous.jq.filter.json.obj

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.tuple.JsonTupleHeader
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, test}
import org.fluminous.jq.filter.pattern.{ExpressionPattern, PatternCases}
import org.fluminous.jq.filter.selector.Selector
import org.fluminous.jq.tokens.{Comma, Identifier, LeftFigureBracket, RawString}
import shapeless.{::, HNil}

case object JsonObjectUncompletedPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObjectUncompleted](
    (test[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectUncompleted(_, Map(jh.fieldName -> s))
    },
    (test[Comma] ->: capture[Identifier] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectUncompleted(_, Map(id.value -> Selector(id.position, id.value)))
    },
    (test[Comma] ->: capture[RawString] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectUncompleted(_, Map(s.value -> Selector(s.position, s.value)))
    },
    (test[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectUncompleted(_, js.values + (jh.fieldName -> s))
    },
    (test[Comma] ->: capture[Identifier] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObjectUncompleted(_, js.values + (id.value -> Selector(id.position, id.value)))
    },
    (test[Comma] ->: capture[RawString] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil =>
        JsonObjectUncompleted(_, js.values + (s.value -> Selector(s.position, s.value)))
    }
  )
}
