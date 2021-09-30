package org.fluminous.jq.filter.json.obj

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.json.tuple.JsonTupleHeader
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, test}
import org.fluminous.jq.filter.pattern.{ExpressionPattern, PatternCases}
import org.fluminous.jq.filter.selector.Selector
import org.fluminous.jq.tokens.{Identifier, LeftFigureBracket, RawString, RightFigureBracket}
import shapeless.{::, HNil}


case object JsonObjectPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObject](
    (test[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectUncompleted
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObject(_, js.values + (jh.fieldName -> s))
    },
    (test[RightFigureBracket] ->: capture[Identifier] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObject(_, js.values + (id.value -> Selector(id.position, id.value)))
    },
    (test[RightFigureBracket] ->: capture[RawString] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil => JsonObject(_, js.values + (s.value -> Selector(s.position, s.value)))
    },
    (test[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: test[
      LeftFigureBracket
    ]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObject(_, Map(jh.fieldName -> s))
    },
    (test[RightFigureBracket] ->: capture[Identifier] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObject(_, Map(id.value -> Selector(id.position, id.value)))
    },
    (test[RightFigureBracket] ->: capture[RawString] ->: test[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObject(_, Map(s.value -> Selector(s.position, s.value)))
    }
  )
}
