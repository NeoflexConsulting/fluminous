package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.json.{JsonObject, JsonObjectUncompleted, JsonTupleHeader}
import org.fluminous.jq.filter.{Filter, Selector}
import org.fluminous.jq.tokens.{Identifier, LeftFigureBracket, RawString, RightFigureBracket}
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, check}
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObject](
    (check[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectUncompleted
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObject(_, js.values + (jh.fieldName -> s))
    },
    (check[RightFigureBracket] ->: capture[Identifier] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObject(_, js.values + (id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil => JsonObject(_, js.values + (s.value -> Selector(s.position, Seq(s.value))))
    },
    (check[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObject(_, Map(jh.fieldName -> s))
    },
    (check[RightFigureBracket] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObject(_, Map(id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObject(_, Map(s.value -> Selector(s.position, Seq(s.value))))
    }
  )
}
