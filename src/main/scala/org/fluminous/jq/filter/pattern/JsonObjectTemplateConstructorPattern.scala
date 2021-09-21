package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.json.{JsonObjectUncompleted, JsonTupleHeader}
import org.fluminous.jq.filter.{Filter, Selector}
import org.fluminous.jq.tokens.{Comma, Identifier, LeftFigureBracket, RawString}
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, check}
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObjectUncompleted](
    (check[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectUncompleted(_, Map(jh.fieldName -> s))
    },
    (check[Comma] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectUncompleted(_, Map(id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[Comma] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectUncompleted(_, Map(s.value -> Selector(s.position, Seq(s.value))))
    },
    (check[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectUncompleted(_, js.values + (jh.fieldName -> s))
    },
    (check[Comma] ->: capture[Identifier] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObjectUncompleted(_, js.values + (id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil =>
        JsonObjectUncompleted(_, js.values + (s.value -> Selector(s.position, Seq(s.value))))
    }
  )
}
