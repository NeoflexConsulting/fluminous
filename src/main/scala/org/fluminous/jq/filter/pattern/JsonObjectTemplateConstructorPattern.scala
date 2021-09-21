package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.{
  Filter,
  JsonObjectTemplateConstructor,
  JsonTupleHeader,
  Selector
}
import org.fluminous.jq.tokens.{ Comma, Identifier, LeftFigureBracket, RawString }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObjectTemplateConstructor](
    (check[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> s))
    },
    (check[Comma] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectTemplateConstructor(_, Map(id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[Comma] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectTemplateConstructor(_, Map(s.value -> Selector(s.position, Seq(s.value))))
    },
    (check[Comma] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> s))
    },
    (check[Comma] ->: capture[Identifier] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (s.value -> Selector(s.position, Seq(s.value))))
    }
  )
}
