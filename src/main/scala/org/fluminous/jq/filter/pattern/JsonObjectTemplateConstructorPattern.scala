package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.filter.{
  JsonArrayTemplate,
  JsonObjectTemplate,
  JsonObjectTemplateConstructor,
  JsonTupleHeader,
  Selector
}
import org.fluminous.jq.tokens.{ Comma, DecimalNumber, Identifier, IntegerNumber, LeftFigureBracket, RawString, Root }

import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObjectTemplateConstructor](
    (check[Comma] ->: capture[Selector] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> s))
    },
    (check[Comma] ->: capture[Root] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case r :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> r))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> s))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case i :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> i))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case n :: jh :: HNil =>
        JsonObjectTemplateConstructor(_, Map(jh.fieldName -> n))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case filter :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> filter))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case filter :: jh :: HNil => JsonObjectTemplateConstructor(_, Map(jh.fieldName -> filter))
    },
    (check[Comma] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectTemplateConstructor(_, Map(id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[Comma] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectTemplateConstructor(_, Map(s.value -> Selector(s.position, Seq(s.value))))
    },
    (check[Comma] ->: capture[Selector] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> s))
    },
    (check[Comma] ->: capture[Root] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case r :: jh :: js :: HNil => JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> r))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: jh :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> s))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case i :: jh :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> i))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case n :: jh :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> n))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case filter :: jh :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> filter))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case filter :: jh :: js :: HNil =>
        JsonObjectTemplateConstructor(_, js.values + (jh.fieldName -> filter))
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
