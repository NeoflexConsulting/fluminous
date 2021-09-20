package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.filter.{
  JsonArrayTemplate,
  JsonObjectTemplate,
  JsonObjectTemplateConstructor,
  JsonTupleHeader,
  Selector
}
import org.fluminous.jq.tokens.{
  DecimalNumber,
  Identifier,
  IntegerNumber,
  LeftFigureBracket,
  RawString,
  RightFigureBracket,
  Root
}

import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObjectTemplate](
    (check[RightFigureBracket] ->: capture[Selector] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectTemplate(_, js.values + (jh.fieldName -> Right(s)))
    },
    (check[RightFigureBracket] ->: capture[Root] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case r :: jh :: js :: HNil => JsonObjectTemplate(_, js.values + (jh.fieldName -> Right(r)))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil =>
        JsonObjectTemplate(_, js.values + (jh.fieldName -> Left(Json.fromString(s.value))))
    },
    (check[RightFigureBracket] ->: capture[IntegerNumber] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case i :: jh :: js :: HNil =>
        JsonObjectTemplate(_, js.values + (jh.fieldName -> Left(Json.fromInt(i.asInt))))
    },
    (check[RightFigureBracket] ->: capture[DecimalNumber] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case n :: jh :: js :: HNil =>
        JsonObjectTemplate(_, js.values + (jh.fieldName -> Left(Json.fromBigDecimal(n.asDecimal))))
    },
    (check[RightFigureBracket] ->: capture[JsonArrayTemplate] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case filter :: jh :: js :: HNil => JsonObjectTemplate(_, js.values + (jh.fieldName -> Right(filter)))
    },
    (check[RightFigureBracket] ->: capture[JsonObjectTemplate] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case filter :: jh :: js :: HNil => JsonObjectTemplate(_, js.values + (jh.fieldName -> Right(filter)))
    },
    (check[RightFigureBracket] ->: capture[Identifier] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObjectTemplate(_, js.values + (id.value -> Right(Selector(id.position, Seq(id.value)))))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonObjectTemplate(_, js.values + (s.value -> Right(Selector(s.position, Seq(s.value)))))
    },
    (check[RightFigureBracket] ->: capture[Selector] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Right(s)))
    },
    (check[RightFigureBracket] ->: capture[Root] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case r :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Right(r)))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Left(Json.fromString(s.value))))
    },
    (check[RightFigureBracket] ->: capture[IntegerNumber] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case i :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Left(Json.fromInt(i.asInt))))
    },
    (check[RightFigureBracket] ->: capture[DecimalNumber] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case n :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Left(Json.fromBigDecimal(n.asDecimal))))
    },
    (check[RightFigureBracket] ->: capture[JsonArrayTemplate] ->: capture[JsonTupleHeader] ->: check[
      LeftFigureBracket
    ]).ifValidReplaceBy {
      case filter :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Right(filter)))
    },
    (check[RightFigureBracket] ->: capture[JsonObjectTemplate] ->: capture[JsonTupleHeader] ->: check[
      LeftFigureBracket
    ]).ifValidReplaceBy {
      case filter :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> Right(filter)))
    },
    (check[RightFigureBracket] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectTemplate(_, Map(id.value -> Right(Selector(id.position, Seq(id.value)))))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectTemplate(_, Map(s.value -> Right(Selector(s.position, Seq(s.value)))))
    }
  )
}
