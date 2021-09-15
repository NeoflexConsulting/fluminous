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

import org.fluminous.jq.filter.pattern.dsl.MatcherExpression.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: List[PatternCase] = List(
    (check[Comma] ->: capture[Selector] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Right(s))))
    },
    (check[Comma] ->: check[Root] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case jh :: HNil => List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Right(Root))))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Left(Json.fromString(s.value)))))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case i :: jh :: HNil => List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Left(Json.fromInt(i.asInt)))))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case n :: jh :: HNil =>
        List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Left(Json.fromBigDecimal(n.asDecimal)))))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case filter :: jh :: HNil => List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Right(filter))))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case filter :: jh :: HNil => List(JsonObjectTemplateConstructor(Map(jh.fieldName -> Right(filter))))
    },
    (check[Comma] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => List(JsonObjectTemplateConstructor(Map(id.value -> Right(Selector(Seq(id.value))))))
    },
    (check[Comma] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => List(JsonObjectTemplateConstructor(Map(s.value -> Right(Selector(Seq(s.value))))))
    },
    (check[Comma] ->: capture[Selector] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Right(s))))
    },
    (check[Comma] ->: check[Root] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case jh :: js :: HNil => List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Right(Root))))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: jh :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Left(Json.fromString(s.value)))))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case i :: jh :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Left(Json.fromInt(i.asInt)))))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: capture[JsonTupleHeader] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case n :: jh :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Left(Json.fromBigDecimal(n.asDecimal)))))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case filter :: jh :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Right(filter))))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case filter :: jh :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (jh.fieldName -> Right(filter))))
    },
    (check[Comma] ->: capture[Identifier] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case id :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (id.value -> Right(Selector(Seq(id.value))))))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil =>
        List(JsonObjectTemplateConstructor(js.values + (s.value -> Right(Selector(Seq(s.value))))))
    }
  )
}
