package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonObjectTemplateConstructor, JsonTupleHeader, Selector }
import org.fluminous.jq.tokens.{ Comma, DecimalNumber, Identifier, IntegerNumber, LeftFigureBracket, RawString, Root }

case object JsonObjectTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case Comma :: (s @ Selector(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> Right(s))) :: rest
    case Comma :: Root :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> Right(Root))) :: rest
    case Comma :: RawString(value, _) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> Left(Json.fromString(value)))) :: rest
    case Comma :: (value @ IntegerNumber(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> Left(Json.fromInt(value.asInt)))) :: rest
    case Comma :: (value @ DecimalNumber(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case Comma :: (filter @ JsonArrayTemplate(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> Right(filter))) :: rest
    case Comma :: Identifier(value) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(value -> Right(Selector(Seq(value))))) :: rest
    case Comma :: RawString(value, _) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(value -> Right(Selector(Seq(value))))) :: rest

    case Comma :: (s @ Selector(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> Right(s))) :: rest
    case Comma :: Root :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> Right(Root))) :: rest
    case Comma :: RawString(value, _) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> Left(Json.fromString(value)))) :: rest
    case Comma :: (value @ IntegerNumber(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> Left(Json.fromInt(value.asInt)))) :: rest
    case Comma :: (value @ DecimalNumber(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case Comma :: (filter @ JsonArrayTemplate(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> Right(filter))) :: rest
    case Comma :: Identifier(value) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (value -> Right(Selector(Seq(value))))) :: rest
    case Comma :: RawString(value, _) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (value -> Right(Selector(Seq(value))))) :: rest
  }
}
