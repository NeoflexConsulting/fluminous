package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.Expression
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

case object JsonObjectTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case RightFigureBracket :: (s @ Selector(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (name -> Right(s))) :: rest
    case RightFigureBracket :: Root :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (name -> Right(Root))) :: rest
    case RightFigureBracket :: RawString(value, _) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (name -> Left(Json.fromString(value)))) :: rest
    case RightFigureBracket :: (value @ IntegerNumber(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(
          values
        ) :: rest =>
      JsonObjectTemplate(values + (name -> Left(Json.fromInt(value.asInt)))) :: rest
    case RightFigureBracket :: (value @ DecimalNumber(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(
          values
        ) :: rest =>
      JsonObjectTemplate(values + (name -> Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case RightFigureBracket :: (filter @ JsonArrayTemplate(_)) :: JsonTupleHeader(name) :: JsonObjectTemplateConstructor(
          values
        ) :: rest =>
      JsonObjectTemplate(values + (name -> Right(filter))) :: rest
    case RightFigureBracket :: Identifier(value) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (value -> Right(Selector(Seq(value))))) :: rest
    case RightFigureBracket :: RawString(value, _) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (value -> Right(Selector(Seq(value))))) :: rest

    case RightFigureBracket :: (s @ Selector(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> Right(s))) :: rest
    case RightFigureBracket :: Root :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> Right(Root))) :: rest
    case RightFigureBracket :: RawString(value, _) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> Left(Json.fromString(value)))) :: rest
    case RightFigureBracket :: (value @ IntegerNumber(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> Left(Json.fromInt(value.asInt)))) :: rest
    case RightFigureBracket :: (value @ DecimalNumber(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case RightFigureBracket :: (filter @ JsonArrayTemplate(_)) :: JsonTupleHeader(name) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> Right(filter))) :: rest
    case RightFigureBracket :: Identifier(value) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(value -> Right(Selector(Seq(value))))) :: rest
    case RightFigureBracket :: RawString(value, _) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(value -> Right(Selector(Seq(value))))) :: rest

  }
}
