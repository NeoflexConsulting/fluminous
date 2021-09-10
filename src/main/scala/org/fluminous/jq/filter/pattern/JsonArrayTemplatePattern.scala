package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonArrayTemplateConstructor, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.{ DecimalNumber, IntegerNumber, LeftSquareBracket, RawString, RightSquareBracket, Root }

case object JsonArrayTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case RightSquareBracket :: (s @ Selector(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Right(s) +: seq).reverse) :: rest
    case RightSquareBracket :: Root :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Right(Root) +: seq).reverse) :: rest
    case RightSquareBracket :: RawString(value, _) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Left(Json.fromString(value)) +: seq).reverse) :: rest
    case RightSquareBracket :: (value @ IntegerNumber(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Left(Json.fromInt(value.asInt)) +: seq).reverse) :: rest
    case RightSquareBracket :: (value @ DecimalNumber(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Left(Json.fromBigDecimal(value.asDecimal)) +: seq).reverse) :: rest
    case RightSquareBracket :: (filter @ JsonObjectTemplate(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Right(filter) +: seq).reverse) :: rest
    case RightSquareBracket :: (filter @ JsonArrayTemplate(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate((Right(filter) +: seq).reverse) :: rest

    case RightSquareBracket :: (s @ Selector(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Right(s))) :: rest
    case RightSquareBracket :: Root :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Right(Root))) :: rest
    case RightSquareBracket :: RawString(value, _) :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Left(Json.fromString(value)))) :: rest
    case RightSquareBracket :: (value @ IntegerNumber(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Left(Json.fromInt(value.asInt)))) :: rest
    case RightSquareBracket :: (value @ DecimalNumber(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case RightSquareBracket :: (filter @ JsonObjectTemplate(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Right(filter))) :: rest
    case RightSquareBracket :: (filter @ JsonArrayTemplate(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplate(Seq(Right(filter))) :: rest
  }
}
