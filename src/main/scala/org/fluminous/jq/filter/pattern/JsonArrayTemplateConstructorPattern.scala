package org.fluminous.jq.filter.pattern

import com.fasterxml.jackson.annotation.JsonPropertyOrder
import io.circe.Json
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonArrayTemplateConstructor, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.{ Comma, DecimalNumber, IntegerNumber, LeftSquareBracket, RawString }

case object JsonArrayTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case Comma :: (s @ Selector(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Right(s))) :: rest
    case Comma :: RawString(value, _) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Left(Json.fromString(value)))) :: rest
    case Comma :: (value @ IntegerNumber(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Left(Json.fromInt(value.asInt)))) :: rest
    case Comma :: (value @ DecimalNumber(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case Comma :: (filter @ JsonObjectTemplate(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Right(filter))) :: rest

    case Comma :: (s @ Selector(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Right(s) +: seq) :: rest
    case Comma :: RawString(value, _) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Left(Json.fromString(value)) +: seq) :: rest
    case Comma :: (value @ IntegerNumber(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Left(Json.fromInt(value.asInt)) +: seq) :: rest
    case Comma :: (value @ DecimalNumber(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Left(Json.fromBigDecimal(value.asDecimal)) +: seq) :: rest
    case Comma :: (filter @ JsonObjectTemplate(_)) :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Right(filter) +: seq) :: rest

  }
}
