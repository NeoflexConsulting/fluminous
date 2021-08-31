package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonTemplateTuple, Selector }
import org.fluminous.jq.tokens.{ Colon, DecimalNumber, Identifier, IntegerNumber, RawString }

case object JsonTupleTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case (s @ Selector(_)) :: Colon :: Identifier(name) :: rest =>
      JsonTemplateTuple(name, Right(s)) :: rest
    case RawString(value, _) :: Colon :: Identifier(name) :: rest =>
      JsonTemplateTuple(name, Left(Json.fromString(value))) :: rest
    case (value @ IntegerNumber(_)) :: Colon :: Identifier(name) :: rest =>
      JsonTemplateTuple(name, Left(Json.fromInt(value.asInt))) :: rest
    case (value @ DecimalNumber(_)) :: Colon :: Identifier(name) :: rest =>
      JsonTemplateTuple(name, Left(Json.fromBigDecimal(value.asDecimal))) :: rest
    case (filter @ JsonArrayTemplate(_)) :: Colon :: Identifier(name) :: rest =>
      JsonTemplateTuple(name, Right(filter)) :: rest

    case (s @ Selector(_)) :: Colon :: RawString(name, _) :: rest =>
      JsonTemplateTuple(name, Right(s)) :: rest
    case RawString(value, _) :: Colon :: RawString(name, _) :: rest =>
      JsonTemplateTuple(name, Left(Json.fromString(value))) :: rest
    case (value @ IntegerNumber(_)) :: Colon :: RawString(name, _) :: rest =>
      JsonTemplateTuple(name, Left(Json.fromInt(value.asInt))) :: rest
    case (value @ DecimalNumber(_)) :: Colon :: RawString(name, _) :: rest =>
      JsonTemplateTuple(name, Left(Json.fromBigDecimal(value.asDecimal))) :: rest
    case (filter @ JsonArrayTemplate(_)) :: Colon :: RawString(name, _) :: rest =>
      JsonTemplateTuple(name, Right(filter)) :: rest
  }
}
